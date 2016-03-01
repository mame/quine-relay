/*
 *  Lazy K interpreter
 *
 *  Copyright 2008 irori <irorin@gmail.com>
 *  This is free software. You may modify and/or distribute it under the
 *  terms of the GNU General Public License, version 2 or any later version.
 *  It comes with no warranty.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <time.h>
#include <assert.h>
#include <stdint.h>

#define INITIAL_HEAP_SIZE 128*1024
#define RDSTACK_SIZE	100000

/**********************************************************************
 *  Storage management
 **********************************************************************/

/* TAG STRUCTURE
 *
 *  -------- -------- -------- ------00   Pair
 *  -------- -------- -------- ------01   Int
 *  -------- -------- -------- ------10   Combinator
 *  -------- -------- -------- -----011   Character
 *  -------- -------- -------- -----111   Miscellaneous
 */

struct tagPair;
typedef struct tagPair *Cell;
#define CELL(x)	((Cell)(x))
#define TAG(c)	((intptr_t)(c) & 0x03)

/* pair */
typedef struct tagPair {
    Cell car;
    Cell cdr;
} Pair;
#define ispair(c)	(TAG(c) == 0)
#define car(c)		((c)->car)
#define cdr(c)		((c)->cdr)
#define SET(c,fst,snd)  ((c)->car = (fst), (c)->cdr = (snd))

/* integer */
#define isint(c)	(TAG(c) == 1)
#define mkint(n)	CELL(((n) << 2) + 1)
#define intof(c)	((intptr_t)(c) >> 2)

/* combinator */
#define iscomb(c)	(TAG(c) == 2)
#define mkcomb(n)	CELL(((n) << 2) + 2)
#define combof(c)	((intptr_t)(c) >> 2)
#define COMB_S		mkcomb(0)
#define COMB_K		mkcomb(1)
#define COMB_I		mkcomb(2)
#define COMB_IOTA	mkcomb(3)
#define COMB_KI		mkcomb(4)
#define COMB_READ	mkcomb(5)
#define COMB_WRITE	mkcomb(6)
#define COMB_INC	mkcomb(7)
#define COMB_CONS	mkcomb(8)

/* character */
#define ischar(c)	(((intptr_t)(c) & 0x07) == 0x03)
#define mkchar(n)	CELL(((n) << 3) + 0x03)
#define charof(c)	((intptr_t)(c) >> 3)

/* immediate objects */
#define isimm(c)	(((intptr_t)(c) & 0x07) == 0x07)
#define mkimm(n)	CELL(((n) << 3) + 0x07)
#define NIL		mkimm(0)
#define COPIED		mkimm(1)
#define UNUSED_MARKER	mkimm(2)

Pair *heap_area, *free_ptr;
int heap_size, next_heap_size;

int gc_notify = 0;
double total_gc_time = 0.0;

void gc_run(Cell *save1, Cell *save2);
void rs_copy(void);
Cell copy_cell(Cell c);

void errexit(char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vfprintf(stderr, fmt, arg);
    va_end(arg);

    exit(1);
}

void storage_init(int size)
{
    heap_size = size;
    heap_area = malloc(sizeof(Pair) * heap_size);
    if (heap_area == NULL)
	errexit("Cannot allocate heap storage (%d cells)\n", heap_size);
    assert(((intptr_t)heap_area & 3) == 0 && (sizeof(Pair) & 3) == 0);
    
    free_ptr = heap_area;
    heap_area += heap_size;
    next_heap_size = heap_size * 3 / 2;
}

Cell pair(Cell fst, Cell snd)
{
    Cell c;
    if (free_ptr >= heap_area)
	gc_run(&fst, &snd);

    assert(free_ptr < heap_area);
    c = free_ptr++;
    car(c) = fst;
    cdr(c) = snd;
    return c;
}

Cell alloc(int n)
{
    Cell p;
    if (free_ptr + n > heap_area)
	gc_run(NULL, NULL);

    assert(free_ptr + n <= heap_area);
    p = free_ptr;
    free_ptr += n;
    return p;
}


void gc_run(Cell *save1, Cell *save2)
{
    static Pair* free_area = NULL;
    int num_alive;
    Pair *scan;
    clock_t start = clock();

    if (free_area == NULL) {
	free_area = malloc(sizeof(Pair) * next_heap_size);
	if (free_area == NULL)
	    errexit("Cannot allocate heap storage (%d cells)\n",
		    next_heap_size);
    }

    free_ptr = scan = free_area;
    free_area = heap_area - heap_size;
    heap_area = free_ptr + next_heap_size;

    rs_copy();
    if (save1)
	*save1 = copy_cell(*save1);
    if (save2)
	*save2 = copy_cell(*save2);

    while (scan < free_ptr) {
	car(scan) = copy_cell(car(scan));
	cdr(scan) = copy_cell(cdr(scan));
	scan++;
    }

    num_alive = free_ptr - (heap_area - next_heap_size);
    if (gc_notify)
	fprintf(stderr, "GC: %d / %d\n", num_alive, heap_size);

    if (heap_size != next_heap_size || num_alive * 8 > next_heap_size) {
	heap_size = next_heap_size;
	if (num_alive * 8 > next_heap_size)
	    next_heap_size = num_alive * 8;

	free(free_area);
	free_area = NULL;
    }

    total_gc_time += (clock() - start) / (double)CLOCKS_PER_SEC;
}

Cell copy_cell(Cell c)
{
    Cell r;

    if (!ispair(c))
	return c;
    if (car(c) == COPIED)
	return cdr(c);

    r = free_ptr++;
    car(r) = car(c);
    if (car(c) == COMB_I) {
	Cell tmp = cdr(c);
	while (ispair(tmp) && car(tmp) == COMB_I)
	    tmp = cdr(tmp);
	cdr(r) = tmp;
    }
    else
	cdr(r) = cdr(c);
    car(c) = COPIED;
    cdr(c) = r;
    return r;
}

/**********************************************************************
 *  Reduction Machine
 **********************************************************************/

typedef struct {
    Cell *sp;
    Cell stack[RDSTACK_SIZE];
} RdStack;

RdStack rd_stack;

void rs_init(void)
{
    int i;
    rd_stack.sp = rd_stack.stack + RDSTACK_SIZE;

    for (i = 0; i < RDSTACK_SIZE; i++)
	rd_stack.stack[i] = UNUSED_MARKER;
}

void rs_copy(void)
{
    Cell *c;
    for (c = rd_stack.stack + RDSTACK_SIZE - 1; c >= rd_stack.sp; c--)
	*c = copy_cell(*c);
}

int rs_max_depth(void)
{
    int i;
    for (i = 0; i < RDSTACK_SIZE; i++) {
	if (rd_stack.stack[i] != UNUSED_MARKER)
	    break;
    }
    return RDSTACK_SIZE - i;
}

void rs_push(Cell c)
{
    if (rd_stack.sp <= rd_stack.stack)
	errexit("runtime error: stack overflow\n");
    *--rd_stack.sp = c;
}

#define TOP		(*rd_stack.sp)
#define POP		(*rd_stack.sp++)
#define PUSH(c)		rs_push(c)
#define PUSHED(n)	(*(rd_stack.sp+(n)))
#define DROP(n)		(rd_stack.sp += (n))
#define ARG(n)		cdr(PUSHED(n))
#define APPLICABLE(n)	(bottom - rd_stack.sp > (n))

/**********************************************************************
 *  Loader
 **********************************************************************/

Cell read_one(FILE *fp, int i_is_iota);
Cell read_many(FILE *fp, int closing_char);

Cell load_program(const char *fname)
{
    FILE *fp;
    Cell c;
    
    if (fname == NULL)
	fp = stdin;
    else {
	fp = fopen(fname, "r");
	if (fp == NULL)
	    errexit("cannot open %s\n", fname);
    }

    c = read_many(fp, EOF);

    if (fname != NULL)
	fclose(fp);

    return c;
}

int next_char(FILE *fp)
{
    int c;
    do {
	c = fgetc(fp);
	if (c == '#') {
	    while (c = fgetc(fp), c != '\n' && c != EOF)
		;
	}
    } while (isspace(c));
    return c;
}

Cell read_many(FILE *fp, int closing_char)
{
    int c;
    Cell obj;

    c = next_char(fp);
    if (c == closing_char)
	return COMB_I;
    ungetc(c, fp);

    PUSH(read_one(fp, 0));
    while ((c = next_char(fp)) != closing_char) {
	ungetc(c, fp);
	obj = read_one(fp, 0);
	obj = pair(TOP, obj);
	TOP = obj;
    }
    return POP;
}

Cell read_one(FILE *fp, int i_is_iota)
{
    int c;
    Cell obj;

    c = next_char(fp);
    switch (c) {
    case '`': case '*':
	PUSH(read_one(fp, c == '*'));
	obj = read_one(fp, c == '*');
	obj = pair(TOP, obj);
	POP;
	return obj;
    case '(':
	obj = read_many(fp, ')');
	return obj;
    case 's': case 'S': return COMB_S;
    case 'k': case 'K': return COMB_K;
    case 'i': return i_is_iota ? COMB_IOTA : COMB_I;
    case 'I': return COMB_I;
    case '0': case '1': {
	obj = COMB_I;
	do {
	    if (c == '0')
		obj = pair(pair(obj, COMB_S), COMB_K);
	    else
		obj = pair(COMB_S, pair(COMB_K, obj));
	    c = next_char(fp);
	} while (c == '0' || c == '1');
	ungetc(c, fp);
	return obj;
    }
    case EOF:
	errexit("parse error: unexpected EOF\n");
    default:
	errexit("parse error: %c\n", c);
    }
}

/**********************************************************************
 *  Reducer
 **********************************************************************/

int reductions;

void eval(Cell root)
{
    Cell *bottom = rd_stack.sp;
    PUSH(root);

    for (;;) {
	while (ispair(TOP))
	    PUSH(car(TOP));

	if (TOP == COMB_I && APPLICABLE(1))
	{ /* I x -> x */
	    POP;
	    TOP = cdr(TOP);
	}
	else if (TOP == COMB_S && APPLICABLE(3))
	{ /* S f g x -> f x (g x) */
	    Cell a = alloc(2);
	    SET(a+0, ARG(1), ARG(3));	/* f x */
	    SET(a+1, ARG(2), ARG(3));	/* g x */
	    DROP(3);
	    SET(TOP, a+0, a+1);	/* f x (g x) */
	}
	else if (TOP == COMB_K && APPLICABLE(2))
	{ /* K x y -> I x */
	    Cell x = ARG(1);
	    DROP(2);
	    SET(TOP, COMB_I, x);
	    TOP = cdr(TOP);	/* shortcut reduction of I */
	}
	else if (TOP == COMB_IOTA && APPLICABLE(1))
	{ /* IOTA x -> x S K */
	    Cell xs = pair(ARG(1), COMB_S);
	    POP;
	    SET(TOP, xs, COMB_K);
	}
	else if (TOP == COMB_KI && APPLICABLE(2))
	{ /* KI x y -> I y */
	    DROP(2);
	    car(TOP) = COMB_I;
	}
	else if (TOP == COMB_CONS && APPLICABLE(3))
	{ /* CONS x y f -> f x y */
	    Cell fx, y;
	    fx = pair(ARG(3), ARG(1));
	    y = ARG(2);
	    DROP(3);
	    SET(TOP, fx, y);
	}
	else if (TOP == COMB_READ && APPLICABLE(2))
	{ /* READ NIL f -> CONS CHAR(c) (READ NIL) f */
	    intptr_t c = getchar();
	    Cell a = alloc(2);
	    SET(a+0, COMB_CONS, mkchar(c == EOF ? 256 : c));
	    SET(a+1, COMB_READ, NIL);
	    POP;
	    SET(TOP, a+0, a+1);
	}
	else if (TOP == COMB_WRITE && APPLICABLE(1))
	{ /* WRITE x -> putc(eval((car x) INC NUM(0))); WRITE (cdr x) */
	    Cell a = alloc(3);
	    SET(a+0, ARG(1), COMB_K);	/* (car x) */
	    SET(a+1, a+0, COMB_INC);	/* (car x) INC */
	    SET(a+2, a+1, mkint(0));	/* (car x) INC NUM(0) */
	    POP;
	    eval(a+2);

	    if (!isint(TOP))
		errexit("invalid output format (result was not a number)\n");
	    if (intof(TOP) >= 256)
		return;

	    putchar(intof(TOP));
	    POP;
	    a = pair(cdr(TOP), COMB_KI);
	    cdr(TOP) = a;
	}
	else if (TOP == COMB_INC && APPLICABLE(1))
	{ /* INC x -> eval(x)+1 */
	    Cell c = ARG(1);
	    POP;
	    eval(c);

	    c = POP;
	    if (!isint(c))
		errexit("invalid output format (attempted to apply inc to a non-number)\n");
	    SET(TOP, COMB_I, mkint(intof(c) + 1));
	}
	else if (ischar(TOP) && APPLICABLE(2)) {
	    intptr_t c = charof(TOP);
	    if (c <= 0) {  /* CHAR(0) f z -> z */
		Cell z = ARG(2);
		DROP(2);
		SET(TOP, COMB_I, z);
	    }
	    else {       /* CHAR(n+1) f z -> f (CHAR(n) f z) */
		Cell a = alloc(2);
		Cell f = ARG(1);
		SET(a+0, mkchar(c-1), f);	/* CHAR(n) f */
		SET(a+1, a+0, ARG(2));		/* CHAR(n) f z */
		DROP(2);
		SET(TOP, f, a+1);		/* f (CHAR(n) f z) */
	    }
	}
	else if (isint(TOP) && APPLICABLE(1))
	    errexit("invalid output format (attempted to apply a number)\n");
	else
	    return;
	reductions++;
    }
}

void eval_print(Cell root)
{
    eval(pair(COMB_WRITE,
	      pair(root,
		   pair(COMB_READ, NIL))));
}

/**********************************************************************
 *  Main
 **********************************************************************/

int main(int argc, char *argv[])
{
    Cell root;
    clock_t start;
    char *prog_file = NULL;
    int i;
    int print_stats = 0;
    
    for (i = 1; i < argc; i++) {
	if (strcmp(argv[i], "-g") == 0)
	    gc_notify = 1;
	else if (strcmp(argv[i], "-s") == 0)
	    print_stats = 1;
        else if (strcmp(argv[i], "-u") == 0)
	    setbuf(stdout, NULL);
	else
	    prog_file = argv[i];
    }

    storage_init(INITIAL_HEAP_SIZE);
    rs_init();

    root = load_program(prog_file);

    start = clock();
    eval_print(root);

    if (print_stats) {
	double evaltime = (clock() - start) / (double)CLOCKS_PER_SEC;

	printf("\n%d reductions\n", reductions);
	printf("  total eval time --- %5.2f sec.\n", evaltime - total_gc_time);
	printf("  total gc time   --- %5.2f sec.\n", total_gc_time);
	printf("  max stack depth --- %d\n", rs_max_depth());
    }
    return 0;
}
