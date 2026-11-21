/* SPDX-License-Identifier: MIT */
/* Classic Malbolge as specified by Ben Olmstead (public domain).
 * https://www.lscheffer.com/malbolge_spec.html
 * Reject invalid source and out-of-range runtime encryption operands.
 */
#include <ctype.h>
#include <stdio.h>

enum { CELLS = 59049, ASCII_FIRST = 33, ASCII_LAST = 126, PERIOD = 94 };
enum { JUMP = 4, OUT = 5, IN = 23, ROT = 39, MOVE = 40,
       CRAZY = 62, NOP = 68, HALT = 81 };

static const unsigned char encryption[] =
    "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1C"
    "B6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@";

static unsigned short memory[CELLS];

static int fail(const char *message)
{
    fprintf(stderr, "malbolge: %s\n", message);
    return 1;
}

static int opcode(int value, int address)
{
    return (value + address) % PERIOD;
}

static int printable(int value)
{
    return ASCII_FIRST <= value && value <= ASCII_LAST;
}

static int valid(int op)
{
    switch (op) {
    case JUMP: case OUT: case IN: case ROT: case MOVE:
    case CRAZY: case NOP: case HALT:
        return 1;
    default:
        return 0;
    }
}

static int crazy(int accumulator, int operand)
{
    static const int trit[3][3] = {{1, 0, 0}, {1, 0, 2}, {2, 2, 1}};
    int result = 0;
    for (int power = 1; power < CELLS; power *= 3) {
        result += power * trit[operand % 3][accumulator % 3];
        operand /= 3;
        accumulator /= 3;
    }
    return result;
}

static int load(FILE *source)
{
    int length = 0;
    for (int byte; (byte = getc(source)) != EOF; ) {
        if (isspace(byte))
            continue;
        if (length == CELLS || !printable(byte) ||
            !valid(opcode(byte, length)))
            return fail("invalid source cell");
        memory[length++] = byte;
    }
    if (ferror(source) || length < 2)
        return fail("could not read a complete program");
    for (; length < CELLS; ++length)
        memory[length] = crazy(memory[length - 1], memory[length - 2]);
    return 0;
}

static int execute(void)
{
    int a = 0, c = 0, d = 0;
    for (;;) {
        if (!printable(memory[c]))
            return fail("non-printable instruction");
        int operand = memory[d];
        switch (opcode(memory[c], c)) {
        case JUMP: c = operand; break;
        case OUT:
            if (putchar(a & 255) == EOF)
                return fail("could not write output");
            break;
        case IN: {
            int byte = getchar();
            if (ferror(stdin))
                return fail("could not read input");
            a = byte == EOF ? CELLS - 1 : byte;
            break;
        }
        case ROT: a = memory[d] = operand / 3 + operand % 3 * 19683; break;
        case MOVE: d = operand; break;
        case CRAZY: a = memory[d] = crazy(a, operand); break;
        case HALT: return fflush(stdout) == EOF ? fail("could not flush output") : 0;
        }
        if (!printable(memory[c]))
            return fail("non-printable encryption operand");
        memory[c] = encryption[memory[c] - ASCII_FIRST];
        c = (c + 1) % CELLS;
        d = (d + 1) % CELLS;
    }
}

int main(int argc, char **argv)
{
    if (argc != 2)
        return fail("usage: malbolge FILE");
    FILE *source = fopen(argv[1], "rb");
    if (!source)
        return fail("could not open source");
    int result = load(source);
    fclose(source);
    return result ? result : execute();
}
