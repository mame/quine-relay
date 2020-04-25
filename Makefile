MAKEFLAGS += --no-print-directory

PATH := $(CURDIR)/vendor/local/bin:/usr/games:$(PATH)
CLASSPATH := .

find_any0 = $(firstword $(foreach x,$(1),$(if $(shell which $(x) 2>/dev/null),$(x),)))
check = $(if $(2),$(2),$(error $(1) interpreter not found!))
find_any = $(call check,$(1),$(call find_any0,$(2)))

JAVASCRIPT := $(call find_any,JavaScript,nodejs node js)
SCHEME     := $(call find_any,Scheme,guile csi gosh)
BF         := $(call find_any,Brainfuck,bf beef)
GBS        := $(call find_any,Gambas script,gbs3 gbs2)

ifeq ($(SCHEME),csi)
  SCHEME := csi -s
endif
ifeq ($(BF),bf)
  BF := bf -c500000
endif

.DELETE_ON_ERROR:

all: QR2.rb
	@echo
	@echo "#############"
	@echo "##  CHECK  ##"
	@echo "#############"
	@echo
	diff -s QR.rb QR2.rb

check: all
	@sha1sum --quiet -c SHA1SUMS

QR.rs: QR.rb
	@echo
	@echo "#######################"
	@echo "##  1: Ruby -> Rust  ##"
	@echo "#######################"
	@echo
	ruby QR.rb > QR.rs

QR.scala: QR.rs
	@echo
	@echo "########################"
	@echo "##  2: Rust -> Scala  ##"
	@echo "########################"
	@echo
	rustc QR.rs
	./QR > QR.scala

QR.scm: QR.scala
	@echo
	@echo "##########################"
	@echo "##  3: Scala -> Scheme  ##"
	@echo "##########################"
	@echo
	scalac QR.scala
	scala QR > QR.scm

QR.sci: QR.scm
	@echo
	@echo "###########################"
	@echo "##  4: Scheme -> Scilab  ##"
	@echo "###########################"
	@echo
	$(SCHEME) QR.scm > QR.sci

QR.sed: QR.sci
	@echo
	@echo "########################"
	@echo "##  5: Scilab -> sed  ##"
	@echo "########################"
	@echo
	scilab-cli -nb -f QR.sci > QR.sed

QR.spl: QR.sed
	@echo
	@echo "#############################"
	@echo "##  6: sed -> Shakespeare  ##"
	@echo "#############################"
	@echo
	sed -E -f QR.sed QR.sed > QR.spl

QR.sl: QR.spl
	@echo
	@echo "################################"
	@echo "##  7: Shakespeare -> S-Lang  ##"
	@echo "################################"
	@echo
	./vendor/local/bin/spl2c < QR.spl > QR.spl.c
	gcc -o QR -I ./vendor/local/include -L ./vendor/local/lib QR.spl.c -lspl -lm
	./QR > QR.sl

QR.st: QR.sl
	@echo
	@echo "##############################"
	@echo "##  8: S-Lang -> Smalltalk  ##"
	@echo "##############################"
	@echo
	slsh QR.sl > QR.st

QR.nut: QR.st
	@echo
	@echo "################################"
	@echo "##  9: Smalltalk -> Squirrel  ##"
	@echo "################################"
	@echo
	gst QR.st > QR.nut

QR.sml: QR.nut
	@echo
	@echo "###################################"
	@echo "##  10: Squirrel -> Standard ML  ##"
	@echo "###################################"
	@echo
	squirrel QR.nut > QR.sml

QR.sq: QR.sml
	@echo
	@echo "#################################"
	@echo "##  11: Standard ML -> Subleq  ##"
	@echo "#################################"
	@echo
	mlton @MLton fixed-heap 200M -- QR.sml
	./QR > QR.sq

QR.tcl: QR.sq
	@echo
	@echo "#########################"
	@echo "##  12: Subleq -> Tcl  ##"
	@echo "#########################"
	@echo
	ruby vendor/subleq.rb QR.sq > QR.tcl

QR.tcsh: QR.tcl
	@echo
	@echo "#######################"
	@echo "##  13: Tcl -> tcsh  ##"
	@echo "#######################"
	@echo
	tclsh QR.tcl > QR.tcsh

QR.t: QR.tcsh
	@echo
	@echo "########################"
	@echo "##  14: tcsh -> Thue  ##"
	@echo "########################"
	@echo
	tcsh QR.tcsh > QR.t

QR.ts: QR.t
	@echo
	@echo "##############################"
	@echo "##  15: Thue -> TypeScript  ##"
	@echo "##############################"
	@echo
	ruby vendor/thue.rb QR.t > QR.ts

QR.unl: QR.ts
	@echo
	@echo "##################################"
	@echo "##  16: TypeScript -> Unlambda  ##"
	@echo "##################################"
	@echo
	tsc --outFile QR.ts.js QR.ts
	$(JAVASCRIPT) QR.ts.js > QR.unl

QR.vala: QR.unl
	@echo
	@echo "############################"
	@echo "##  17: Unlambda -> Vala  ##"
	@echo "############################"
	@echo
	ruby vendor/unlambda.rb QR.unl > QR.vala

QR.mid: QR.vala
	@echo
	@echo "##########################"
	@echo "##  18: Vala -> Velato  ##"
	@echo "##########################"
	@echo
	valac QR.vala
	./QR > QR.mid

QR.v: QR.mid
	@echo
	@echo "#############################"
	@echo "##  19: Velato -> Verilog  ##"
	@echo "#############################"
	@echo
	mono vendor/local/bin/Vlt.exe /s QR.mid
	mono QR.exe > QR.v

QR.vim: QR.v
	@echo
	@echo "################################"
	@echo "##  20: Verilog -> Vimscript  ##"
	@echo "################################"
	@echo
	iverilog -o QR QR.v
	./QR -vcd-none > QR.vim

QR.vb: QR.vim
	@echo
	@echo "#####################################"
	@echo "##  21: Vimscript -> Visual Basic  ##"
	@echo "#####################################"
	@echo
	vim -EsS QR.vim > QR.vb

QR.ws: QR.vb
	@echo
	@echo "######################################"
	@echo "##  22: Visual Basic -> Whitespace  ##"
	@echo "######################################"
	@echo
	vbnc QR.vb
	mono ./QR.exe > QR.ws

QR.xslt: QR.ws
	@echo
	@echo "##############################"
	@echo "##  23: Whitespace -> XSLT  ##"
	@echo "##############################"
	@echo
	ruby vendor/whitespace.rb QR.ws > QR.xslt

QR.yab: QR.xslt
	@echo
	@echo "###########################"
	@echo "##  24: XSLT -> Yabasic  ##"
	@echo "###########################"
	@echo
	xsltproc QR.xslt > QR.yab

QR.yorick: QR.yab
	@echo
	@echo "#############################"
	@echo "##  25: Yabasic -> Yorick  ##"
	@echo "#############################"
	@echo
	yabasic QR.yab > QR.yorick

QR.azm: QR.yorick
	@echo
	@echo "##########################"
	@echo "##  26: Yorick -> Zoem  ##"
	@echo "##########################"
	@echo
	yorick -batch QR.yorick > QR.azm

QR.zsh: QR.azm
	@echo
	@echo "#######################"
	@echo "##  27: Zoem -> zsh  ##"
	@echo "#######################"
	@echo
	zoem -i QR.azm > QR.zsh

QR.+: QR.zsh
	@echo
	@echo "#####################"
	@echo "##  28: zsh -> A+  ##"
	@echo "#####################"
	@echo
	zsh QR.zsh > QR.+

qr.adb: QR.+
	@echo
	@echo "#####################"
	@echo "##  29: A+ -> Ada  ##"
	@echo "#####################"
	@echo
	a+ QR.+ > qr.adb

QR.als: qr.adb
	@echo
	@echo "########################"
	@echo "##  30: Ada -> AFNIX  ##"
	@echo "########################"
	@echo
	gnatmake qr.adb
	./qr > QR.als

QR.aheui: QR.als
	@echo
	@echo "##########################"
	@echo "##  31: AFNIX -> Aheui  ##"
	@echo "##########################"
	@echo
	LANG=C LD_LIBRARY_PATH=/usr/lib/afnix axi QR.als > QR.aheui

QR.a68: QR.aheui
	@echo
	@echo "#############################"
	@echo "##  32: Aheui -> ALGOL 68  ##"
	@echo "#############################"
	@echo
	ruby vendor/aheui.rb QR.aheui > QR.a68

QR.ante: QR.a68
	@echo
	@echo "############################"
	@echo "##  33: ALGOL 68 -> Ante  ##"
	@echo "############################"
	@echo
	a68g QR.a68 > QR.ante

QR.cc: QR.ante
	@echo
	@echo "#############################"
	@echo "##  34: Ante -> AspectC++  ##"
	@echo "#############################"
	@echo
	ruby vendor/ante.rb QR.ante > QR.cc

QR.aj: QR.cc
	@echo
	@echo "################################"
	@echo "##  35: AspectC++ -> AspectJ  ##"
	@echo "################################"
	@echo
	ag++ -std=c++11 -o QR QR.cc
	./QR > QR.aj

QR.asy: QR.aj
	@echo
	@echo "################################"
	@echo "##  36: AspectJ -> Asymptote  ##"
	@echo "################################"
	@echo
	ajc QR.aj
	java QR > QR.asy

QR.dats: QR.asy
	@echo
	@echo "############################"
	@echo "##  37: Asymptote -> ATS  ##"
	@echo "############################"
	@echo
	asy QR.asy > QR.dats

QR.awk: QR.dats
	@echo
	@echo "######################"
	@echo "##  38: ATS -> Awk  ##"
	@echo "######################"
	@echo
	patscc -o QR QR.dats
	./QR > QR.awk

QR.bash: QR.awk
	@echo
	@echo "#######################"
	@echo "##  39: Awk -> bash  ##"
	@echo "#######################"
	@echo
	awk -f QR.awk > QR.bash

QR.bc: QR.bash
	@echo
	@echo "######################"
	@echo "##  40: bash -> bc  ##"
	@echo "######################"
	@echo
	bash QR.bash > QR.bc

QR.bsh: QR.bc
	@echo
	@echo "###########################"
	@echo "##  41: bc -> BeanShell  ##"
	@echo "###########################"
	@echo
	BC_LINE_LENGTH=4000000 bc -q QR.bc > QR.bsh

QR.bef: QR.bsh
	@echo
	@echo "################################"
	@echo "##  42: BeanShell -> Befunge  ##"
	@echo "################################"
	@echo
	bsh QR.bsh > QR.bef

QR.Blc: QR.bef
	@echo
	@echo "###########################"
	@echo "##  43: Befunge -> BLC8  ##"
	@echo "###########################"
	@echo
	cfunge QR.bef > QR.Blc

QR.bf: QR.Blc
	@echo
	@echo "#############################"
	@echo "##  44: BLC8 -> Brainfuck  ##"
	@echo "#############################"
	@echo
	ruby vendor/blc.rb < QR.Blc > QR.bf

QR.c: QR.bf
	@echo
	@echo "##########################"
	@echo "##  45: Brainfuck -> C  ##"
	@echo "##########################"
	@echo
	$(BF) QR.bf > QR.c

QR.cpp: QR.c
	@echo
	@echo "####################"
	@echo "##  46: C -> C++  ##"
	@echo "####################"
	@echo
	$(CC) -o QR QR.c
	./QR > QR.cpp

QR.cs: QR.cpp
	@echo
	@echo "#####################"
	@echo "##  47: C++ -> C#  ##"
	@echo "#####################"
	@echo
	$(CXX) -o QR QR.cpp
	./QR > QR.cs

QR.chef: QR.cs
	@echo
	@echo "######################"
	@echo "##  48: C# -> Chef  ##"
	@echo "######################"
	@echo
	mcs QR.cs
	mono QR.exe > QR.chef

QR.clj: QR.chef
	@echo
	@echo "###########################"
	@echo "##  49: Chef -> Clojure  ##"
	@echo "###########################"
	@echo
	PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl
	perl QR.chef.pl > QR.clj

QR.cmake: QR.clj
	@echo
	@echo "############################"
	@echo "##  50: Clojure -> CMake  ##"
	@echo "############################"
	@echo
	clojure QR.clj > QR.cmake

QR.cob: QR.cmake
	@echo
	@echo "##########################"
	@echo "##  51: CMake -> Cobol  ##"
	@echo "##########################"
	@echo
	cmake -P QR.cmake > QR.cob

QR.coffee: QR.cob
	@echo
	@echo "#################################"
	@echo "##  52: Cobol -> CoffeeScript  ##"
	@echo "#################################"
	@echo
	cobc -O2 -x QR.cob
	./QR > QR.coffee

QR.lisp: QR.coffee
	@echo
	@echo "#######################################"
	@echo "##  53: CoffeeScript -> Common Lisp  ##"
	@echo "#######################################"
	@echo
	coffee --nodejs --stack_size=100000 QR.coffee > QR.lisp

QR.curry: QR.lisp
	@echo
	@echo "################################"
	@echo "##  54: Common Lisp -> Curry  ##"
	@echo "################################"
	@echo
	clisp QR.lisp > QR.curry

QR.d: QR.curry
	@echo
	@echo "######################"
	@echo "##  55: Curry -> D  ##"
	@echo "######################"
	@echo
	pakcs --nocypm :load QR.curry :save :quit
	./QR > QR.d

QR.dfy: QR.d
	@echo
	@echo "######################"
	@echo "##  56: D -> Dafny  ##"
	@echo "######################"
	@echo
	gdc -o QR QR.d
	./QR > QR.dfy

QR.dc: QR.dfy
	@echo
	@echo "#######################"
	@echo "##  57: Dafny -> dc  ##"
	@echo "#######################"
	@echo
	dafny QR.dfy
	mono QR.exe > QR.dc

QR.ec: QR.dc
	@echo
	@echo "####################"
	@echo "##  58: dc -> eC  ##"
	@echo "####################"
	@echo
	dc QR.dc > QR.ec || true

QR.exs: QR.ec
	@echo
	@echo "########################"
	@echo "##  59: eC -> Elixir  ##"
	@echo "########################"
	@echo
	@mv QR.c QR.c.bak
	ecp -c QR.ec -o QR.sym
	ecc -c QR.ec -o QR.c
	ecs -console QR.sym QR.imp -o QR.main.ec
	ecp -c QR.main.ec -o QR.main.sym
	ecc -c QR.main.ec -o QR.main.c
	gcc -o QR QR.c QR.main.c -lecereCOM
	./QR > QR.exs
	@mv QR.c.bak QR.c

QR.el: QR.exs
	@echo
	@echo "################################"
	@echo "##  60: Elixir -> Emacs Lisp  ##"
	@echo "################################"
	@echo
	elixir QR.exs > QR.el

QR.erl: QR.el
	@echo
	@echo "################################"
	@echo "##  61: Emacs Lisp -> Erlang  ##"
	@echo "################################"
	@echo
	emacs -Q --script QR.el > QR.erl

QR.fsx: QR.erl
	@echo
	@echo "########################"
	@echo "##  62: Erlang -> F#  ##"
	@echo "########################"
	@echo
	escript QR.erl > QR.fsx

QR.false: QR.fsx
	@echo
	@echo "#######################"
	@echo "##  63: F# -> FALSE  ##"
	@echo "#######################"
	@echo
	fsharpc QR.fsx -o QR.exe
	mono QR.exe > QR.false

QR.fl: QR.false
	@echo
	@echo "#########################"
	@echo "##  64: FALSE -> Flex  ##"
	@echo "#########################"
	@echo
	ruby vendor/false.rb QR.false > QR.fl

QR.fish: QR.fl
	@echo
	@echo "########################"
	@echo "##  65: Flex -> Fish  ##"
	@echo "########################"
	@echo
	flex -o QR.fl.c QR.fl
	gcc -o QR QR.fl.c
	./QR > QR.fish

QR.fs: QR.fish
	@echo
	@echo "#########################"
	@echo "##  66: Fish -> Forth  ##"
	@echo "#########################"
	@echo
	fish QR.fish > QR.fs

QR.f: QR.fs
	@echo
	@echo "##############################"
	@echo "##  67: Forth -> FORTRAN77  ##"
	@echo "##############################"
	@echo
	gforth QR.fs > QR.f

QR.f90: QR.f
	@echo
	@echo "##################################"
	@echo "##  68: FORTRAN77 -> Fortran90  ##"
	@echo "##################################"
	@echo
	@mv QR.c QR.c.bak
	gfortran -o QR QR.f
	./QR > QR.f90
	@mv QR.c.bak QR.c

QR.gbs: QR.f90
	@echo
	@echo "######################################"
	@echo "##  69: Fortran90 -> Gambas script  ##"
	@echo "######################################"
	@echo
	gfortran -o QR QR.f90
	./QR > QR.gbs

QR.g: QR.gbs
	@echo
	@echo "################################"
	@echo "##  70: Gambas script -> GAP  ##"
	@echo "################################"
	@echo
	$(GBS) QR.gbs > QR.g

QR.gdb: QR.g
	@echo
	@echo "######################"
	@echo "##  71: GAP -> GDB  ##"
	@echo "######################"
	@echo
	gap -q QR.g > QR.gdb

QR.gel: QR.gdb
	@echo
	@echo "###############################"
	@echo "##  72: GDB -> GEL (Genius)  ##"
	@echo "###############################"
	@echo
	gdb -q -x QR.gdb > QR.gel

QR.gsl: QR.gel
	@echo
	@echo "######################################################"
	@echo "##  73: GEL (Genius) -> GeneratorScriptingLanguage  ##"
	@echo "######################################################"
	@echo
	genius QR.gel > QR.gsl

QR.plt: QR.gsl
	@echo
	@echo "#################################################"
	@echo "##  74: GeneratorScriptingLanguage -> Gnuplot  ##"
	@echo "#################################################"
	@echo
	gsl -q QR.gsl > QR.plt

QR.go: QR.plt
	@echo
	@echo "#########################"
	@echo "##  75: Gnuplot -> Go  ##"
	@echo "#########################"
	@echo
	gnuplot QR.plt > QR.go

QR.gs: QR.go
	@echo
	@echo "############################"
	@echo "##  76: Go -> GolfScript  ##"
	@echo "############################"
	@echo
	go run QR.go > QR.gs

QR.grass: QR.gs
	@echo
	@echo "###############################"
	@echo "##  77: GolfScript -> Grass  ##"
	@echo "###############################"
	@echo
	ruby vendor/golfscript.rb QR.gs > QR.grass

QR.groovy: QR.grass
	@echo
	@echo "###########################"
	@echo "##  78: Grass -> Groovy  ##"
	@echo "###########################"
	@echo
	ruby vendor/grass.rb QR.grass > QR.groovy

QR.gz: QR.groovy
	@echo
	@echo "##########################"
	@echo "##  79: Groovy -> Gzip  ##"
	@echo "##########################"
	@echo
	groovy QR.groovy > QR.gz

QR.hs: QR.gz
	@echo
	@echo "###########################"
	@echo "##  80: Gzip -> Haskell  ##"
	@echo "###########################"
	@echo
	gzip -cd QR.gz > QR.hs

QR.hx: QR.hs
	@echo
	@echo "###########################"
	@echo "##  81: Haskell -> Haxe  ##"
	@echo "###########################"
	@echo
	ghc QR.hs
	./QR > QR.hx

QR.icn: QR.hx
	@echo
	@echo "########################"
	@echo "##  82: Haxe -> Icon  ##"
	@echo "########################"
	@echo
	haxe -main QR -neko QR.n
	neko QR.n > QR.icn

QR.i: QR.icn
	@echo
	@echo "############################"
	@echo "##  83: Icon -> INTERCAL  ##"
	@echo "############################"
	@echo
	icont -s QR.icn
	./QR > QR.i

QR.j: QR.i
	@echo
	@echo "##############################"
	@echo "##  84: INTERCAL -> Jasmin  ##"
	@echo "##############################"
	@echo
	@mv QR.c QR.c.bak
	ick -bfOc QR.i
	gcc -static QR.c -I /usr/include/ick-* -o QR -lick
	./QR > QR.j
	@mv QR.c.bak QR.c

QR.java: QR.j
	@echo
	@echo "##########################"
	@echo "##  85: Jasmin -> Java  ##"
	@echo "##########################"
	@echo
	jasmin QR.j
	java QR > QR.java

QR.js: QR.java
	@echo
	@echo "##############################"
	@echo "##  86: Java -> JavaScript  ##"
	@echo "##############################"
	@echo
	javac QR.java
	java QR > QR.js

QR.jq: QR.js
	@echo
	@echo "############################"
	@echo "##  87: JavaScript -> Jq  ##"
	@echo "############################"
	@echo
	$(JAVASCRIPT) QR.js > QR.jq

QR.jsfuck: QR.jq
	@echo
	@echo "########################"
	@echo "##  88: Jq -> JSFuck  ##"
	@echo "########################"
	@echo
	jq -r -n -f QR.jq > QR.jsfuck

QR.jl: QR.jsfuck
	@echo
	@echo "###########################"
	@echo "##  89: JSFuck -> Julia  ##"
	@echo "###########################"
	@echo
	ulimit -s unlimited && $(JAVASCRIPT) --stack_size=100000 QR.jsfuck > QR.jl

QR.ksh: QR.jl
	@echo
	@echo "########################"
	@echo "##  90: Julia -> ksh  ##"
	@echo "########################"
	@echo
	julia QR.jl > QR.ksh

QR.lazy: QR.ksh
	@echo
	@echo "#########################"
	@echo "##  91: ksh -> Lazy K  ##"
	@echo "#########################"
	@echo
	ksh QR.ksh > QR.lazy

qr.li: QR.lazy
	@echo
	@echo "############################"
	@echo "##  92: Lazy K -> Lisaac  ##"
	@echo "############################"
	@echo
	lazyk QR.lazy > qr.li

QR.ls: qr.li
	@echo
	@echo "################################"
	@echo "##  93: Lisaac -> LiveScript  ##"
	@echo "################################"
	@echo
	lisaac qr.li
	./qr > QR.ls

QR.ll: QR.ls
	@echo
	@echo "##################################"
	@echo "##  94: LiveScript -> LLVM asm  ##"
	@echo "##################################"
	@echo
	lsc QR.ls > QR.ll

QR.lol: QR.ll
	@echo
	@echo "###############################"
	@echo "##  95: LLVM asm -> LOLCODE  ##"
	@echo "###############################"
	@echo
	@mv QR.bc QR.bc.bak
	llvm-as QR.ll
	lli QR.bc > QR.lol
	@mv QR.bc.bak QR.bc

QR.lua: QR.lol
	@echo
	@echo "##########################"
	@echo "##  96: LOLCODE -> Lua  ##"
	@echo "##########################"
	@echo
	lci QR.lol > QR.lua

QR.m4: QR.lua
	@echo
	@echo "#####################"
	@echo "##  97: Lua -> M4  ##"
	@echo "#####################"
	@echo
	lua5.3 QR.lua > QR.m4

QR.mk: QR.m4
	@echo
	@echo "##########################"
	@echo "##  98: M4 -> Makefile  ##"
	@echo "##########################"
	@echo
	m4 QR.m4 > QR.mk

QR.mac: QR.mk
	@echo
	@echo "##############################"
	@echo "##  99: Makefile -> Maxima  ##"
	@echo "##############################"
	@echo
	make -f QR.mk > QR.mac

QR.mzn: QR.mac
	@echo
	@echo "###############################"
	@echo "##  100: Maxima -> MiniZinc  ##"
	@echo "###############################"
	@echo
	@if [ $(CI) = "true" ]; then mv /tmp /tmp.bak && ln -s /dev/shm /tmp; fi
	maxima -q --init-mac=QR.mac > QR.mzn
	@if [ $(CI) = "true" ]; then rm /tmp && mv /tmp.bak /tmp; fi

QR.il: QR.mzn
	@echo
	@echo "#############################"
	@echo "##  101: MiniZinc -> MSIL  ##"
	@echo "#############################"
	@echo
	minizinc --solver Gecode --soln-sep '' QR.mzn > QR.il

QR.mustache: QR.il
	@echo
	@echo "#############################"
	@echo "##  102: MSIL -> Mustache  ##"
	@echo "#############################"
	@echo
	ilasm QR.il
	mono QR.exe > QR.mustache

QR.asm: QR.mustache
	@echo
	@echo "#############################"
	@echo "##  103: Mustache -> NASM  ##"
	@echo "#############################"
	@echo
	mustache QR.mustache QR.mustache > QR.asm

QR.neko: QR.asm
	@echo
	@echo "#########################"
	@echo "##  104: NASM -> Neko  ##"
	@echo "#########################"
	@echo
	nasm -felf QR.asm
	ld -m elf_i386 -o QR QR.o
	./QR > QR.neko

QR.5c: QR.neko
	@echo
	@echo "###########################"
	@echo "##  105: Neko -> Nickle  ##"
	@echo "###########################"
	@echo
	nekoc QR.neko
	neko QR.n > QR.5c

QR.nim: QR.5c
	@echo
	@echo "##########################"
	@echo "##  106: Nickle -> Nim  ##"
	@echo "##########################"
	@echo
	nickle QR.5c > QR.nim

QR.m: QR.nim
	@echo
	@echo "###############################"
	@echo "##  107: Nim -> Objective-C  ##"
	@echo "###############################"
	@echo
	nim c QR.nim
	./QR > QR.m

QR.ml: QR.m
	@echo
	@echo "#################################"
	@echo "##  108: Objective-C -> OCaml  ##"
	@echo "#################################"
	@echo
	gcc -o QR QR.m
	./QR > QR.ml

QR.octave: QR.ml
	@echo
	@echo "############################"
	@echo "##  109: OCaml -> Octave  ##"
	@echo "############################"
	@echo
	ocaml QR.ml > QR.octave

QR.ook: QR.octave
	@echo
	@echo "###########################"
	@echo "##  110: Octave -> Ook!  ##"
	@echo "###########################"
	@echo
	mv QR.m QR.m.bak
	octave -qf QR.octave > QR.ook
	mv QR.m.bak QR.m

QR.gp: QR.ook
	@echo
	@echo "############################"
	@echo "##  111: Ook! -> PARI/GP  ##"
	@echo "############################"
	@echo
	ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf
	$(BF) QR.ook.bf > QR.gp

QR.p: QR.gp
	@echo
	@echo "################################"
	@echo "##  112: PARI/GP -> Parser 3  ##"
	@echo "################################"
	@echo
	gp -f -q QR.gp > QR.p

QR.pas: QR.p
	@echo
	@echo "###############################"
	@echo "##  113: Parser 3 -> Pascal  ##"
	@echo "###############################"
	@echo
	parser3 QR.p > QR.pas

QR.pl: QR.pas
	@echo
	@echo "#############################"
	@echo "##  114: Pascal -> Perl 5  ##"
	@echo "#############################"
	@echo
	fpc QR.pas
	./QR > QR.pl

QR.pl6: QR.pl
	@echo
	@echo "#############################"
	@echo "##  115: Perl 5 -> Perl 6  ##"
	@echo "#############################"
	@echo
	perl QR.pl > QR.pl6

QR.php: QR.pl6
	@echo
	@echo "##########################"
	@echo "##  116: Perl 6 -> PHP  ##"
	@echo "##########################"
	@echo
	perl6 QR.pl6 > QR.php

QR.png: QR.php
	@echo
	@echo "########################"
	@echo "##  117: PHP -> Piet  ##"
	@echo "########################"
	@echo
	php QR.php > QR.png

QR.pike: QR.png
	@echo
	@echo "#########################"
	@echo "##  118: Piet -> Pike  ##"
	@echo "#########################"
	@echo
	npiet QR.png > QR.pike

QR.ps: QR.pike
	@echo
	@echo "###############################"
	@echo "##  119: Pike -> PostScript  ##"
	@echo "###############################"
	@echo
	pike QR.pike > QR.ps

QR.ppt: QR.ps
	@echo
	@echo "#############################################"
	@echo "##  120: PostScript -> PPT (Punched tape)  ##"
	@echo "#############################################"
	@echo
	gs -dNODISPLAY -q QR.ps > QR.ppt

QR.prolog: QR.ppt
	@echo
	@echo "#########################################"
	@echo "##  121: PPT (Punched tape) -> Prolog  ##"
	@echo "#########################################"
	@echo
	ppt -d < QR.ppt > QR.prolog

QR.pr: QR.prolog
	@echo
	@echo "#####################################"
	@echo "##  122: Prolog -> Promela (Spin)  ##"
	@echo "#####################################"
	@echo
	swipl -q -t qr -f QR.prolog > QR.pr

QR.py: QR.pr
	@echo
	@echo "#####################################"
	@echo "##  123: Promela (Spin) -> Python  ##"
	@echo "#####################################"
	@echo
	spin -T QR.pr > QR.py

QR.R: QR.py
	@echo
	@echo "########################"
	@echo "##  124: Python -> R  ##"
	@echo "########################"
	@echo
	python3 QR.py > QR.R

QR.ratfor: QR.R
	@echo
	@echo "########################"
	@echo "##  125: R -> Ratfor  ##"
	@echo "########################"
	@echo
	R --slave -f QR.R > QR.ratfor

QR.rc: QR.ratfor
	@echo
	@echo "#########################"
	@echo "##  126: Ratfor -> rc  ##"
	@echo "#########################"
	@echo
	ratfor -o QR.ratfor.f QR.ratfor
	gfortran -o QR QR.ratfor.f
	./QR > QR.rc

QR.rexx: QR.rc
	@echo
	@echo "#######################"
	@echo "##  127: rc -> REXX  ##"
	@echo "#######################"
	@echo
	rc QR.rc > QR.rexx

QR2.rb: QR.rexx
	@echo
	@echo "#########################"
	@echo "##  128: REXX -> Ruby  ##"
	@echo "#########################"
	@echo
	rexx ./QR.rexx > QR2.rb

clean:
	@mv QR.rb quine-relay.rb
	rm -f qr QR qr.* QR.* QR2.rb *.class gst.im
	@mv quine-relay.rb QR.rb
