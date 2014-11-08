MAKEFLAGS += --no-print-directory

NODE := $(shell which nodejs 2>/dev/null || which node)
ifeq ($(NODE),)
  $(warning Node.js not found!)
endif

SCHEME := $(shell which guile csi gosh 2>/dev/null | head -1)
ifeq ($(SCHEME),)
  $(warning Scheme interpreter not found!)
endif
ifeq ($(SCHEME),$(shell which csi 2>/dev/null | head -1))
  SCHEME := csi -s
endif

BF := $(shell which bf beef 2>/dev/null | head -1)
ifeq ($(BF),)
  $(warning Brainfuck interpreter not found!)
endif

.DELETE_ON_ERROR:


all: QR2.rb
	@echo
	@echo "#############"
	@echo "##  CHECK  ##"
	@echo "#############"
	@echo
	diff QR.rb QR2.rb

QR.scala: QR.rb
	@echo
	@echo "########################"
	@echo "##  1: Ruby -> Scala  ##"
	@echo "########################"
	@echo
	ruby QR.rb > QR.scala

QR.scm: QR.scala
	@echo
	@echo "##########################"
	@echo "##  2: Scala -> Scheme  ##"
	@echo "##########################"
	@echo
	scalac QR.scala
	CLASSPATH=. scala QR > QR.scm

QR.bash: QR.scm
	@echo
	@echo "##########################"
	@echo "##  3: Scheme -> Shell  ##"
	@echo "##########################"
	@echo
	$(SCHEME) QR.scm > QR.bash

QR.st: QR.bash
	@echo
	@echo "#############################"
	@echo "##  4: Shell -> Smalltalk  ##"
	@echo "#############################"
	@echo
	bash QR.bash > QR.st

QR.tcl: QR.st
	@echo
	@echo "###########################"
	@echo "##  5: Smalltalk -> Tcl  ##"
	@echo "###########################"
	@echo
	gst QR.st > QR.tcl

QR.unl: QR.tcl
	@echo
	@echo "##########################"
	@echo "##  6: Tcl -> Unlambda  ##"
	@echo "##########################"
	@echo
	tclsh QR.tcl > QR.unl

QR.vala: QR.unl
	@echo
	@echo "###########################"
	@echo "##  7: Unlambda -> Vala  ##"
	@echo "###########################"
	@echo
	ruby vendor/unlambda.rb QR.unl > QR.vala

QR.v: QR.vala
	@echo
	@echo "##########################"
	@echo "##  8: Vala -> Verilog  ##"
	@echo "##########################"
	@echo
	valac QR.vala
	./QR > QR.v

QR.ws: QR.v
	@echo
	@echo "################################"
	@echo "##  9: Verilog -> Whitespace  ##"
	@echo "################################"
	@echo
	iverilog -o QR QR.v
	./QR -vcd-none > QR.ws

qr.adb: QR.ws
	@echo
	@echo "#############################"
	@echo "##  10: Whitespace -> Ada  ##"
	@echo "#############################"
	@echo
	ruby vendor/whitespace.rb QR.ws > qr.adb

QR.a68: qr.adb
	@echo
	@echo "##########################"
	@echo "##  11: Ada -> ALGOL68  ##"
	@echo "##########################"
	@echo
	gnatmake qr.adb
	./qr > QR.a68

QR.awk: QR.a68
	@echo
	@echo "##########################"
	@echo "##  12: ALGOL68 -> Awk  ##"
	@echo "##########################"
	@echo
	a68g QR.a68 > QR.awk

QR.boo: QR.awk
	@echo
	@echo "######################"
	@echo "##  13: Awk -> Boo  ##"
	@echo "######################"
	@echo
	awk -f QR.awk > QR.boo

QR.bf: QR.boo
	@echo
	@echo "############################"
	@echo "##  14: Boo -> Brainfuck  ##"
	@echo "############################"
	@echo
	booi QR.boo > QR.bf

QR.c: QR.bf
	@echo
	@echo "##########################"
	@echo "##  15: Brainfuck -> C  ##"
	@echo "##########################"
	@echo
	$(BF) QR.bf > QR.c

QR.cpp: QR.c
	@echo
	@echo "####################"
	@echo "##  16: C -> C++  ##"
	@echo "####################"
	@echo
	${CC} -o QR QR.c
	./QR > QR.cpp

QR.cs: QR.cpp
	@echo
	@echo "#####################"
	@echo "##  17: C++ -> C#  ##"
	@echo "#####################"
	@echo
	${CXX} -o QR QR.cpp
	./QR > QR.cs

QR.clj: QR.cs
	@echo
	@echo "#########################"
	@echo "##  18: C# -> Clojure  ##"
	@echo "#########################"
	@echo
	mcs QR.cs
	mono QR.exe > QR.clj

QR.cob: QR.clj
	@echo
	@echo "############################"
	@echo "##  19: Clojure -> Cobol  ##"
	@echo "############################"
	@echo
	clojure QR.clj > QR.cob

QR.coffee: QR.cob
	@echo
	@echo "#################################"
	@echo "##  20: Cobol -> CoffeeScript  ##"
	@echo "#################################"
	@echo
	cobc -O2 -x QR.cob
	./QR > QR.coffee

QR.lisp: QR.coffee
	@echo
	@echo "#######################################"
	@echo "##  21: CoffeeScript -> Common Lisp  ##"
	@echo "#######################################"
	@echo
	coffee QR.coffee > QR.lisp

QR.fs: QR.lisp
	@echo
	@echo "################################"
	@echo "##  22: Common Lisp -> Forth  ##"
	@echo "################################"
	@echo
	clisp QR.lisp > QR.fs

QR.f: QR.fs
	@echo
	@echo "##############################"
	@echo "##  23: Forth -> FORTRAN77  ##"
	@echo "##############################"
	@echo
	gforth QR.fs > QR.f

QR.f90: QR.f
	@echo
	@echo "##################################"
	@echo "##  24: FORTRAN77 -> Fortran90  ##"
	@echo "##################################"
	@echo
	mv QR.c QR.c.bak
	f2c QR.f
	${CC} -o QR QR.c -L/usr/lib -lf2c
	mv QR.c.bak QR.c
	./QR > QR.f90

QR.go: QR.f90
	@echo
	@echo "###########################"
	@echo "##  25: Fortran90 -> Go  ##"
	@echo "###########################"
	@echo
	gfortran -o QR QR.f90
	./QR > QR.go

QR.groovy: QR.go
	@echo
	@echo "########################"
	@echo "##  26: Go -> Groovy  ##"
	@echo "########################"
	@echo
	go run QR.go > QR.groovy

QR.hs: QR.groovy
	@echo
	@echo "#############################"
	@echo "##  27: Groovy -> Haskell  ##"
	@echo "#############################"
	@echo
	groovy QR.groovy > QR.hs

QR.icn: QR.hs
	@echo
	@echo "###########################"
	@echo "##  28: Haskell -> Icon  ##"
	@echo "###########################"
	@echo
	ghc QR.hs
	./QR > QR.icn

QR.i: QR.icn
	@echo
	@echo "############################"
	@echo "##  29: Icon -> INTERCAL  ##"
	@echo "############################"
	@echo
	icont -s QR.icn
	./QR > QR.i

QR.j: QR.i
	@echo
	@echo "##############################"
	@echo "##  30: INTERCAL -> Jasmin  ##"
	@echo "##############################"
	@echo
	mv QR.c QR.c.bak
	ick -bfO QR.i
	mv QR.c.bak QR.c
	./QR > QR.j

QR.java: QR.j
	@echo
	@echo "##########################"
	@echo "##  31: Jasmin -> Java  ##"
	@echo "##########################"
	@echo
	jasmin QR.j
	CLASSPATH=. java QR > QR.java

QR.ll: QR.java
	@echo
	@echo "############################"
	@echo "##  32: Java -> LLVM asm  ##"
	@echo "############################"
	@echo
	javac QR.java
	CLASSPATH=. java QR > QR.ll

QR.logo: QR.ll
	@echo
	@echo "############################"
	@echo "##  33: LLVM asm -> Logo  ##"
	@echo "############################"
	@echo
	llvm-as QR.ll
	lli QR.bc > QR.logo

QR.lol: QR.logo
	@echo
	@echo "###########################"
	@echo "##  34: Logo -> LOLCODE  ##"
	@echo "###########################"
	@echo
	logo QR.logo > QR.lol

QR.lua: QR.lol
	@echo
	@echo "##########################"
	@echo "##  35: LOLCODE -> Lua  ##"
	@echo "##########################"
	@echo
	vendor/lci-*/lci QR.lol > QR.lua

QR.makefile: QR.lua
	@echo
	@echo "###########################"
	@echo "##  36: Lua -> Makefile  ##"
	@echo "###########################"
	@echo
	lua QR.lua > QR.makefile

QR.mac: QR.makefile
	@echo
	@echo "##############################"
	@echo "##  37: Makefile -> Maxima  ##"
	@echo "##############################"
	@echo
	make -f QR.makefile > QR.mac

QR.il: QR.mac
	@echo
	@echo "##########################"
	@echo "##  38: Maxima -> MSIL  ##"
	@echo "##########################"
	@echo
	maxima -q --init-mac=QR.mac > QR.il

QR.asm: QR.il
	@echo
	@echo "########################"
	@echo "##  39: MSIL -> NASM  ##"
	@echo "########################"
	@echo
	ilasm QR.il
	mono QR.exe > QR.asm

QR.neko: QR.asm
	@echo
	@echo "########################"
	@echo "##  40: NASM -> Neko  ##"
	@echo "########################"
	@echo
	nasm -felf QR.asm
	ld -m elf_i386 -o QR QR.o
	./QR > QR.neko

QR.js: QR.neko
	@echo
	@echo "##########################"
	@echo "##  41: Neko -> NodeJS  ##"
	@echo "##########################"
	@echo
	nekoc QR.neko
	neko QR.n > QR.js

QR.m: QR.js
	@echo
	@echo "#################################"
	@echo "##  42: NodeJS -> Objective-C  ##"
	@echo "#################################"
	@echo
	$(NODE) QR.js > QR.m

QR.ml: QR.m
	@echo
	@echo "################################"
	@echo "##  43: Objective-C -> OCaml  ##"
	@echo "################################"
	@echo
	gcc -o QR QR.m
	./QR > QR.ml

QR.octave: QR.ml
	@echo
	@echo "###########################"
	@echo "##  44: OCaml -> Octave  ##"
	@echo "###########################"
	@echo
	ocaml QR.ml > QR.octave

QR.pasm: QR.octave
	@echo
	@echo "################################"
	@echo "##  45: Octave -> Parrot asm  ##"
	@echo "################################"
	@echo
	octave -qf QR.octave > QR.pasm

QR.pas: QR.pasm
	@echo
	@echo "################################"
	@echo "##  46: Parrot asm -> Pascal  ##"
	@echo "################################"
	@echo
	parrot QR.pasm > QR.pas

QR.pl: QR.pas
	@echo
	@echo "##########################"
	@echo "##  47: Pascal -> Perl  ##"
	@echo "##########################"
	@echo
	fpc QR.pas
	./QR > QR.pl

QR.php: QR.pl
	@echo
	@echo "#######################"
	@echo "##  48: Perl -> PHP  ##"
	@echo "#######################"
	@echo
	perl QR.pl > QR.php

QR.pike: QR.php
	@echo
	@echo "#######################"
	@echo "##  49: PHP -> Pike  ##"
	@echo "#######################"
	@echo
	php QR.php > QR.pike

QR.prolog: QR.pike
	@echo
	@echo "##########################"
	@echo "##  50: Pike -> Prolog  ##"
	@echo "##########################"
	@echo
	pike QR.pike > QR.prolog

QR.py: QR.prolog
	@echo
	@echo "############################"
	@echo "##  51: Prolog -> Python  ##"
	@echo "############################"
	@echo
	swipl -q -t qr -f QR.prolog > QR.py

QR.R: QR.py
	@echo
	@echo "#######################"
	@echo "##  52: Python -> R  ##"
	@echo "#######################"
	@echo
	python QR.py > QR.R

QR.rexx: QR.R
	@echo
	@echo "#####################"
	@echo "##  53: R -> REXX  ##"
	@echo "#####################"
	@echo
	R --slave < QR.R > QR.rexx

QR2.rb: QR.rexx
	@echo
	@echo "########################"
	@echo "##  54: REXX -> Ruby  ##"
	@echo "########################"
	@echo
	rexx ./QR.rexx > QR2.rb

clean:
	@mv QR.rb quine-relay.rb
	rm -f qr QR qr.* QR.* QR2.rb *.class gst.im
	@mv quine-relay.rb QR.rb
