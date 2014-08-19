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
	@echo "#####################"
	@echo "##  Ruby -> Scala  ##"
	@echo "#####################"
	@echo
	ruby QR.rb > QR.scala

QR.scm: QR.scala
	@echo
	@echo "#######################"
	@echo "##  Scala -> Scheme  ##"
	@echo "#######################"
	@echo
	scalac QR.scala
	CLASSPATH=. scala QR > QR.scm

QR.bash: QR.scm
	@echo
	@echo "#######################"
	@echo "##  Scheme -> Shell  ##"
	@echo "#######################"
	@echo
	$(SCHEME) QR.scm > QR.bash

QR.st: QR.bash
	@echo
	@echo "##########################"
	@echo "##  Shell -> Smalltalk  ##"
	@echo "##########################"
	@echo
	bash QR.bash > QR.st

QR.tcl: QR.st
	@echo
	@echo "########################"
	@echo "##  Smalltalk -> Tcl  ##"
	@echo "########################"
	@echo
	gst QR.st > QR.tcl

QR.unl: QR.tcl
	@echo
	@echo "#######################"
	@echo "##  Tcl -> Unlambda  ##"
	@echo "#######################"
	@echo
	tclsh QR.tcl > QR.unl

QR.vala: QR.unl
	@echo
	@echo "########################"
	@echo "##  Unlambda -> Vala  ##"
	@echo "########################"
	@echo
	ruby unlambda.rb QR.unl > QR.vala

QR.v: QR.vala
	@echo
	@echo "#######################"
	@echo "##  Vala -> Verilog  ##"
	@echo "#######################"
	@echo
	valac QR.vala
	./QR > QR.v

QR.ws: QR.v
	@echo
	@echo "#############################"
	@echo "##  Verilog -> Whitespace  ##"
	@echo "#############################"
	@echo
	iverilog -o QR QR.v
	./QR -vcd-none > QR.ws

qr.adb: QR.ws
	@echo
	@echo "#########################"
	@echo "##  Whitespace -> Ada  ##"
	@echo "#########################"
	@echo
	ruby whitespace.rb QR.ws > qr.adb

QR.a68: qr.adb
	@echo
	@echo "######################"
	@echo "##  Ada -> ALGOL68  ##"
	@echo "######################"
	@echo
	gnatmake qr.adb
	./qr > QR.a68

QR.awk: QR.a68
	@echo
	@echo "######################"
	@echo "##  ALGOL68 -> Awk  ##"
	@echo "######################"
	@echo
	a68g QR.a68 > QR.awk

QR.boo: QR.awk
	@echo
	@echo "##################"
	@echo "##  Awk -> Boo  ##"
	@echo "##################"
	@echo
	awk -f QR.awk > QR.boo

QR.bf: QR.boo
	@echo
	@echo "########################"
	@echo "##  Boo -> Brainfuck  ##"
	@echo "########################"
	@echo
	booi QR.boo > QR.bf

QR.c: QR.bf
	@echo
	@echo "######################"
	@echo "##  Brainfuck -> C  ##"
	@echo "######################"
	@echo
	$(BF) QR.bf > QR.c

QR.cpp: QR.c
	@echo
	@echo "################"
	@echo "##  C -> C++  ##"
	@echo "################"
	@echo
	${CC} -o QR QR.c
	./QR > QR.cpp

QR.cs: QR.cpp
	@echo
	@echo "#################"
	@echo "##  C++ -> C#  ##"
	@echo "#################"
	@echo
	${CXX} -o QR QR.cpp
	./QR > QR.cs

QR.clj: QR.cs
	@echo
	@echo "#####################"
	@echo "##  C# -> Clojure  ##"
	@echo "#####################"
	@echo
	mcs QR.cs
	mono QR.exe > QR.clj

QR.cob: QR.clj
	@echo
	@echo "########################"
	@echo "##  Clojure -> Cobol  ##"
	@echo "########################"
	@echo
	clojure QR.clj > QR.cob

QR.coffee: QR.cob
	@echo
	@echo "#############################"
	@echo "##  Cobol -> CoffeeScript  ##"
	@echo "#############################"
	@echo
	cobc -O2 -x QR.cob
	./QR > QR.coffee

QR.lisp: QR.coffee
	@echo
	@echo "##################################"
	@echo "##  CoffeeScript -> CommonLisp  ##"
	@echo "##################################"
	@echo
	coffee QR.coffee > QR.lisp

QR.fs: QR.lisp
	@echo
	@echo "###########################"
	@echo "##  CommonLisp -> Forth  ##"
	@echo "###########################"
	@echo
	clisp QR.lisp > QR.fs

QR.f: QR.fs
	@echo
	@echo "##########################"
	@echo "##  Forth -> FORTRAN77  ##"
	@echo "##########################"
	@echo
	gforth QR.fs > QR.f

QR.f90: QR.f
	@echo
	@echo "##############################"
	@echo "##  FORTRAN77 -> Fortran90  ##"
	@echo "##############################"
	@echo
	mv QR.c QR.c.bak
	f2c QR.f
	${CC} -o QR QR.c -L/usr/lib -lf2c
	mv QR.c.bak QR.c
	./QR > QR.f90

QR.go: QR.f90
	@echo
	@echo "#######################"
	@echo "##  Fortran90 -> Go  ##"
	@echo "#######################"
	@echo
	gfortran -o QR QR.f90
	./QR > QR.go

QR.groovy: QR.go
	@echo
	@echo "####################"
	@echo "##  Go -> Groovy  ##"
	@echo "####################"
	@echo
	go run QR.go > QR.groovy

QR.hs: QR.groovy
	@echo
	@echo "#########################"
	@echo "##  Groovy -> Haskell  ##"
	@echo "#########################"
	@echo
	groovy QR.groovy > QR.hs

QR.icn: QR.hs
	@echo
	@echo "#######################"
	@echo "##  Haskell -> Icon  ##"
	@echo "#######################"
	@echo
	runghc QR.hs > QR.icn

QR.i: QR.icn
	@echo
	@echo "########################"
	@echo "##  Icon -> INTERCAL  ##"
	@echo "########################"
	@echo
	icont -s QR.icn
	./QR > QR.i

QR.j: QR.i
	@echo
	@echo "##########################"
	@echo "##  INTERCAL -> Jasmin  ##"
	@echo "##########################"
	@echo
	mv QR.c QR.c.bak
	ick -bfO QR.i
	mv QR.c.bak QR.c
	./QR > QR.j

QR.java: QR.j
	@echo
	@echo "######################"
	@echo "##  Jasmin -> Java  ##"
	@echo "######################"
	@echo
	jasmin QR.j
	CLASSPATH=. java QR > QR.java

QR.ll: QR.java
	@echo
	@echo "########################"
	@echo "##  Java -> LLVM asm  ##"
	@echo "########################"
	@echo
	javac QR.java
	CLASSPATH=. java QR > QR.ll

QR.logo: QR.ll
	@echo
	@echo "########################"
	@echo "##  LLVM asm -> Logo  ##"
	@echo "########################"
	@echo
	llvm-as QR.ll
	lli QR.bc > QR.logo

QR.lua: QR.logo
	@echo
	@echo "###################"
	@echo "##  Logo -> Lua  ##"
	@echo "###################"
	@echo
	logo QR.logo > QR.lua

QR.makefile: QR.lua
	@echo
	@echo "#######################"
	@echo "##  Lua -> Makefile  ##"
	@echo "#######################"
	@echo
	lua QR.lua > QR.makefile

QR.il: QR.makefile
	@echo
	@echo "########################"
	@echo "##  Makefile -> MSIL  ##"
	@echo "########################"
	@echo
	make -f QR.makefile > QR.il

QR.js: QR.il
	@echo
	@echo "######################"
	@echo "##  MSIL -> NodeJS  ##"
	@echo "######################"
	@echo
	ilasm QR.il
	mono QR.exe > QR.js

QR.m: QR.js
	@echo
	@echo "#############################"
	@echo "##  NodeJS -> Objective-C  ##"
	@echo "#############################"
	@echo
	$(NODE) QR.js > QR.m

QR.ml: QR.m
	@echo
	@echo "############################"
	@echo "##  Objective-C -> OCaml  ##"
	@echo "############################"
	@echo
	gcc -o QR QR.m
	./QR > QR.ml

QR.octave: QR.ml
	@echo
	@echo "#######################"
	@echo "##  OCaml -> Octave  ##"
	@echo "#######################"
	@echo
	ocaml QR.ml > QR.octave

QR.pasm: QR.octave
	@echo
	@echo "############################"
	@echo "##  Octave -> Parrot asm  ##"
	@echo "############################"
	@echo
	octave -qf QR.octave > QR.pasm

QR.pas: QR.pasm
	@echo
	@echo "############################"
	@echo "##  Parrot asm -> Pascal  ##"
	@echo "############################"
	@echo
	parrot QR.pasm > QR.pas

QR.pl: QR.pas
	@echo
	@echo "######################"
	@echo "##  Pascal -> Perl  ##"
	@echo "######################"
	@echo
	fpc QR.pas
	./QR > QR.pl

QR.php: QR.pl
	@echo
	@echo "###################"
	@echo "##  Perl -> PHP  ##"
	@echo "###################"
	@echo
	perl QR.pl > QR.php

QR.pike: QR.php
	@echo
	@echo "###################"
	@echo "##  PHP -> Pike  ##"
	@echo "###################"
	@echo
	php QR.php > QR.pike

QR.prolog: QR.pike
	@echo
	@echo "######################"
	@echo "##  Pike -> Prolog  ##"
	@echo "######################"
	@echo
	pike QR.pike > QR.prolog

QR.py: QR.prolog
	@echo
	@echo "########################"
	@echo "##  Prolog -> Python  ##"
	@echo "########################"
	@echo
	swipl -q -t qr -f QR.prolog > QR.py

QR.R: QR.py
	@echo
	@echo "###################"
	@echo "##  Python -> R  ##"
	@echo "###################"
	@echo
	python QR.py > QR.R

QR.rexx: QR.R
	@echo
	@echo "#################"
	@echo "##  R -> REXX  ##"
	@echo "#################"
	@echo
	R --slave < QR.R > QR.rexx

QR2.rb: QR.rexx
	@echo
	@echo "####################"
	@echo "##  REXX -> Ruby  ##"
	@echo "####################"
	@echo
	rexx ./QR.rexx > QR2.rb

clean:
	@mv QR.rb quine-relay.rb
	rm -f qr QR qr.* QR.* QR2.rb *.class
	@mv quine-relay.rb QR.rb
