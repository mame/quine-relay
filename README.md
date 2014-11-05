# Quine Relay

### What this is

This is a Ruby program that generates
Scala program that generates
Scheme program that generates
...(through 51 languages in total)...
REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

(If you want to see the old 50-language version, see [50](https://github.com/mame/quine-relay/tree/50) branch.)

### Usage

#### 1. Install all interpreters/compilers.

You are (relatively) fortunate if you are using Ubuntu 14.10 "Utopic Unicorn".
First, you have to type the following apt-get command to install all of them.

    $ sudo apt-get install algol68g bash bf boo clisp clojure1.4 cmake \
      coffeescript f2c fp-compiler g++ gauche gawk gcc gforth gfortran ghc \
      gnat gnu-smalltalk gobjc golang groovy icont iconx intercal iverilog \
      jasmin-sable llvm lua5.2 make mono-devel mono-mcs nodejs ocaml octave \
      open-cobol openjdk-6-jdk parrot perl php5-cli pike7.8 python r-base \
      regina-rexx ruby2.0 scala swi-prolog tcl ucblogo valac

Then, you have to build the bundled interpreters.

    $ cd vendor
    $ make

If you are using Arch Linux, you might want to ask @xyproto to update
the [quine-relay-git](https://aur.archlinux.org/packages/quine-relay-git/) package :-)

You may find [instructions for other platforms in the wiki](https://github.com/mame/quine-relay/wiki/Installation).

If you are not using these Linux distributions, please find your way yourself.
If you could do it, please let me know.  Good luck.

#### 2. Run each program on each interpreter/compiler.

    $ ruby QR.rb > QR.scala
    $ scalac QR.scala && CLASSPATH=. scala QR > QR.scm
    $ gosh QR.scm > QR.bash
    $ bash QR.bash > QR.st
    $ gst QR.st > QR.tcl
    $ tclsh QR.tcl > QR.unl
    $ ruby vendor/unlambda.rb QR.unl > QR.vala
    $ valac QR.vala && ./QR > QR.v
    $ iverilog -o QR QR.v && ./QR -vcd-none > QR.ws
    $ ruby vendor/whitespace.rb QR.ws > qr.adb
    $ gnatmake qr.adb && ./qr > QR.a68
    $ a68g QR.a68 > QR.awk
    $ awk -f QR.awk > QR.boo
    $ booi QR.boo > QR.bf
    $ bf QR.bf > QR.c
    $ gcc -o QR QR.c && ./QR > QR.cpp
    $ g++ -o QR QR.cpp && ./QR > QR.cs
    $ mcs QR.cs && mono QR.exe > QR.clj
    $ clojure QR.clj > QR.cob
    $ cobc -O2 -x QR.cob && ./QR > QR.coffee
    $ coffee QR.coffee > QR.lisp
    $ clisp QR.lisp > QR.fs
    $ gforth QR.fs > QR.f
    $ f2c QR.f && gcc -o QR QR.c -L/usr/lib -lf2c && ./QR > QR.f90
    $ gfortran -o QR QR.f90 && ./QR > QR.go
    $ go run QR.go > QR.groovy
    $ groovy QR.groovy > QR.hs
    $ ghc QR.hs && ./QR > QR.icn
    $ icont -s QR.icn && ./QR > QR.i
    $ ick -bfO QR.i && ./QR > QR.j
    $ jasmin QR.j && CLASSPATH=. java QR > QR.java
    $ javac QR.java && CLASSPATH=. java QR > QR.ll
    $ llvm-as QR.ll && lli QR.bc > QR.logo
    $ logo QR.logo > QR.lol
    $ vendor/lci-*/lci QR.lol > QR.lua
    $ lua QR.lua > QR.makefile
    $ make -f QR.makefile > QR.il
    $ ilasm QR.il && mono QR.exe > QR.js
    $ nodejs QR.js > QR.m
    $ gcc -o QR QR.m && ./QR > QR.ml
    $ ocaml QR.ml > QR.octave
    $ octave -qf QR.octave > QR.pasm
    $ parrot QR.pasm > QR.pas
    $ fpc QR.pas && ./QR > QR.pl
    $ perl QR.pl > QR.php
    $ php QR.php > QR.pike
    $ pike QR.pike > QR.prolog
    $ swipl -q -t qr -f QR.prolog > QR.py
    $ python QR.py > QR.R
    $ R --slave < QR.R > QR.rexx
    $ rexx ./QR.rexx > QR2.rb

You will see that `QR.rb` is the same as `QR2.rb`.

    $ diff QR.rb QR2.rb

Alternatively, just type `make`.

    $ make

Note: It may require huge memory to compile some files.

### Tested interpreter/compiler versions

As I said above, I tested the program on Ubuntu.
It does not provide Unlambda and Whitespace interpreters,
so this repository includes my own implementations.
For other languages, I used the following deb packages:

\# |language     |ubuntu package |version
---|-------------|---------------|-----------------------------------
1  |Ruby         |ruby2.0        |2.0.0.484-1ubuntu2
2  |Scala        |scala          |2.9.2+dfsg-2
3  |Scheme       |gauche         |0.9.3.3-8ubuntu1
4  |Shell        |bash           |4.3-11ubuntu1
5  |Smalltalk    |gnu-smalltalk  |3.2.4-2
6  |Tcl          |tcl            |8.6.0+6ubuntu3
7  |Unlambda     |(none)         |-
8  |Vala         |valac          |0.22.1-0ubuntu1
9  |Verilog      |iverilog       |0.9.7-1
10 |Whitespace   |(none)         |-
11 |Ada          |gnat           |4.6ubuntu4
12 |ALGOL68      |algol68g       |2.4.1-1
13 |Awk          |gawk           |1:4.0.1+dfsg-2.1ubuntu2
14 |Boo          |boo            |0.9.5~git20110729.r1.202a430-2
15 |Brainfuck    |bf             |20041219ubuntu5
16 |C            |gcc            |4:4.8.2-1ubuntu6
17 |C++          |g++            |4:4.8.2-1ubuntu6
18 |C#           |mono-mcs       |3.2.8+dfsg-4ubuntu1
19 |Clojure      |clojure1.4     |1.4.0+dfsg-3
20 |Cobol        |open-cobol     |1.1-1build1
21 |CoffeeScript |coffeescript   |1.4.0-1
22 |Common Lisp  |clisp          |1:2.49-9ubuntu1
23 |Forth        |gforth         |0.7.0+ds2-0.1
24 |FORTRAN77    |f2c            |20100827-1
25 |Fortran90    |gfortran       |4:4.8.2-1ubuntu6
26 |Go           |golang         |2:1.2.1-2ubuntu1
27 |Groovy       |groovy         |2.0.0~beta2+isreally1.8.6-0ubuntu1
28 |Haskell      |ghc            |7.6.3-10
29 |Icon         |icont          |9.4.3-4.2ubuntu1
   |             |iconx          |9.4.3-4.2ubuntu1
30 |INTERCAL     |intercal       |29:0.29-3
31 |Jasmin       |jasmin-sable   |2.4.0-3
32 |Java         |openjdk-6-jdk  |6b33-1.13.5-1ubuntu0.14.04
33 |LLVM asm     |llvm           |1:3.4-0ubuntu1
34 |Logo         |ucblogo        |5.5-2.1
35 |LOLCODE      |(none)         |-
36 |Lua          |lua5.2         |5.2.3-1
37 |Makefile     |make           |3.81-8.2ubuntu3
38 |MSIL         |mono-devel     |3.2.8+dfsg-4ubuntu1
39 |NodeJS       |nodejs         |0.10.25~dfsg2-2ubuntu1
40 |Objective-C  |gobjc          |4:4.8.2-1ubuntu6
41 |OCaml        |ocaml          |4.01.0-3ubuntu3
42 |Octave       |octave         |3.8.1-1ubuntu1
43 |Parrot asm   |parrot         |5.9.0-1build1
44 |Pascal       |fp-compiler    |2.6.2-8
45 |Perl         |perl           |5.18.2-2ubuntu1
46 |PHP          |php5-cli       |5.5.9+dfsg-1ubuntu4.4
47 |Pike         |pike7.8        |7.8.700-7
48 |Prolog       |swi-prolog     |6.6.4-2ubuntu1
49 |Python       |python         |2.7.5-5ubuntu3
50 |R            |r-base         |3.0.2-1ubuntu1
51 |REXX         |regina-rexx    |3.6-2

Note: `CC=tcc ick -bfO` may be used to compile INTERCAL sources
with less memory.

### How to re-generate the source

    $ sudo apt-get install rake ruby-cairo ruby-rsvg2 ruby-gdk-pixbuf2 \
      optipng advancecomp
    $ cd src
    $ rake2.0 clobber
    $ rake2.0

### License

Copyright (c) 2013, 2014 Yusuke Endoh (@mametter), @hirekoke

MIT License

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
