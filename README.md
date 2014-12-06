# Quine Relay

[![Build Status](https://travis-ci.org/mame/quine-relay.svg?branch=master)](https://travis-ci.org/mame/quine-relay)

## What this is

This is a Ruby program that generates
Scala program that generates
Scheme program that generates
...(through 82 languages in total)...
REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

(If you want to see the old 50-language version, see [50](https://github.com/mame/quine-relay/tree/50) branch.)

## Usage

### Ubuntu

#### 1. Install all interpreters/compilers.

If you are using Ubuntu 14.10 "Utopic Unicorn", you can perform the following steps:

First, you have to type the following apt-get command to install all of them.

    $ sudo apt-get install afnix algol68g aplus-fsf asymptote \
      ats-lang-anairiats bash bc bf boo bsdgames cduce clisp clojure1.4 \
      cmake coffeescript f2c fp-compiler g++ gauche gawk gcc gdc gforth \
      gfortran ghc ghostscript gnat gnu-smalltalk gobjc golang groff groovy \
      icont iconx intercal iverilog jasmin-sable libgd2-xpm-dev libpng12-dev \
      llvm lua5.2 make maxima mlton mono-devel mono-mcs mono-vbnc nasm neko \
      nickle ocaml octave open-cobol openjdk-6-jdk pari-gp parrot perl \
      php5-cli pike7.8 python r-base ratfor regina-rexx rhino ruby2.0 scala \
      scilab slsh spl-core swi-prolog tcl ucblogo valac xsltproc yorick zoem

Then, you have to build the bundled interpreters.

    $ make -C vendor

To run it on Ubuntu 12.04 LTS, you might want to refer to `.travis.yml`.

#### 2. Run each program on each interpreter/compiler.

    $ ruby QR.rb > QR.scala
    $ scalac QR.scala && CLASSPATH=. scala QR > QR.scm
    $ gosh QR.scm > QR.sci
    $ scilab -nw -nb -f QR.sci > QR.bash
    $ bash QR.bash > QR.sl
    $ slsh QR.sl > QR.st
    $ gst QR.st > QR.spl
    $ splrun QR.spl > QR.sml
    $ mlton @MLton fixed-heap 200M -- QR.sml && ./QR > QR.sq
    $ ruby vendor/subleq.rb QR.sq > QR.tcl
    $ tclsh QR.tcl > QR.t
    $ ruby vendor/thue.rb QR.t > QR.unl
    $ ruby vendor/unlambda.rb QR.unl > QR.vala
    $ valac QR.vala && ./QR > QR.v
    $ iverilog -o QR QR.v && ./QR -vcd-none > QR.vb
    $ vbnc QR.vb && mono ./QR.exe > QR.ws
    $ ruby vendor/whitespace.rb QR.ws > QR.xslt
    $ xsltproc QR.xslt > QR.yorick
    $ yorick -batch QR.yorick > QR.azm
    $ zoem -i QR.azm > QR.+
    $ a+ QR.+ > qr.adb
    $ gnatmake qr.adb && ./qr > QR.als
    $ axi QR.als > QR.a68
    $ a68g QR.a68 > QR.ante
    $ ruby vendor/ante.rb QR.ante > QR.asy
    $ asy QR.asy > QR.dats
    $ atscc -o QR QR.dats && ./QR > QR.awk
    $ awk -f QR.awk > QR.bc
    $ BC_LINE_LENGTH=3000000 bc -q QR.bc > QR.bef
    $ cfunge QR.bef > QR.Blc
    $ ruby vendor/blc.rb < QR.Blc > QR.boo
    $ booi QR.boo > QR.bf
    $ bf QR.bf > QR.c
    $ gcc -o QR QR.c && ./QR > QR.cpp
    $ g++ -o QR QR.cpp && ./QR > QR.cs
    $ mcs QR.cs && mono QR.exe > QR.cd
    $ cduce QR.cd > QR.chef
    $ PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl && perl QR.chef.pl > QR.clj
    $ clojure QR.clj > QR.cob
    $ cobc -O2 -x QR.cob && ./QR > QR.coffee
    $ coffee QR.coffee > QR.lisp
    $ clisp QR.lisp > QR.d
    $ gdc -o QR QR.d && ./QR > QR.fs
    $ gforth QR.fs > QR.f
    $ f2c QR.f && gcc -o QR QR.c -L/usr/lib -lf2c -lm && ./QR > QR.f90
    $ gfortran -o QR QR.f90 && ./QR > QR.go
    $ go run QR.go > QR.groovy
    $ groovy QR.groovy > QR.hs
    $ ghc QR.hs && ./QR > QR.icn
    $ icont -s QR.icn && ./QR > QR.i
    $ ick -bfO QR.i && ./QR > QR.j
    $ jasmin QR.j && CLASSPATH=. java QR > QR.java
    $ javac QR.java && CLASSPATH=. java QR > QR.js
    $ rhino QR.js > QR.ll
    $ mv QR.bc QR.bc.bak && llvm-as QR.ll && lli QR.bc > QR.logo && mv QR.bc.bak QR.bc
    $ logo QR.logo > QR.lol
    $ lci QR.lol > QR.lua
    $ lua QR.lua > QR.mk
    $ make -f QR.mk > QR.mac
    $ maxima -q --init-mac=QR.mac > QR.il
    $ ilasm QR.il && mono QR.exe > QR.asm
    $ nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > QR.neko
    $ nekoc QR.neko && neko QR.n > QR.5c
    $ nickle QR.5c > QR.m
    $ gcc -o QR QR.m && ./QR > QR.ml
    $ ocaml QR.ml > QR.octave
    $ octave -qf QR.octave > QR.ook
    $ ruby vendor/ook.rb QR.ook > QR.gp
    $ gp -f -q QR.gp > QR.pasm
    $ parrot QR.pasm > QR.pas
    $ fpc QR.pas && ./QR > QR.pl
    $ perl QR.pl > QR.php
    $ php QR.php > QR.png
    $ npiet QR.png > QR.pike
    $ pike QR.pike > QR.ps
    $ gs -dNODISPLAY -q QR.ps > QR.ppt
    $ ppt -d < QR.ppt > QR.prolog
    $ swipl -q -t qr -f QR.prolog > QR.py
    $ python QR.py > QR.R
    $ R --slave -f QR.R > QR.r
    $ ratfor -o QR.r.f QR.r && gfortran -o QR QR.r.f && ./QR > QR.rexx
    $ rexx ./QR.rexx > QR2.rb

You will see that `QR.rb` is the same as `QR2.rb`.

    $ diff QR.rb QR2.rb

Alternatively, just type `make`.

    $ make

Note: It may require huge memory to compile some files.

### Arch Linux

Just install [quine-relay-git](https://aur.archlinux.org/packages/quine-relay-git/) from AUR and run `quine-relay`.
Report any problems as comments to the AUR package or to the respective packages, if one of the many compilers should have issues.

### Other platforms

You may find [instructions for other platforms in the wiki](https://github.com/mame/quine-relay/wiki/Installation).

If you are not using these Linux distributions, please find your way yourself.
If you could do it, please let me know.  Good luck.

## Tested interpreter/compiler versions

I used the following Ubuntu deb packages to test this program.

\# |language           |ubuntu package     |version
---|-------------------|-------------------|-----------------------------------
1  |Ruby               |ruby2.0            |2.0.0.484+really457-3ubuntu1.2
2  |Scala              |scala              |2.9.2+dfsg-2
3  |Scheme             |gauche             |0.9.3.3-8ubuntu1
4  |Scilab             |scilab             |5.5.0-3ubuntu2
5  |Shell (bash)       |bash               |4.3-11ubuntu1
6  |S-Lang             |slsh               |2.2.4-17ubuntu1
7  |Smalltalk          |gnu-smalltalk      |3.2.4-2.1
8  |SPL                |spl-core           |1.0~pre6-4ubuntu1
9  |Standard ML        |mlton              |20100608-5.1
10 |Subleq             |*N/A*              |-
11 |Tcl                |tcl                |8.6.0+6ubuntu3
12 |Thue               |*N/A*              |-
13 |Unlambda           |*N/A*              |-
14 |Vala               |valac              |0.24.0-6
15 |Verilog            |iverilog           |0.9.7-1
16 |Visual Basic       |mono-vbnc          |3.0~pre20130627.4dcc70f-1
17 |Whitespace         |*N/A*              |-
18 |XSLT               |xsltproc           |1.1.28-2build1
19 |Yorick             |yorick             |2.2.03+dfsg-3ubuntu1
20 |Zoem               |zoem               |11-166-1ubuntu1
21 |A+                 |aplus-fsf          |4.22.1-6
22 |Ada                |gnat               |4.9ubuntu1
23 |AFNIX              |afnix              |2.2.0-2ubuntu1
24 |ALGOL68            |algol68g           |2.4.1-1
25 |Ante               |*N/A*              |-
26 |Asymptote          |asymptote          |2.15-2build2
27 |ATS                |ats-lang-anairiats |0.2.11-1
28 |Awk                |gawk               |1:4.1.1+dfsg-1
29 |bc                 |bc                 |1.06.95-9
30 |Befunge            |*N/A*              |-
31 |BLC8               |*N/A*              |-
32 |Boo                |boo                |0.9.5~git20110729.r1.202a430-2
33 |Brainfuck          |bf                 |20041219ubuntu5
34 |C                  |gcc                |4:4.9.1-4ubuntu2
35 |C++                |g++                |4:4.9.1-4ubuntu2
36 |C#                 |mono-mcs           |3.2.8+dfsg-4ubuntu2
37 |CDuce              |cduce              |0.6.0-1
38 |Chef               |*N/A*              |-
39 |Clojure            |clojure1.4         |1.4.0+dfsg-3
40 |Cobol              |open-cobol         |1.1-1build1
41 |CoffeeScript       |coffeescript       |1.4.0-1
42 |Common Lisp        |clisp              |1:2.49-9ubuntu1
43 |D                  |gdc                |4.9.1-4ubuntu2
44 |Forth              |gforth             |0.7.2+dfsg1-1
45 |FORTRAN77          |f2c                |20100827-1
46 |Fortran90          |gfortran           |4:4.9.1-4ubuntu2
47 |Go                 |golang             |2:1.2.1-2ubuntu1
48 |Groovy             |groovy             |2.0.0~beta2+isreally1.8.6-4ubuntu1
49 |Haskell            |ghc                |7.6.3-19
50 |Icon               |icont              |9.4.3-4.2ubuntu1
   |                   |iconx              |9.4.3-4.2ubuntu1
51 |INTERCAL           |intercal           |29:0.29-3
52 |Jasmin             |jasmin-sable       |2.4.0-4
53 |Java               |openjdk-6-jdk      |6b33-1.13.5-1ubuntu1
54 |JavaScript         |rhino              |1.7R4-3
55 |LLVM asm           |llvm               |1:3.5-23ubuntu1
56 |Logo               |ucblogo            |5.5-2.1
57 |LOLCODE            |*N/A*              |-
58 |Lua                |lua5.2             |5.2.3-1
59 |Makefile           |make               |4.0-8
60 |Maxima             |maxima             |5.33.0-14
61 |MSIL               |mono-devel         |3.2.8+dfsg-4ubuntu2
62 |NASM               |nasm               |2.11-1
63 |Neko               |neko               |2.0.0-3
64 |Nickle             |nickle             |2.77-1
65 |Objective-C        |gobjc              |4:4.9.1-4ubuntu2
66 |OCaml              |ocaml              |4.01.0-4ubuntu1
67 |Octave             |octave             |3.8.1-1ubuntu1
68 |Ook!               |*N/A*              |-
69 |PARI/GP            |pari-gp            |2.7.1-1
70 |Parrot asm         |parrot             |6.3.0-1
71 |Pascal             |fp-compiler        |2.6.4+dfsg-3
72 |Perl               |perl               |5.20.1-1
73 |PHP                |php5-cli           |5.5.12+dfsg-2ubuntu4.1
74 |Piet               |*N/A*              |-
75 |Pike               |pike7.8            |7.8.866-3
76 |PostScript         |ghostscript        |9.14~dfsg-0ubuntu3
77 |PPT (Punched tape) |bsdgames           |2.17-22
78 |Prolog             |swi-prolog         |6.6.4-2ubuntu1
79 |Python             |python             |2.7.8-1
80 |R                  |r-base             |3.1.1-1
81 |Ratfor             |ratfor             |1.0-15
82 |REXX               |regina-rexx        |3.6-2

Note that some languages are not available in Ubuntu (marked as *N/A*).
This repository includes their implementations in `vendor/`.
See also `vendor/README` in detail.

## How to re-generate the source

    $ sudo apt-get install rake ruby-cairo ruby-rsvg2 ruby-gdk-pixbuf2 \
      optipng advancecomp
    $ cd src
    $ rake2.0 clobber
    $ rake2.0

## License

The MIT License applies to all resources
*except* the files in the `vendor/` directory.

The files in the `vendor/` directory are from third-parties
and are distributed under different licenses.
See `vendor/README` in detail.

---

The MIT License (MIT)

Copyright (c) 2013, 2014 Yusuke Endoh (@mametter), @hirekoke

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
