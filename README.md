# Quine Relay

[![Build Status](https://travis-ci.org/mame/quine-relay.svg?branch=master)](https://travis-ci.org/mame/quine-relay)

## What this is

This is a Ruby program that generates
Scala program that generates
Scheme program that generates
...(through 100 languages in total)...
REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: langs.png

(If you want to see the old 50-language version, see [50](https://github.com/mame/quine-relay/tree/50) branch.)

## Usage

### Ubuntu

If you are using Ubuntu 16.04 "Xenial Xerus", you can perform the following steps.

#### 1. Install all interpreters/compilers.

First, you have to type the following apt-get command to install all of them.

    $ sudo apt-get install afnix algol68g aplus-fsf asymptote ats2-lang bash \
      bc bf bsdgames cduce clisp clojure1.6 cmake coffeescript dafny dc \
      ecere-dev elixir emacs24 erlang f2c fp-compiler fsharp g++ \
      gambas3-script gap gauche gawk gcc gdc genius gforth gfortran ghc \
      ghostscript gnat gnu-smalltalk gnuplot gobjc golang gpt gri groff \
      groovy haxe icont iconx intercal iverilog jasmin-sable jq julia \
      libgd-dev libpng-dev lisaac llvm lua5.3 make maxima minizinc mlton \
      mono-devel mono-mcs mono-vbnc nasm neko nickle nim ocaml octave \
      open-cobol openjdk-8-jdk pari-gp perl php-cli pike8.0 python r-base \
      ratfor regina-rexx rhino ruby scala scilab slsh squirrel3 swi-prolog \
      tcl valac xsltproc yorick zoem

Then, build the bundled interpreters.

    $ make -C vendor

#### 2. Run each program on each interpreter/compiler.

    $ ruby QR.rb > QR.scala
    $ scalac QR.scala && scala QR > QR.scm
    $ gosh QR.scm > QR.sci
    $ scilab -nw -nb -f QR.sci > QR.bash
    $ bash QR.bash > QR.sl
    $ slsh QR.sl > QR.st
    $ gst QR.st > QR.nut
    $ squirrel3 QR.nut > QR.sml
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
    $ patscc -o QR QR.dats && ./QR > QR.awk
    $ awk -f QR.awk > QR.bc
    $ BC_LINE_LENGTH=4000000 bc -q QR.bc > QR.bef
    $ cfunge QR.bef > QR.Blc
    $ ruby vendor/blc.rb < QR.Blc > QR.bf
    $ bf QR.bf > QR.c
    $ gcc -o QR QR.c && ./QR > QR.cpp
    $ g++ -o QR QR.cpp && ./QR > QR.cs
    $ mcs QR.cs && mono QR.exe > QR.cd
    $ cduce QR.cd > QR.chef
    $ PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl &&
      perl QR.chef.pl > QR.clj
    $ clojure QR.clj > QR.cob
    $ cobc -O2 -x QR.cob && ./QR > QR.coffee
    $ coffee QR.coffee > QR.lisp
    $ clisp QR.lisp > QR.d
    $ gdc -o QR QR.d && ./QR > QR.dfy
    $ dafny QR.dfy && mono QR.exe > QR.dc
    $ dc QR.dc > QR.ec
    $ ecp -c QR.ec -o QR.sym && ecc -c QR.ec -o QR.c && ecs -console QR.sym QR.imp -o QR.main.ec &&
      ecp -c QR.main.ec -o QR.main.sym && ecc -c QR.main.ec -o QR.main.c &&
      gcc -o QR QR.c QR.main.c -lecereCOM && ./QR > QR.exs
    $ elixir QR.exs > QR.el
    $ emacs -Q --script QR.el > QR.erl
    $ escript QR.erl > QR.fsx
    $ fsharpc QR.fsx -o QR.exe && mono QR.exe > QR.false
    $ ruby vendor/false.rb QR.false > QR.fs
    $ gforth QR.fs > QR.f
    $ gfortran -o QR QR.f && ./QR > QR.f90
    $ gfortran -o QR QR.f90 && ./QR > QR.gbs
    $ gbs3 QR.gbs > QR.g
    $ gap -q QR.g > QR.gel
    $ genius QR.gel > QR.plt
    $ gnuplot QR.plt > QR.go
    $ go run QR.go > QR.gpt
    $ gpt -o QR QR.gpt && ./QR > QR.gri
    $ gri QR.gri > QR.groovy
    $ groovy QR.groovy > QR.hs
    $ ghc QR.hs && ./QR > QR.hx
    $ haxe -main QR -neko QR.n && neko QR.n > QR.icn
    $ icont -s QR.icn && ./QR > QR.i
    $ ick -bfOc QR.i && gcc -static QR.c -I /usr/include/ick-* -o QR -lick &&
      ./QR > QR.j
    $ jasmin QR.j && java QR > QR.java
    $ javac QR.java && java QR > QR.js
    $ rhino QR.js > QR.jq
    $ jq -r -n -f QR.jq > QR.jl
    $ julia QR.jl > QR.lazy
    $ lazyk QR.lazy > qr.li
    $ lisaac qr.li && ./qr > QR.ll
    $ llvm-as QR.ll && lli QR.bc > QR.lol
    $ lci QR.lol > QR.lua
    $ lua5.3 QR.lua > QR.mk
    $ make -f QR.mk > QR.mac
    $ maxima -q --init-mac=QR.mac > QR.mzn
    $ mzn2fzn QR.mzn && fzn-gecode QR.fzn | solns2out --soln-sep '' QR.ozn > QR.il
    $ ilasm QR.il && mono QR.exe > QR.asm
    $ nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > QR.neko
    $ nekoc QR.neko && neko QR.n > QR.5c
    $ nickle QR.5c > QR.nim
    $ nim c QR.nim && ./QR > QR.m
    $ gcc -o QR QR.m && ./QR > QR.ml
    $ ocaml QR.ml > QR.octave
    $ octave -qf QR.octave > QR.ook
    $ ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf && bf QR.ook.bf > QR.gp
    $ gp -f -q QR.gp > QR.pas
    $ fpc QR.pas && ./QR > QR.pl
    $ perl QR.pl > QR.php
    $ php QR.php > QR.png
    $ npiet QR.png > QR.pike
    $ pike QR.pike > QR.ps
    $ gs -dNODISPLAY -q QR.ps > QR.ppt
    $ ppt -d < QR.ppt > QR.prolog
    $ swipl -q -t qr -f QR.prolog > QR.py
    $ python QR.py > QR.R
    $ R --slave -f QR.R > QR.ratfor
    $ ratfor -o QR.ratfor.f QR.ratfor && gfortran -o QR QR.ratfor.f &&
      ./QR > QR.rexx
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

\#  |language           |ubuntu package |version
----|-------------------|---------------|-------------------------
1   |Ruby               |ruby           |1:2.3.0+4
2   |Scala              |scala          |2.11.8-1
3   |Scheme             |gauche         |0.9.4-6
4   |Scilab             |scilab         |5.5.2-2ubuntu5
5   |Shell (bash)       |bash           |4.3-15ubuntu1
6   |S-Lang             |slsh           |2.3.0-2.3ubuntu1
7   |Smalltalk          |gnu-smalltalk  |3.2.5-1build3
8   |Squirrel3          |squirrel3      |3.1-4
9   |Standard ML        |mlton          |20100608-5.1ubuntu1
10  |Subleq             |*N/A*          |-
11  |Tcl                |tcl            |8.6.0+9
12  |Thue               |*N/A*          |-
13  |Unlambda           |*N/A*          |-
14  |Vala               |valac          |0.32.1-1
15  |Verilog            |iverilog       |10.1-0.1build1
16  |Visual Basic       |mono-vbnc      |4.0.1-1
17  |Whitespace         |*N/A*          |-
18  |XSLT               |xsltproc       |1.1.29-1
19  |Yorick             |yorick         |2.2.04+dfsg1-6
20  |Zoem               |zoem           |11-166-1.2
21  |A+                 |aplus-fsf      |4.22.1-9
22  |Ada                |gnat           |6.1ubuntu2
23  |AFNIX              |afnix          |2.6.3-1
24  |ALGOL68            |algol68g       |2.8-2
25  |Ante               |*N/A*          |-
26  |Asymptote          |asymptote      |2.38-2build1
27  |ATS                |ats2-lang      |0.2.9-1
28  |Awk                |gawk           |1:4.1.3+dfsg-0.1build1
29  |bc                 |bc             |1.06.95-9build2
30  |Befunge            |*N/A*          |-
31  |BLC8               |*N/A*          |-
32  |Brainfuck          |bf             |20041219ubuntu5
33  |C                  |gcc            |4:6.1.1-1ubuntu2
34  |C++                |g++            |4:6.1.1-1ubuntu2
35  |C#                 |mono-mcs       |4.2.1.102+dfsg2-7ubuntu4
36  |CDuce              |cduce          |0.6.0-5build2
37  |Chef               |*N/A*          |-
38  |Clojure            |clojure1.6     |1.6.0+dfsg-2
39  |Cobol              |open-cobol     |1.1-2
40  |CoffeeScript       |coffeescript   |1.10.0~dfsg-1
41  |Common Lisp        |clisp          |1:2.49-9ubuntu1
42  |D                  |gdc            |4:6.1.1-1ubuntu2
43  |Dafny              |dafny          |1.9.7-1
44  |dc                 |dc             |1.06.95-9build2
45  |eC                 |ecere-dev      |0.44.15-1
46  |Elixir             |elixir         |1.2.6-1
47  |Emacs Lisp         |emacs24        |24.5+1-6ubuntu3
48  |Erlang             |erlang         |1:18.3.4.4+dfsg-1ubuntu2
49  |F#                 |fsharp         |4.0.0.4+dfsg2-2
50  |FALSE              |*N/A*          |-
51  |Forth              |gforth         |0.7.3+dfsg-3
52  |FORTRAN77          |f2c            |20100827-3
53  |Fortran90          |gfortran       |4:6.1.1-1ubuntu2
54  |Gambas script      |gambas3-script |3.8.4-6ubuntu1
55  |GAP                |gap            |4r8p4-1build1
56  |GEL (Genius)       |genius         |1.0.21-1build1
57  |Gnuplot            |gnuplot        |5.0.4+dfsg1-3
58  |Go                 |golang         |2:1.6.1+1ubuntu2
59  |G-Portugol         |gpt            |1.1-2ubuntu2
60  |Gri                |gri            |2.12.23-10
61  |Groovy             |groovy         |2.4.7-2
62  |Haskell            |ghc            |7.10.3-9ubuntu1
63  |Haxe               |haxe           |1:3.2.1+dfsg-1build3
64  |Icon               |icont          |9.4.3-4.2ubuntu1
    |                   |iconx          |9.4.3-4.2ubuntu1
65  |INTERCAL           |intercal       |30:0.30-1
66  |Jasmin             |jasmin-sable   |2.5.0-1
67  |Java               |openjdk-8-jdk  |8u102-b14.1-2
68  |JavaScript         |rhino          |1.7R4-3
69  |Jq                 |jq             |1.5+dfsg-1
70  |Julia              |julia          |0.4.7-1
71  |Lazy K             |*N/A*          |-
72  |Lisaac             |lisaac         |1:0.39~rc1-3
73  |LLVM asm           |llvm           |1:3.8-34
74  |LOLCODE            |*N/A*          |-
75  |Lua                |lua5.3         |5.3.1-1.1build1
76  |Makefile           |make           |4.1-9
77  |Maxima             |maxima         |5.37.2-8
78  |MiniZinc           |minizinc       |2.0.14+dfsg1-1
79  |MSIL               |mono-devel     |4.2.1.102+dfsg2-7ubuntu4
80  |NASM               |nasm           |2.12.01-1
81  |Neko               |neko           |2.1.0-3
82  |Nickle             |nickle         |2.77-1build1
83  |Nim                |nim            |0.13.0-1
84  |Objective-C        |gobjc          |4:6.1.1-1ubuntu2
85  |OCaml              |ocaml          |4.02.3-6ubuntu2
86  |Octave             |octave         |4.0.3-2ubuntu3
87  |Ook!               |*N/A*          |-
88  |PARI/GP            |pari-gp        |2.7.6-1build2
89  |Pascal             |fp-compiler    |3.0.0+dfsg-8
90  |Perl               |perl           |5.22.2-3
91  |PHP                |php-cli        |1:7.0+44
92  |Piet               |*N/A*          |-
93  |Pike               |pike8.0        |8.0.276-1
94  |PostScript         |ghostscript    |9.19~dfsg+1-0ubuntu6
95  |PPT (Punched tape) |bsdgames       |2.17-24
96  |Prolog             |swi-prolog     |7.2.3+dfsg-1build2
97  |Python             |python         |2.7.11-2
98  |R                  |r-base         |3.3.1-1build1
99  |Ratfor             |ratfor         |1.0-15
100 |REXX               |regina-rexx    |3.6-2

Note that some languages are not available in Ubuntu (marked as *N/A*).
This repository includes their implementations in `vendor/`.
See also `vendor/README` in detail.


## Frequently asked questions

### Q. Why?

A. [Take your pick](https://github.com/mame/quine-relay/issues/11).

### Q. How?

A. Good news: I published a book, ["The world of obfuscated, esoteric, artistic programming"](http://gihyo.jp/book/2015/978-4-7741-7643-7).
It explains how to write a quine, an ascii-art quine, and a uroboros quine like this quine-relay.
You can buy my book on [amazon.co.jp](http://www.amazon.co.jp/dp/4774176435).

(It also contains my almost all (about forty) works, including
[alphabet-only Ruby program](http://www.slideshare.net/mametter/ruby-esoteric-obfuscated-ruby-programming-5088683),
[radiation-hardened quine](https://github.com/mame/radiation-hardened-quine),
etc., and explains many techniques to write such programs.)

Bad news: It is written in Japanese.
I hope you could translate it to English <strike>and help me earn royalties</strike>.

### Q. Language XXX is missing!

A. See [the criteria for language inclusion][criteria] in detail.  (In short: please create a deb package and contribute it to Ubuntu.)

[criteria]: https://github.com/mame/quine-relay/wiki/Criteria-for-language-inclusion

### Q. Does it really work?

A. [![Build Status](https://travis-ci.org/mame/quine-relay.svg?branch=master)](https://travis-ci.org/mame/quine-relay)

### Q. How long did it take you?

A. [Do you try to cross the world line?](https://github.com/mame/quine-relay/issues/60)

### Q. The code does not fit into my display!

A. [Here you go][thumbnail].

[thumbnail]: thumbnail.png

### Q. How was the code generated?

A.

    $ sudo apt-get install rake ruby-cairo ruby-rsvg2 ruby-gdk-pixbuf2 \
      optipng advancecomp ruby-chunky-png
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

Copyright (c) 2013, 2014, 2015, 2016 Yusuke Endoh (@mametter), @hirekoke

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
