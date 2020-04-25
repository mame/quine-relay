# Quine Relay

[![Build Status](https://travis-ci.org/mame/quine-relay.svg?branch=laboratory)](https://travis-ci.org/mame/quine-relay)
[:heart:](https://github.com/sponsors/mame)

## What this is

This is a Ruby program that generates
Rust program that generates
Scala program that generates
...(through 128 languages in total)...
REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: langs.png

(If you want to see the old 50-language version, see [50](https://github.com/mame/quine-relay/tree/50) branch.)

## Usage

### Ubuntu

If you are using Ubuntu 20.04 LTS (Focal Fossa), you can perform the following steps.

#### 1. Install all interpreters/compilers.

First, you have to type the following apt-get command to install all of them.

    $ sudo apt-get install afnix algol68g aplus-fsf aspectc++ aspectj \
      asymptote ats2-lang bash bc bf bsdgames bsh clisp clojure cmake \
      coffeescript dafny dc ecere-dev elixir emacs-nox erlang f2c fish flex \
      fp-compiler fsharp g++ gambas3-script gap gawk gcc gdb gdc \
      generator-scripting-language genius gforth gfortran ghc ghostscript \
      gnat gnu-smalltalk gnucobol gnuplot gobjc golang groovy guile-2.0 gzip \
      haxe icont iconx intercal iverilog jasmin-sable jq julia ksh lisaac \
      livescript llvm lua5.3 m4 make maxima minizinc mlton mono-devel \
      mono-mcs mono-vbnc nasm neko nickle nim node-typescript nodejs ocaml \
      octave openjdk-11-jdk pakcs pari-gp parser3-cgi perl php-cli pike8.0 \
      python3 r-base rakudo ratfor rc regina-rexx ruby ruby-mustache rustc \
      scala scilab-cli sed slsh spin squirrel3 swi-prolog tcl tcsh valac vim \
      xsltproc yabasic yorick zoem zsh

Then, build the bundled interpreters.

    $ sudo apt-get install cmake libpng-dev libgd-dev groff bison curl
    $ make -C vendor

#### 2. Run each program on each interpreter/compiler.

    $ ulimit -s unlimited
    $ ruby QR.rb > QR.rs
    $ rustc QR.rs && ./QR > QR.scala
    $ scalac QR.scala && scala QR > QR.scm
    $ guile QR.scm > QR.sci
    $ scilab-cli -nb -f QR.sci > QR.sed
    $ sed -E -f QR.sed QR.sed > QR.spl
    $ ./vendor/local/bin/spl2c < QR.spl > QR.spl.c && gcc -o QR -I ./vendor/local/include -L ./vendor/local/lib QR.spl.c -lspl -lm &&
      ./QR > QR.sl
    $ slsh QR.sl > QR.st
    $ gst QR.st > QR.nut
    $ squirrel QR.nut > QR.sml
    $ mlton @MLton fixed-heap 200M -- QR.sml && ./QR > QR.sq
    $ ruby vendor/subleq.rb QR.sq > QR.tcl
    $ tclsh QR.tcl > QR.tcsh
    $ tcsh QR.tcsh > QR.t
    $ ruby vendor/thue.rb QR.t > QR.ts
    $ tsc --outFile QR.ts.js QR.ts && nodejs QR.ts.js > QR.unl
    $ ruby vendor/unlambda.rb QR.unl > QR.vala
    $ valac QR.vala && ./QR > QR.mid
    $ mono vendor/local/bin/Vlt.exe /s QR.mid && mono QR.exe > QR.v
    $ iverilog -o QR QR.v && ./QR -vcd-none > QR.vim
    $ vim -EsS QR.vim > QR.vb
    $ vbnc QR.vb && mono ./QR.exe > QR.ws
    $ ruby vendor/whitespace.rb QR.ws > QR.xslt
    $ xsltproc QR.xslt > QR.yab
    $ yabasic QR.yab > QR.yorick
    $ yorick -batch QR.yorick > QR.azm
    $ zoem -i QR.azm > QR.zsh
    $ zsh QR.zsh > QR.+
    $ a+ QR.+ > qr.adb
    $ gnatmake qr.adb && ./qr > QR.als
    $ LANG=C LD_LIBRARY_PATH=/usr/lib/afnix axi QR.als > QR.aheui
    $ ruby vendor/aheui.rb QR.aheui > QR.a68
    $ a68g QR.a68 > QR.ante
    $ ruby vendor/ante.rb QR.ante > QR.cc
    $ ag++ -std=c++11 -o QR QR.cc && ./QR > QR.aj
    $ ajc QR.aj && java QR > QR.asy
    $ asy QR.asy > QR.dats
    $ patscc -o QR QR.dats && ./QR > QR.awk
    $ awk -f QR.awk > QR.bash
    $ bash QR.bash > QR.bc
    $ BC_LINE_LENGTH=4000000 bc -q QR.bc > QR.bsh
    $ bsh QR.bsh > QR.bef
    $ cfunge QR.bef > QR.Blc
    $ ruby vendor/blc.rb < QR.Blc > QR.bf
    $ bf -c500000 QR.bf > QR.c
    $ gcc -o QR QR.c && ./QR > QR.cpp
    $ g++ -o QR QR.cpp && ./QR > QR.cs
    $ mcs QR.cs && mono QR.exe > QR.chef
    $ PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl &&
      perl QR.chef.pl > QR.clj
    $ clojure QR.clj > QR.cmake
    $ cmake -P QR.cmake > QR.cob
    $ cobc -O2 -x QR.cob && ./QR > QR.coffee
    $ coffee --nodejs --stack_size=100000 QR.coffee > QR.lisp
    $ clisp QR.lisp > QR.curry
    $ pakcs --nocypm :load QR.curry :save :quit && ./QR > QR.d
    $ gdc -o QR QR.d && ./QR > QR.dfy
    $ dafny QR.dfy && mono QR.exe > QR.dc
    $ dc QR.dc > QR.ec || true
    $ ecp -c QR.ec -o QR.sym && ecc -c QR.ec -o QR.c && ecs -console QR.sym QR.imp -o QR.main.ec &&
      ecp -c QR.main.ec -o QR.main.sym && ecc -c QR.main.ec -o QR.main.c &&
      gcc -o QR QR.c QR.main.c -lecereCOM && ./QR > QR.exs
    $ elixir QR.exs > QR.el
    $ emacs -Q --script QR.el > QR.erl
    $ escript QR.erl > QR.fsx
    $ fsharpc QR.fsx -o QR.exe && mono QR.exe > QR.false
    $ ruby vendor/false.rb QR.false > QR.fl
    $ flex -o QR.fl.c QR.fl && gcc -o QR QR.fl.c && ./QR > QR.fish
    $ fish QR.fish > QR.fs
    $ gforth QR.fs > QR.f
    $ gfortran -o QR QR.f && ./QR > QR.f90
    $ gfortran -o QR QR.f90 && ./QR > QR.gbs
    $ gbs3 QR.gbs > QR.g
    $ gap -q QR.g > QR.gdb
    $ gdb -q -x QR.gdb > QR.gel
    $ genius QR.gel > QR.gsl
    $ gsl -q QR.gsl > QR.plt
    $ gnuplot QR.plt > QR.go
    $ go run QR.go > QR.gs
    $ ruby vendor/golfscript.rb QR.gs > QR.grass
    $ ruby vendor/grass.rb QR.grass > QR.groovy
    $ groovy QR.groovy > QR.gz
    $ gzip -cd QR.gz > QR.hs
    $ ghc QR.hs && ./QR > QR.hx
    $ haxe -main QR -neko QR.n && neko QR.n > QR.icn
    $ icont -s QR.icn && ./QR > QR.i
    $ ick -bfOc QR.i && gcc -static QR.c -I /usr/include/ick-* -o QR -lick &&
      ./QR > QR.j
    $ jasmin QR.j && java QR > QR.java
    $ javac QR.java && java QR > QR.js
    $ nodejs QR.js > QR.jq
    $ jq -r -n -f QR.jq > QR.jsfuck
    $ nodejs --stack_size=100000 QR.jsfuck > QR.jl
    $ julia QR.jl > QR.ksh
    $ ksh QR.ksh > QR.lazy
    $ lazyk QR.lazy > qr.li
    $ lisaac qr.li && ./qr > QR.ls
    $ lsc QR.ls > QR.ll
    $ llvm-as QR.ll && lli QR.bc > QR.lol
    $ lci QR.lol > QR.lua
    $ lua5.3 QR.lua > QR.m4
    $ m4 QR.m4 > QR.mk
    $ make -f QR.mk > QR.mac
    $ maxima -q --init-mac=QR.mac > QR.mzn
    $ minizinc --solver Gecode --soln-sep '' QR.mzn > QR.il
    $ ilasm QR.il && mono QR.exe > QR.mustache
    $ mustache QR.mustache QR.mustache > QR.asm
    $ nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > QR.neko
    $ nekoc QR.neko && neko QR.n > QR.5c
    $ nickle QR.5c > QR.nim
    $ nim c QR.nim && ./QR > QR.m
    $ gcc -o QR QR.m && ./QR > QR.ml
    $ ocaml QR.ml > QR.octave
    $ mv QR.m QR.m.bak && octave -qf QR.octave > QR.ook && mv QR.m.bak QR.m
    $ ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf && bf -c500000 QR.ook.bf > QR.gp
    $ gp -f -q QR.gp > QR.p
    $ parser3 QR.p > QR.pas
    $ fpc QR.pas && ./QR > QR.pl
    $ perl QR.pl > QR.pl6
    $ perl6 QR.pl6 > QR.php
    $ php QR.php > QR.png
    $ npiet QR.png > QR.pike
    $ pike QR.pike > QR.ps
    $ gs -dNODISPLAY -q QR.ps > QR.ppt
    $ ppt -d < QR.ppt > QR.prolog
    $ swipl -q -t qr -f QR.prolog > QR.pr
    $ spin -T QR.pr > QR.py
    $ python3 QR.py > QR.R
    $ R --slave -f QR.R > QR.ratfor
    $ ratfor -o QR.ratfor.f QR.ratfor && gfortran -o QR QR.ratfor.f &&
      ./QR > QR.rc
    $ rc QR.rc > QR.rexx
    $ rexx ./QR.rexx > QR2.rb

You will see that `QR.rb` is the same as `QR2.rb`.

    $ diff QR.rb QR2.rb

Alternatively, just type `make`.

    $ make

Note: It may require huge memory to compile some files.

### Other platforms

You may find [instructions for other platforms in the wiki](https://github.com/mame/quine-relay/wiki/Installation).

If you are not using these Linux distributions, please find your way yourself.
If you could do it, please let me know.  Good luck.

## Tested interpreter/compiler versions

I used the following Ubuntu deb packages to test this program.

\#  |language                   |ubuntu package               |version
----|---------------------------|-----------------------------|--------------------------------------
1   |Ruby                       |ruby                         |1:2.7+1
2   |Rust                       |rustc                        |1.41.0+dfsg1+llvm-0ubuntu2
3   |Scala                      |scala                        |2.11.12-4
4   |Scheme                     |guile-2.0                    |2.0.13+1-5.4
5   |Scilab                     |scilab-cli                   |6.1.0+dfsg1-1ubuntu3
6   |sed                        |sed                          |4.7-1
7   |Shakespeare                |*N/A*                        |-
8   |S-Lang                     |slsh                         |2.3.2-4
9   |Smalltalk                  |gnu-smalltalk                |3.2.5-1.3build3
10  |Squirrel                   |squirrel3                    |3.1-8build1
11  |Standard ML                |mlton                        |20130715-3
12  |Subleq                     |*N/A*                        |-
13  |Tcl                        |tcl                          |8.6.9+1
14  |tcsh                       |tcsh                         |6.21.00-1
15  |Thue                       |*N/A*                        |-
16  |TypeScript                 |node-typescript              |3.8.3-1
17  |Unlambda                   |*N/A*                        |-
18  |Vala                       |valac                        |0.48.3-1
19  |Velato                     |*N/A*                        |-
20  |Verilog                    |iverilog                     |10.3-1build1
21  |Vimscript                  |vim                          |2:8.1.2269-1ubuntu5
22  |Visual Basic               |mono-vbnc                    |4.0.1-3
23  |Whitespace                 |*N/A*                        |-
24  |XSLT                       |xsltproc                     |1.1.34-4
25  |Yabasic                    |yabasic                      |1:2.86.6-1
26  |Yorick                     |yorick                       |2.2.04+dfsg1-10
27  |Zoem                       |zoem                         |11-166-1.2
28  |zsh                        |zsh                          |5.8-3ubuntu1
29  |A+                         |aplus-fsf                    |4.22.1-10.1
30  |Ada                        |gnat                         |9ubuntu2
31  |AFNIX                      |afnix                        |2.9.2-2build1
32  |Aheui                      |*N/A*                        |-
33  |ALGOL 68                   |algol68g                     |2.8.4-1
34  |Ante                       |*N/A*                        |-
35  |AspectC++                  |aspectc++                    |1:2.2+git20200229-1
36  |AspectJ                    |aspectj                      |1.9.2-1
37  |Asymptote                  |asymptote                    |2.62-1
38  |ATS                        |ats2-lang                    |0.3.13-1
39  |Awk                        |gawk                         |1:5.0.1+dfsg-1
40  |bash                       |bash                         |5.0-6ubuntu1
41  |bc                         |bc                           |1.07.1-2build1
42  |BeanShell                  |bsh                          |2.0b4-20
43  |Befunge                    |*N/A*                        |-
44  |BLC8                       |*N/A*                        |-
45  |Brainfuck                  |bf                           |20041219ubuntu6
46  |C                          |gcc                          |4:9.3.0-1ubuntu2
47  |C++                        |g++                          |4:9.3.0-1ubuntu2
48  |C#                         |mono-mcs                     |6.8.0.105+dfsg-2
49  |Chef                       |*N/A*                        |-
50  |Clojure                    |clojure                      |1.10.1-1
51  |CMake                      |cmake                        |3.16.3-1ubuntu1
52  |Cobol                      |gnucobol                     |2.2-5
53  |CoffeeScript               |coffeescript                 |1.12.8~dfsg-4build1
54  |Common Lisp                |clisp                        |1:2.49.20180218+really2.49.92-3build3
55  |Curry                      |pakcs                        |2.2.0-1
56  |D                          |gdc                          |4:10.0-1ubuntu2
57  |Dafny                      |dafny                        |2.3.0+dfsg-0.1
58  |dc                         |dc                           |1.07.1-2build1
59  |eC                         |ecere-dev                    |0.44.15-1build3
60  |Elixir                     |elixir                       |1.9.1.dfsg-1.3
61  |Emacs Lisp                 |emacs-nox                    |1:26.3+1-1ubuntu2
62  |Erlang                     |erlang                       |1:22.2.7+dfsg-1
63  |F#                         |fsharp                       |4.0.0.4+dfsg2-2
64  |FALSE                      |*N/A*                        |-
65  |Flex                       |flex                         |2.6.4-6.2
66  |Fish                       |fish                         |3.1.0-1.2
67  |Forth                      |gforth                       |0.7.3+dfsg-9build2
68  |FORTRAN77                  |f2c                          |20160102-1
69  |Fortran90                  |gfortran                     |4:9.3.0-1ubuntu2
70  |Gambas script              |gambas3-script               |3.14.3-2ubuntu3.1
71  |GAP                        |gap                          |4r10p2-2
72  |GDB                        |gdb                          |9.1-0ubuntu1
73  |GEL (Genius)               |genius                       |1.0.24-2build2
74  |GeneratorScriptingLanguage |generator-scripting-language |4.1.5-3
75  |Gnuplot                    |gnuplot                      |5.2.8+dfsg1-2
76  |Go                         |golang                       |2:1.13~1ubuntu2
77  |GolfScript                 |*N/A*                        |-
78  |Grass                      |*N/A*                        |-
79  |Groovy                     |groovy                       |2.4.17-4ubuntu1
80  |Gzip                       |gzip                         |1.10-0ubuntu4
81  |Haskell                    |ghc                          |8.8.1+dfsg1+is+8.6.5+dfsg1-3
82  |Haxe                       |haxe                         |1:4.0.5-1
83  |Icon                       |icont, iconx                 |9.4.3-7ubuntu1
84  |INTERCAL                   |intercal                     |30:0.30-3
85  |Jasmin                     |jasmin-sable                 |2.5.0-2
86  |Java                       |openjdk-11-jdk               |11.0.7+10-3ubuntu1
87  |JavaScript                 |nodejs                       |10.19.0~dfsg-3ubuntu1
88  |Jq                         |jq                           |1.6-1
89  |JSFuck                     |nodejs                       |10.19.0~dfsg-3ubuntu1
90  |Julia                      |julia                        |1.4.1+dfsg-1
91  |ksh                        |ksh                          |2020.0.0-5
92  |Lazy K                     |*N/A*                        |-
93  |Lisaac                     |lisaac                       |1:0.39~rc1-3build1
94  |LiveScript                 |livescript                   |1.6.0+dfsg-1
95  |LLVM asm                   |llvm                         |1:10.0-50~exp1
96  |LOLCODE                    |*N/A*                        |-
97  |Lua                        |lua5.3                       |5.3.3-1.1ubuntu2
98  |M4                         |m4                           |1.4.18-4
99  |Makefile                   |make                         |4.2.1-1.2
100 |Maxima                     |maxima                       |5.43.2-3
101 |MiniZinc                   |minizinc                     |2.4.2-1build1
102 |MSIL                       |mono-devel                   |6.8.0.105+dfsg-2
103 |Mustache                   |ruby-mustache                |1.0.2-1
104 |NASM                       |nasm                         |2.14.02-1
105 |Neko                       |neko                         |2.3.0-1
106 |Nickle                     |nickle                       |2.85-1build1
107 |Nim                        |nim                          |1.0.6-1
108 |Objective-C                |gobjc                        |4:9.3.0-1ubuntu2
109 |OCaml                      |ocaml                        |4.08.1-8
110 |Octave                     |octave                       |5.2.0-1
111 |Ook!                       |*N/A*                        |-
112 |PARI/GP                    |pari-gp                      |2.11.2-2
113 |Parser 3                   |parser3-cgi                  |3.4.5-7
114 |Pascal                     |fp-compiler                  |3.0.4+dfsg-23
115 |Perl 5                     |perl                         |5.30.0-9build1
116 |Perl 6                     |rakudo                       |2019.11-4
117 |PHP                        |php-cli                      |2:7.4+75
118 |Piet                       |*N/A*                        |-
119 |Pike                       |pike8.0                      |8.0.702-1ubuntu1
120 |PostScript                 |ghostscript                  |9.50~dfsg-5ubuntu4
121 |PPT (Punched tape)         |bsdgames                     |2.17-28build1
122 |Prolog                     |swi-prolog                   |7.6.4+dfsg-2ubuntu2
123 |Promela (Spin)             |spin                         |6.4.9+dfsg-1
124 |Python                     |python3                      |3.8.2-0ubuntu2
125 |R                          |r-base                       |3.6.3-2
126 |Ratfor                     |ratfor                       |1.04-1
127 |rc                         |rc                           |1.7.4+97.gceb59bb-2
128 |REXX                       |regina-rexx                  |3.6-2.1

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

See also [:heart:][sponsors].

[criteria]: https://github.com/mame/quine-relay/wiki/Criteria-for-language-inclusion
[sponsors]: https://github.com/sponsors/mame


### Q. Does it really work?

A. [![Build Status](https://travis-ci.org/mame/quine-relay.svg?branch=laboratory)](https://travis-ci.org/mame/quine-relay)

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

Copyright (c) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Yusuke Endoh (@mametter), @hirekoke

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
