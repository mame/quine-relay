# Quine Relay

### What this is

This is a Ruby program that generates
Scala program that generates
Scheme program that generates
...(through 65 languages in total)...
REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

(If you want to see the old 50-language version, see [50](https://github.com/mame/quine-relay/tree/50) branch.)

### Usage

#### 1. Install all interpreters/compilers.

If you are using Arch Linux, just install [quine-relay-git](https://aur.archlinux.org/packages/quine-relay-git/) from AUR and run `quine-relay`.
Report any problems as comments to the AUR package or to the respective packages, if one of the many compilers should have issues.

If you are using Ubuntu 14.10 "Utopic Unicorn", you can perform the following steps:

First, you have to type the following apt-get command to install all of them.

    $ sudo apt-get install algol68g bash bf boo bsdgames clisp clojure1.4 \
      cmake coffeescript f2c fp-compiler g++ gauche gawk gcc gforth gfortran \
      ghc ghostscript gnat gnu-smalltalk gobjc golang groovy icont iconx \
      intercal iverilog jasmin-sable libpng12-dev llvm lua5.2 make maxima \
      mlton mono-devel mono-mcs nasm neko nickle nodejs ocaml octave \
      open-cobol openjdk-6-jdk pari-gp parrot perl php5-cli pike7.8 python \
      r-base ratfor regina-rexx ruby2.0 scala scilab slsh spl-core \
      swi-prolog tcl ucblogo valac

Then, you have to build the bundled interpreters.

    $ cd vendor
    $ make

You may find [instructions for other platforms in the wiki](https://github.com/mame/quine-relay/wiki/Installation).

If you are not using these Linux distributions, please find your way yourself.
If you could do it, please let me know.  Good luck.

#### 2. Run each program on each interpreter/compiler.

    $ ruby QR.rb > QR.scala
    $ scalac QR.scala && CLASSPATH=. scala QR > QR.scm
    $ gosh QR.scm > QR.sci
    $ scilab -nw -nb -f QR.sci > QR.bash
    $ bash QR.bash > QR.sl
    $ slsh QR.sl > QR.st
    $ gst QR.st > QR.spl
    $ splrun QR.spl > QR.sml
    $ mlton QR.sml && ./QR > QR.tcl
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
    $ f2c QR.f && gcc -o QR QR.c -L/usr/lib -lf2c -lm && ./QR > QR.f90
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
    $ make -f QR.makefile > QR.mac
    $ maxima -q --init-mac=QR.mac > QR.il
    $ ilasm QR.il && mono QR.exe > QR.asm
    $ nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > QR.neko
    $ nekoc QR.neko && neko QR.n > QR.5c
    $ nickle QR.5c > QR.js
    $ nodejs QR.js > QR.m
    $ gcc -o QR QR.m && ./QR > QR.ml
    $ ocaml QR.ml > QR.octave
    $ octave -qf QR.octave > QR.ook
    $ ruby vendor/ook.rb QR.ook > QR.gp
    $ gp -f -q QR.gp > QR.pasm
    $ parrot QR.pasm > QR.pas
    $ fpc QR.pas && ./QR > QR.pl
    $ perl QR.pl > QR.php
    $ php QR.php > QR.png
    $ vendor/npiet-*/npiet QR.png > QR.pike
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

### Tested interpreter/compiler versions

As I said above, I tested the program on Ubuntu.
It does not provide Unlambda and Whitespace interpreters,
so this repository includes my own implementations.
For other languages, I used the following deb packages:

\# |language           |ubuntu package |version                             |arch package    |version   |repo
---|-------------------|---------------|------------------------------------|----------------|----------|--------
1  |Ruby               |ruby2.0        |2.0.0.484-1ubuntu2                  |ruby            |2.1.5     |[extra]
2  |Scala              |scala          |2.9.2+dfsg-2                        |scala           |2.11.4    |[community]
3  |Scheme             |gauche         |0.9.3.3-8ubuntu1                    |chicken         |4.9.0.1   |[community]
4  |Scilab             |scilab         |5.5.0-2                             |scilab          |5.5.1     |AUR
5  |Shell (bash)       |bash           |4.3-11ubuntu1                       |bash            |4.3.030   |[core]
6  |S-Lang             |slsh           |2.2.4-15ubuntu1                     |slang           |2.2.4     |[extra]
7  |Smalltalk          |gnu-smalltalk  |3.2.4-2                             |smalltalk       |3.2.5     |[community]
8  |SPL                |spl-core       |1.0~pre6-4ubuntu1                   |spl             |1.0pre6   |AUR
9  |Standard ML        |mlton          |20100608-5.1                        |mlton           |20130715  |[community]
10 |Tcl                |tcl            |8.6.0+6ubuntu3                      |tcl             |8.6.3     |[extra]
11 |Unlambda           |(none)         |-                                   |(none)          |-         |-
12 |Vala               |valac          |0.22.1-0ubuntu1                     |vala            |0.26.1    |[extra]
13 |Verilog            |iverilog       |0.9.7-1                             |iverilog        |0.9.7     |[community]
14 |Whitespace         |(none)         |-                                   |wspace          |0.3       |AUR
15 |Ada                |gnat           |4.6ubuntu4                          |gcc-ada         |4.9.2     |[core]
16 |ALGOL68            |algol68g       |2.4.1-1                             |algol68g        |2.8       |[community]
17 |Awk                |gawk           |1:4.0.1+dfsg-2.1ubuntu2             |gawk            |4.1.1     |[core]
18 |Boo                |boo            |0.9.5~git20110729.r1.202a430-2      |boo             |0.9.4.9   |[extra]
19 |Brainfuck          |bf             |20041219ubuntu5                     |beef            |1.0.0     |AUR
20 |C                  |gcc            |4:4.8.2-1ubuntu6                    |gcc             |4.9.2     |[core]
21 |C++                |g++            |4:4.8.2-1ubuntu6                    |gcc             |4.9.2     |[core]
22 |C#                 |mono-mcs       |3.2.8+dfsg-4ubuntu1                 |mono            |3.10.0    |[extra]
23 |Clojure            |clojure1.4     |1.4.0+dfsg-3                        |clojure         |1.6.0     |[community]
24 |Cobol              |open-cobol     |1.1-1build1                         |open-cobol      |1.1       |AUR
25 |CoffeeScript       |coffeescript   |1.4.0-1                             |coffee-script   |1.8.0     |AUR
26 |Common Lisp        |clisp          |1:2.49-9ubuntu1                     |clisp           |2.49      |[extra]
27 |Forth              |gforth         |0.7.0+ds2-0.1                       |gforth          |0.7.3     |AUR
28 |FORTRAN77          |f2c            |20100827-1                          |f2c             |1.0       |AUR
29 |Fortran90          |gfortran       |4:4.8.2-1ubuntu6                    |gcc-fortran     |4.9.2     |[core]
30 |Go                 |golang         |2:1.2.1-2ubuntu1                    |go              |1.3.3     |[community]
31 |Groovy             |groovy         |2.0.0~beta2+isreally1.8.6-0ubuntu1  |groovy          |2.3.7     |[community]
32 |Haskell            |ghc            |7.6.3-10                            |ghc             |7.8.3     |[extra]
33 |Icon               |icont          |9.4.3-4.2ubuntu1                    |icon            |951       |AUR
   |                   |iconx          |9.4.3-4.2ubuntu1                    |                |          |
34 |INTERCAL           |intercal       |29:0.29-3                           |c-intercal      |0.29      |AUR
35 |Jasmin             |jasmin-sable   |2.4.0-3                             |jasmin          |2.4       |AUR
36 |Java               |openjdk-6-jdk  |6b33-1.13.5-1ubuntu0.14.04          |jdk8-openjdk    |8.u25     |[extra]
37 |LLVM asm           |llvm           |1:3.4-0ubuntu1                      |llvm            |3.5.0     |[extra]
38 |Logo               |ucblogo        |5.5-2.1                             |ucblogo         |6.0       |[community]
39 |LOLCODE            |(none)         |-                                   |lci-git         |20110725  |AUR
40 |Lua                |lua5.2         |5.2.3-1                             |lua             |5.2.3     |[extra]
41 |Makefile           |make           |3.81-8.2ubuntu3                     |make            |4.1       |[core]
42 |Maxima             |maxima         |5.32.1-1                            |maxima          |5.34.1    |[extra]
43 |MSIL               |mono-devel     |3.2.8+dfsg-4ubuntu1                 |mono            |3.10.0    |[extra]
44 |NASM               |nasm           |2.10.09-1                           |nasm            |2.11.06   |[extra]
45 |Neko               |neko           |2.0.0-3                             |neko            |2.0.0     |AUR
46 |Nickle             |nickle         |2.77-1                              |nickle          |2.70      |[community]
47 |NodeJS             |nodejs         |0.10.25~dfsg2-2ubuntu1              |nodejs          |0.10.33   |[community]
48 |Objective-C        |gobjc          |4:4.8.2-1ubuntu6                    |gcc-objc        |4.9.2     |[core]
49 |OCaml              |ocaml          |4.01.0-3ubuntu3                     |ocaml           |4.02.0    |[extra]
50 |Octave             |octave         |3.8.1-1ubuntu1                      |octave          |3.8.2     |[extra]
51 |Ook!               |(none)         |-                                   |(none)          |-         |-
52 |PARI/GP            |pari-gp        |2.5.5-1                             |pari            |2.7.2     |[community]
53 |Parrot asm         |parrot         |5.9.0-1build1                       |parrot          |6.8.0     |[community]
54 |Pascal             |fp-compiler    |2.6.2-8                             |fpc             |2.6.4     |[community]
55 |Perl               |perl           |5.18.2-2ubuntu1                     |perl            |5.20.1    |[core]
56 |PHP                |php5-cli       |5.5.9+dfsg-1ubuntu4.4               |php             |5.6.3     |[extra]
57 |Piet               |(none)         |-                                   |npiet           |1.3c      |AUR
58 |Pike               |pike7.8        |7.8.700-7                           |pike            |7.8.866   |AUR
59 |PostScript         |ghostscript    |9.10~dfsg-0ubuntu10.2               |ghostscript     |9.15      |[extra]
60 |PPT (Punched tape) |bsdgames       |2.17-21                             |bsd-games       |2.17      |[community]
61 |Prolog             |swi-prolog     |6.6.4-2ubuntu1                      |swi-prolog      |6.6.6     |[community]
62 |Python             |python         |2.7.5-5ubuntu3                      |python2         |2.7.8     |[extra]
63 |R                  |r-base         |3.0.2-1ubuntu1                      |r               |3.1.2     |[extra]
64 |Ratfor             |ratfor         |1.0-15                              |ratfor          |1.0       |AUR
65 |REXX               |regina-rexx    |3.6-2                               |regina-rexx-das |3.7       |AUR

Note: `CC=tcc ick -bfO` may be used to compile INTERCAL sources
with less memory.

### How to re-generate the source on Ubuntu

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
