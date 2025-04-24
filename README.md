# Quine Relay

[![CI](https://github.com/mame/quine-relay/workflows/CI/badge.svg)](https://github.com/mame/quine-relay/actions?query=workflow%3ACI)

## What this is

[QR.rb](https://github.com/mame/quine-relay/blob/master/QR.rb) is a Ruby program that generates
a Rust program that generates
a Scala program that generates
...(through 128 languages in total)...
a REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: langs.png

(If you want to see the old 50-language version, see the [50](https://github.com/mame/quine-relay/tree/50) branch.)

## Usage

### Ubuntu

If you are using Ubuntu 25.04 (Plucky Puffin), you can follow these steps.

#### 1. Install all the interpreters/compilers.

First, you need to type the following apt-get command to install them all.

    $ sudo apt-get install afnix algol68g aplus-fsf aspectj asymptote \
      ats2-lang bash bc bsh clisp clojure cmake coffeescript crystal dc \
      dhall dotnet8 elixir emacs-nox erlang execline f2c fish flex \
      fp-compiler g++ gambas3-gb-pcre gambas3-scripter gap gawk gcc gdb gdc \
      genius gforth gfortran ghc ghostscript gm2 gnat gnucobol4 gnuplot \
      gobjc golang gpt groovy guile-3.0 gzip haxe icont iconx intercal \
      iverilog jasmin-sable jq kotlin ksh libevent-dev libpolyml-dev lisaac \
      livescript llvm lua5.3 m4 make minizinc mono-devel nasm neko nickle \
      nim node-typescript nodejs ocaml octave openjdk-11-jdk pari-gp \
      parser3-cgi perl php-cli pike8.0 polyml python3 r-base rakudo ratfor \
      rc regina-rexx ruby ruby-mustache rustc scala scilab-cli sed slsh spin \
      squirrel3 surgescript swi-prolog tcl tcsh valac vim wabt xsltproc \
      yabasic yorick zoem zsh

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
    $ spl2c < QR.spl > QR.spl.c && gcc -z muldefs -o QR -I ./vendor/local/include -L ./vendor/local/lib QR.spl.c -lspl -lm &&
      ./QR > QR.sl
    $ slsh QR.sl > QR.nut
    $ squirrel QR.nut > QR.sml
    $ polyc -o QR QR.sml && ./QR > QR.sq
    $ ruby vendor/subleq.rb QR.sq > QR.ss
    $ surgescript QR.ss > QR.tcl
    $ tclsh QR.tcl > QR.tcsh
    $ tcsh QR.tcsh > QR.t
    $ ruby vendor/thue.rb QR.t > QR.ts
    $ tsc --outFile QR.ts.js QR.ts && nodejs QR.ts.js > QR.unl
    $ ruby vendor/unlambda.rb QR.unl > QR.vala
    $ valac QR.vala && ./QR > QR.mid
    $ mono vendor/local/bin/Vlt.exe /s QR.mid && mono QR.exe > QR.v
    $ iverilog -o QR QR.v && ./QR -vcd-none > QR.vim
    $ vim -EsS QR.vim > QR.vb
    $ echo '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework><EnableDefaultCompileItems>false</EnableDefaultCompileItems></PropertyGroup><ItemGroup><Compile Include="QR.vb" /></ItemGroup></Project>' > tmp.vbproj &&
      DOTNET_NOLOGO=1 dotnet run --project tmp.vbproj > QR.wasm
    $ $(WASI_RUNTIME) QR.wasm > QR.wat
    $ wat2wasm QR.wat -o QR.wat.wasm && $(WASI_RUNTIME) QR.wat.wasm > QR.ws
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
    $ ruby vendor/ante.rb QR.ante > QR.aj
    $ ajc QR.aj && java QR > QR.asy
    $ asy QR.asy > QR.dats
    $ patscc -o QR QR.dats && ./QR > QR.awk
    $ awk -f QR.awk > QR.bash
    $ bash QR.bash > QR.bc
    $ BC_LINE_LENGTH=4000000 bc -q QR.bc > QR.bsh
    $ bsh QR.bsh > QR.bef
    $ cfunge QR.bef > QR.Blc
    $ ruby vendor/blc.rb < QR.Blc > QR.bf
    $ ruby vendor/bf.rb QR.bf > QR.c
    $ gcc -o QR QR.c && ./QR > QR.cpp
    $ g++ -o QR QR.cpp && ./QR > QR.cs
    $ echo '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework><EnableDefaultCompileItems>false</EnableDefaultCompileItems></PropertyGroup><ItemGroup><Compile Include="QR.cs" /></ItemGroup></Project>' > tmp.csproj &&
      DOTNET_NOLOGO=1 dotnet run --project tmp.csproj > QR.chef
    $ PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl &&
      perl QR.chef.pl > QR.clj
    $ clojure QR.clj > QR.cmake
    $ cmake -P QR.cmake > QR.cob
    $ cobc -O2 -x QR.cob && ./QR > QR.coffee
    $ coffee --nodejs --stack_size=100000 QR.coffee > QR.lisp
    $ clisp QR.lisp > QR.cr
    $ crystal QR.cr > QR.d
    $ gdc -o QR QR.d && ./QR > QR.dc
    $ dc QR.dc > QR.dhall || true
    $ dhall text --file QR.dhall > QR.exs
    $ elixir QR.exs > QR.el
    $ emacs -Q --script QR.el > QR.erl
    $ escript QR.erl > QR.e
    $ execlineb QR.e > QR.fsx
    $ echo '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework><EnableDefaultCompileItems>false</EnableDefaultCompileItems></PropertyGroup><ItemGroup><Compile Include="QR.fsx" /></ItemGroup></Project>' > tmp.fsproj &&
      DOTNET_NOLOGO=1 dotnet run --project tmp.fsproj > QR.false
    $ ruby vendor/false.rb QR.false > QR.fl
    $ flex -o QR.fl.c QR.fl && gcc -o QR QR.fl.c && ./QR > QR.fish
    $ fish QR.fish > QR.fs
    $ gforth QR.fs > QR.f
    $ gfortran -o QR QR.f && ./QR > QR.f90
    $ gfortran -o QR QR.f90 && ./QR > QR.gbs
    $ gbs3 QR.gbs > QR.g
    $ gap -q QR.g > QR.gdb
    $ gdb -q -x QR.gdb > QR.gel
    $ genius QR.gel > QR.plt
    $ gnuplot QR.plt > QR.go
    $ go run QR.go > QR.gs
    $ ruby vendor/golfscript.rb QR.gs > QR.gpt
    $ mv QR.c QR.c.bak && gpt -t QR.c QR.gpt && gcc -o QR QR.c && ./QR > QR.grass &&
      mv QR.c.bak QR.c
    $ ruby vendor/grass.rb QR.grass > QR.groovy
    $ groovy QR.groovy > QR.gz
    $ gzip -cd QR.gz > QR.hs
    $ rm -f QR.o && ghc QR.hs && ./QR > QR.hx
    $ haxe -main QR -neko QR.n && neko QR.n > QR.icn
    $ icont -s QR.icn && ./QR > QR.i
    $ ick -bfOc QR.i && gcc -static QR.c -I /usr/include/ick-* -o QR -lick &&
      ./QR > QR.j
    $ jasmin QR.j && java QR > QR.java
    $ javac QR.java && java QR > QR.js
    $ nodejs QR.js > QR.jq
    $ jq -r -n -f QR.jq > QR.jsfuck
    $ nodejs --stack_size=100000 QR.jsfuck > QR.kt
    $ kotlinc QR.kt -include-runtime -d QR.jar && kotlin QR.jar > QR.ksh
    $ ksh QR.ksh > QR.lazy
    $ lazyk QR.lazy > qr.li
    $ lisaac -gcc -Wno-implicit-function-declaration qr.li && ./qr > QR.ls
    $ lsc QR.ls > QR.ll
    $ llvm-as QR.ll && lli QR.bc > QR.lol
    $ lci QR.lol > QR.lua
    $ lua5.3 QR.lua > QR.m4
    $ m4 QR.m4 > QR.mk
    $ make -f QR.mk > QR.mzn
    $ minizinc --solver COIN-BC --soln-sep '' QR.mzn > QR.mod
    $ gm2 -fiso QR.mod -o QR && ./QR > QR.il
    $ ilasm QR.il && mono QR.exe > QR.mustache
    $ mustache QR.mustache QR.mustache > QR.asm
    $ nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > QR.neko
    $ nekoc QR.neko && neko QR.n > QR.5c
    $ nickle QR.5c > QR.nim
    $ nim compile QR.nim && ./QR > QR.m
    $ gcc -o QR QR.m && ./QR > QR.ml
    $ ocaml QR.ml > QR.octave
    $ mv QR.m QR.m.bak && octave -qf QR.octave > QR.ook && mv QR.m.bak QR.m
    $ ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf && ruby vendor/bf.rb QR.ook.bf > QR.gp
    $ gp -f -q QR.gp > QR.p
    $ parser3 QR.p > QR.pas
    $ fpc QR.pas && ./QR > QR.pl
    $ perl QR.pl > QR.pl6
    $ perl6 QR.pl6 > QR.php
    $ php QR.php > QR.png
    $ npiet QR.png > QR.pike
    $ pike QR.pike > QR.ps
    $ gs -dNODISPLAY -q QR.ps > QR.prolog
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

Note: It may take a lot of memory to compile some files.

### Docker

Simply build the image and run a container as follows:

    $ docker build -t qr .
    $ docker run --privileged --rm -e CI=true qr

Note: You must run in privileged mode, otherwise the `maxima` command will fail.

If you want to check the generated files, you can mount the local directory in the Docker container (but still use the `vendor` directory of the container), as follows:

    $ docker run --privileged --rm -e CI=true -v $(pwd):/usr/local/share/quine-relay -v /usr/local/share/quine-relay/vendor qr

### Other platforms

You may find [instructions for other platforms in the wiki](https://github.com/mame/quine-relay/wiki/Installation).

If you do not use these Linux distributions, please find your own way.
If you manage it, please let me know.  I wish you good luck.

## Interpreter/compiler versions tested

I used the following Ubuntu deb packages to test this program.

\#  |language                    |ubuntu package                    |version
----|----------------------------|----------------------------------|---------------------------------
1   |Ruby                        |ruby                              |1:3.3\~ubuntu3
2   |Rust                        |rustc                             |1.84.0ubuntu1
3   |Scala                       |scala                             |2.11.12-6
4   |Scheme                      |guile-3.0                         |3.0.10+really3.0.10-4
5   |Scilab                      |scilab-cli                        |2024.1.0+dfsg-6build5
6   |sed                         |sed                               |4.9-2build1
7   |Shakespeare                 |*N/A*                             |-
8   |S-Lang                      |slsh                              |2.3.3-5
9   |Squirrel                    |squirrel3                         |3.1-8.2
10  |Standard ML                 |polyml, libpolyml-dev             |5.7.1-5build1
11  |Subleq                      |*N/A*                             |-
12  |SurgeScript                 |surgescript                       |0.5.4.4-1.1
13  |Tcl                         |tcl                               |8.6.14build1
14  |tcsh                        |tcsh                              |6.24.13-2
15  |Thue                        |*N/A*                             |-
16  |TypeScript                  |node-typescript                   |4.9.5+ds1-2
17  |Unlambda                    |*N/A*                             |-
18  |Vala                        |valac                             |0.56.18-2
19  |Velato                      |*N/A*                             |-
20  |Verilog                     |iverilog                          |12.0-2build2
21  |Vimscript                   |vim                               |2:9.1.0967-1ubuntu4
22  |Visual Basic                |dotnet8                           |8.0.115-8.0.15-0ubuntu1
23  |WebAssembly (Binary format) |wabt                              |1.0.34+dsfg2+\~cs1.0.32-1ubuntu2
24  |WebAssembly (Text format)   |wabt                              |1.0.34+dsfg2+\~cs1.0.32-1ubuntu2
25  |Whitespace                  |*N/A*                             |-
26  |XSLT                        |xsltproc                          |1.1.39-0exp1ubuntu4
27  |Yabasic                     |yabasic                           |1:2.91.1-1
28  |Yorick                      |yorick                            |2.2.04+dfsg1-14
29  |Zoem                        |zoem                              |21-341-1
30  |zsh                         |zsh                               |5.9-6ubuntu3
31  |A+                          |aplus-fsf                         |4.22.1-13
32  |Ada                         |gnat                              |14.1ubuntu1
33  |AFNIX                       |afnix                             |3.8.0-1.1ubuntu1
34  |Aheui                       |*N/A*                             |-
35  |ALGOL 68                    |algol68g                          |3.1.2-1
36  |Ante                        |*N/A*                             |-
37  |AspectJ                     |aspectj                           |1.9.6-1
38  |Asymptote                   |asymptote                         |2.95+ds-1
39  |ATS                         |ats2-lang                         |0.4.2-3
40  |Awk                         |gawk                              |1:5.2.1-2build3
41  |bash                        |bash                              |5.2.37-1.1ubuntu1
42  |bc                          |bc                                |1.07.1-4
43  |BeanShell                   |bsh                               |2.0b4-20
44  |Befunge                     |*N/A*                             |-
45  |BLC8                        |*N/A*                             |-
46  |Brainfuck                   |*N/A*                             |-
47  |C                           |gcc                               |4:14.2.0-1ubuntu1
48  |C++                         |g++                               |4:14.2.0-1ubuntu1
49  |C#                          |dotnet8                           |8.0.115-8.0.15-0ubuntu1
50  |Chef                        |*N/A*                             |-
51  |Clojure                     |clojure                           |1.12.0-1
52  |CMake                       |cmake                             |3.31.6-1ubuntu1
53  |Cobol                       |gnucobol4                         |4.0\~early\~20200606-7
54  |CoffeeScript                |coffeescript                      |2.7.0+dfsg1-2
55  |Common Lisp                 |clisp                             |1:2.49.20241123.git9ff8aed-1
56  |Crystal                     |crystal, libevent-dev             |1.14.0+dfsg-1
57  |D                           |gdc                               |4:14.2.0-1ubuntu1
58  |dc                          |dc                                |1.07.1-4
59  |Dhall                       |dhall                             |1.42.1-1build2
60  |Elixir                      |elixir                            |1.18.1.dfsg-1.1
61  |Emacs Lisp                  |emacs-nox                         |1:30.1+1-5ubuntu1
62  |Erlang                      |erlang                            |1:27.3+dfsg-1ubuntu1.1
63  |Execline                    |execline                          |2.9.6.1-1
64  |F#                          |dotnet8                           |8.0.115-8.0.15-0ubuntu1
65  |FALSE                       |*N/A*                             |-
66  |Flex                        |flex                              |2.6.4-8.2build1
67  |Fish                        |fish                              |4.0.1-1
68  |Forth                       |gforth                            |0.7.3+dfsg-9build4.1
69  |FORTRAN77                   |f2c                               |20240504-1
70  |Fortran90                   |gfortran                          |4:14.2.0-1ubuntu1
71  |Gambas script               |gambas3-scripter, gambas3-gb-pcre |3.20.2-1build1
72  |GAP                         |gap                               |4.14.0-3
73  |GDB                         |gdb                               |16.2-8ubuntu1
74  |GEL (Genius)                |genius                            |1.0.27-1build4
75  |Gnuplot                     |gnuplot                           |6.0.2+dfsg1-1
76  |Go                          |golang                            |2:1.24\~2
77  |GolfScript                  |*N/A*                             |-
78  |G-Portugol                  |gpt                               |1.1-8
79  |Grass                       |*N/A*                             |-
80  |Groovy                      |groovy                            |2.4.21-10
81  |Gzip                        |gzip                              |1.13-1ubuntu3
82  |Haskell                     |ghc                               |9.6.6-4
83  |Haxe                        |haxe                              |1:4.3.6-3
84  |Icon                        |icont, iconx                      |9.5.24a-2
85  |INTERCAL                    |intercal                          |30:0.30-6
86  |Jasmin                      |jasmin-sable                      |2.5.0-3
87  |Java                        |openjdk-11-jdk                    |11.0.26+4-1ubuntu1
88  |JavaScript                  |nodejs                            |20.18.1+dfsg-1ubuntu2
89  |Jq                          |jq                                |1.7.1-3ubuntu1
90  |JSFuck                      |nodejs                            |20.18.1+dfsg-1ubuntu2
91  |Kotlin                      |kotlin                            |1.3.31+ds1-2
92  |ksh                         |ksh                               |20240113-1.0.10-2
93  |Lazy K                      |*N/A*                             |-
94  |Lisaac                      |lisaac                            |1:0.39\~rc1-3.1
95  |LiveScript                  |livescript                        |1.6.1+dfsg-3
96  |LLVM asm                    |llvm                              |1:20.0-63ubuntu1
97  |LOLCODE                     |*N/A*                             |-
98  |Lua                         |lua5.3                            |5.3.6-2build2
99  |M4                          |m4                                |1.4.19-7
100 |Makefile                    |make                              |4.4.1-1
101 |MiniZinc                    |minizinc                          |2.9.2+dfsg1-1
102 |Modula-2                    |gm2                               |4:14.2.0-1ubuntu1
103 |MSIL                        |mono-devel                        |6.12.0.199+dfsg-3
104 |Mustache                    |ruby-mustache                     |1.1.1-2
105 |NASM                        |nasm                              |2.16.03-1
106 |Neko                        |neko                              |2.4.0-2
107 |Nickle                      |nickle                            |2.102
108 |Nim                         |nim                               |1.6.14-1ubuntu2
109 |Objective-C                 |gobjc                             |4:14.2.0-1ubuntu1
110 |OCaml                       |ocaml                             |5.3.0-2
111 |Octave                      |octave                            |9.4.0-1
112 |Ook!                        |*N/A*                             |-
113 |PARI/GP                     |pari-gp                           |2.17.2-1
114 |Parser 3                    |parser3-cgi                       |3.5.1-2
115 |Pascal                      |fp-compiler                       |3.2.2+dfsg-46
116 |Perl 5                      |perl                              |5.40.1-2ubuntu0.1
117 |Perl 6                      |rakudo                            |2022.12-1
118 |PHP                         |php-cli                           |2:8.4+96ubuntu1
119 |Piet                        |*N/A*                             |-
120 |Pike                        |pike8.0                           |8.0.1738-1.4build1
121 |PostScript                  |ghostscript                       |10.05.0dfsg1-0ubuntu1
122 |Prolog                      |swi-prolog                        |9.0.4+dfsg-3.1ubuntu4
123 |Promela (Spin)              |spin                              |6.5.2+dfsg-2
124 |Python                      |python3                           |3.13.3-1
125 |R                           |r-base                            |4.4.3-1
126 |Ratfor                      |ratfor                            |1.07-1
127 |rc                          |rc                                |1.7.4+97.gceb59bb-5build2
128 |REXX                        |regina-rexx                       |3.9.5+dfsg1-0.1

Note that some languages are not available in Ubuntu (marked as *N/A*).
This repository contains their implementations in `vendor/`.
See also `vendor/README` for detail.


## Frequently asked questions

### Q. Why?

A. [Take your pick](https://github.com/mame/quine-relay/issues/11).

### Q. How?

A. Good news: I have published a book, ["The World of Obfuscated, Esoteric, Artistic Programming"](http://gihyo.jp/book/2015/978-4-7741-7643-7).
It explains how to write a quine, an ascii-art quine, and an uroboros quine like this quine-relay.
You can buy my book on [amazon.co.jp](http://www.amazon.co.jp/dp/4774176435).

(It also contains my almost all of my (about forty) works, including
[alphabet-only Ruby program](http://www.slideshare.net/mametter/ruby-esoteric-obfuscated-ruby-programming-5088683),
[radiation-hardened quine](https://github.com/mame/radiation-hardened-quine),
etc., and explains many techniques for writing such programs.)

Bad news: It is written in Japanese.
I hope you can translate it into English <strike>and help me earn royalties</strike>.

### Q. Language XXX is missing!

A. See [the language inclusion criteria][criteria] in detail.  (In short, please create a deb package and contribute it to Ubuntu.)

See also [:heart:][sponsors].

[criteria]: https://github.com/mame/quine-relay/wiki/Language-inclusion-criteria
[sponsors]: https://github.com/sponsors/mame


### Q. Does it really work?

A. [![CI](https://github.com/mame/quine-relay/workflows/CI/badge.svg)](https://github.com/mame/quine-relay/actions?query=workflow%2ACI)

### Q. How long did it take you?

A. [Are you trying to cross the world line?](https://github.com/mame/quine-relay/issues/60)

### Q. The code does not fit in my screen!

A. [Here you go][thumbnail].

[thumbnail]: thumbnail.png

### Q. How was the code generated?

A.

    $ sudo apt-get install rake ruby-cairo ruby-rsvg2 ruby-gdk-pixbuf2 \
      optipng advancecomp ruby-chunky-png
    $ cd src
    $ rake2.0 clobber
    $ rake2.0

## History

## for Ubuntu 13.04

[50 languages](https://github.com/mame/quine-relay/tree/ad3f8222c796969db8cfb1bae015a46c2387b3d6)

Added: Ruby, Scala, Scheme, bash, Smalltalk, Unlambda, Tcl, Whitespace, Verilog, Vala, Ada, ALGOL 68, Awk, Brainfuck, Boo, C, C++, C#, Cobol, Clojure, Fortran90, FORTRAN77, Forth, Common Lisp, CoffeeScript, Groovy, Go, INTERCAL, Icon, Haskell, Jasmin, Java, LLVM asm, Logo, Lua, Makefile, MSIL, Objective-C, JavaScript, OCaml, Octave, Parrot asm, Pascal, Perl, PHP, Pike, Prolog, Python, R, REXX

## for Ubuntu 13.10

[50 languages](https://github.com/mame/quine-relay/tree/ea4d39fb1ebc7ee23ec6f60ca7bfa0d465b5806a)

## for Ubuntu 14.04

[50 languages](https://github.com/mame/quine-relay/tree/d16bf072e3063dc476dc440c8f3e33d7426e98db)

## for Ubuntu 14.10

[64 languages](https://github.com/mame/quine-relay/tree/e449baba456d4885102482cbd365335be59241b2)

Added: Scilab, S-Lang, SPL, LOLCODE, Maxima, NASM, Neko, Nickle, Ook!, PARI/GP, Piet, PPT (Punched tape), PostScript, Ratfor

## for Ubuntu 15.04

[100 languages](https://github.com/mame/quine-relay/tree/7749715289ca162eb1c1eb1ff1ed1393edc41630)

Added: Subleq, Standard ML, Thue, Visual Basic, XSLT, Yorick, Zoem, A+, AFNIX, Ante, Asymptote, ATS, BLC8, Befunge, bc, Chef, CDuce, D, dc, eC, Emacs Lisp, Erlang, F#, Falcon, FALSE, Gambas script, GAP, GEL (Genius), Gnuplot, G-Portugol, Gri, Haxe, Julia, Lisaac, Lazy K, Kaya

## for Ubuntu 15.10

[100 languages](https://github.com/mame/quine-relay/tree/f45035f867c7c8f7b4e12fa63e7c8eef9aabecad)

Removed: Boo, Falcon, Kaya

Added: Elixir, Jq, Nim

## for Ubuntu 16.04

[100 languages](https://github.com/mame/quine-relay/tree/233ba8b4e1d7e4c59a46d64481048a8ea7f4400e)

## for Ubuntu 17.04

[100 languages](https://github.com/mame/quine-relay/tree/e65a798da23df0367d9eb5e4d46f00d87e6cb342)

Removed: SPL, Gri, Logo, Parrot asm

Added: Squirrel, Dafny, Grass, MiniZinc

## for Ubuntu 17.10

[100 languages](https://github.com/mame/quine-relay/tree/943b83801a1bd019ebf348adc78df2cdfde06513)

Removed: CDuce

Added: Rust

## for Ubuntu 18.04

[128 languages](https://github.com/mame/quine-relay/tree/6e173d76e972e1da7992b84768bf9f4c788949ed)

Removed: Gambas script, Perl

Added: Shakespeare, sed, tcsh, TypeScript, Velato, Vimscript, Yabasic, zsh, Aheui, AspectC++, AspectJ, BeanShell, CMake, Flex, Fish, GDB, GolfScript, Gzip, Gri, JSFuck, ksh, LiveScript, M4, Mustache, nesC, Parser 3, Perl 5, Perl 6, Promela (Spin), rc

## for Ubuntu 19.04

[128 languages](https://github.com/mame/quine-relay/tree/c8898351500682cea02219313e9203da7eca5505)

Removed: Scilab, G-Portugol, nesC

Added: Curry, Gambas script, GeneratorScriptingLanguage

## for Ubuntu 19.10

[128 languages](https://github.com/mame/quine-relay/tree/13041dbf3f80a90c9221ef94f8e8bc385800b6fd)

Removed: Gri

Added: Scilab

## for Ubuntu 20.04

[128 languages](https://github.com/mame/quine-relay/tree/62e2cc2d61d99719328094d185d899bc03a851fb)

## for Ubuntu 20.10

[128 languages](https://github.com/mame/quine-relay/tree/114f44fefd610812d2f5e3032603762752ed51b2)

Removed: AspectC++, eC

Added: SurgeScript, Dhall

## for Ubuntu 21.04

[128 languages](https://github.com/mame/quine-relay/tree/d8df33bad3693afd0bd3bf1c2b1cedd5753325dc)

Removed: Curry

Added: G-Portugol

## for Ubuntu 21.10

[128 languages](https://github.com/mame/quine-relay/tree/158b6251d36a48122ec0006feaf759d8b1973b0f)

## for Ubuntu 22.04

[128 languages](https://github.com/mame/quine-relay/tree/7b81d8704549d31814499c5300be2be9568467c8)

Removed: Julia, Nim, Pike

Added: WebAssembly (Text format), WebAssembly (Binary format), Kotlin

## for Ubuntu 22.10

[128 languages](https://github.com/mame/quine-relay/tree/362962dd2d55d7c36dd9fa3e0d4c0c52c0e6a18f)

## for Ubuntu 23.04

[128 languages](https://github.com/mame/quine-relay/tree/5dfdada5aa58f6a97ae85b84f86c7eb091225a8c)

Removed: Squirrel, GeneratorScriptingLanguage

Added: Crystal, Nim

## for Ubuntu 23.10

[128 languages](https://github.com/mame/quine-relay/tree/74c4cc7d79fccbb1c8315070b9efea03cb787755)

Removed: Smalltalk

Added: Modula-2

## for Ubuntu 24.04

[128 languages](https://github.com/mame/quine-relay/tree/20d7f437c053b8e0b301ba996d124a4b812e3571)

Removed: PPT (Punched tape)

Added: Pike

## for Ubuntu 24.10

128 languages

Removed: Dafny, Maxima

Added: Execline, Squirrel

## License

The MIT License applies to all resources
*except* the files in the `vendor/` directory.

The files in the `vendor/` directory are from third-parties
and are distributed under different licenses.
See `vendor/README` in detail.

---

The MIT License (MIT)

Copyright (c) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024 Yusuke Endoh (@mametter), @hirekoke

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
