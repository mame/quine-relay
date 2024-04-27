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

If you are using Ubuntu 24.04 LTS (Noble Numbat), you can follow these steps.

#### 1. Install all the interpreters/compilers.

First, you need to type the following apt-get command to install them all.

    $ sudo apt-get install afnix algol68g aplus-fsf aspectj asymptote \
      ats2-lang bash bc bsh clisp clojure cmake coffeescript crystal dafny \
      dc dhall dotnet8 elixir emacs-nox erlang f2c fish flex fp-compiler g++ \
      gambas3-gb-pcre gambas3-scripter gap gawk gcc gdb gdc genius gforth \
      gfortran ghc ghostscript gm2 gnat gnucobol4 gnuplot gobjc golang gpt \
      groovy guile-3.0 gzip haxe icont iconx intercal iverilog jasmin-sable \
      jq kotlin ksh libevent-dev libpolyml-dev lisaac livescript llvm lua5.3 \
      m4 make maxima minizinc mono-devel nasm neko nickle nim \
      node-typescript nodejs ocaml octave openjdk-11-jdk pari-gp parser3-cgi \
      perl php-cli pike8.0 polyml python3 r-base rakudo ratfor rc \
      regina-rexx ruby ruby-mustache rustc scala scilab-cli sed slsh spin \
      surgescript swi-prolog tcl tcsh valac vim wabt xsltproc yabasic yorick \
      zoem zsh

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
    $ slsh QR.sl > QR.sml
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
    $ gdc -o QR QR.d && ./QR > QR.dfy
    $ dafny QR.dfy && mono QR.exe > QR.dc
    $ dc QR.dc > QR.dhall || true
    $ dhall text --file QR.dhall > QR.exs
    $ elixir QR.exs > QR.el
    $ emacs -Q --script QR.el > QR.erl
    $ escript QR.erl > QR.fsx
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
    $ ghc QR.hs && ./QR > QR.hx
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
    $ lisaac qr.li && ./qr > QR.ls
    $ lsc QR.ls > QR.ll
    $ llvm-as QR.ll && lli QR.bc > QR.lol
    $ lci QR.lol > QR.lua
    $ lua5.3 QR.lua > QR.m4
    $ m4 QR.m4 > QR.mk
    $ make -f QR.mk > QR.mac
    $ maxima -q --init-mac=QR.mac > QR.mzn
    $ minizinc --solver Gecode --soln-sep '' QR.mzn > QR.mod
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
----|----------------------------|----------------------------------|-------------------------------------
1   |Ruby                        |ruby                              |1:3.2\~ubuntu1
2   |Rust                        |rustc                             |1.75.0+dfsg0ubuntu1-0ubuntu7
3   |Scala                       |scala                             |2.11.12-5
4   |Scheme                      |guile-3.0                         |3.0.9-1build2
5   |Scilab                      |scilab-cli                        |2024.0.0+dfsg-5build3
6   |sed                         |sed                               |4.9-2build1
7   |Shakespeare                 |*N/A*                             |-
8   |S-Lang                      |slsh                              |2.3.3-3build2
9   |Standard ML                 |polyml, libpolyml-dev             |5.7.1-5build1
10  |Subleq                      |*N/A*                             |-
11  |SurgeScript                 |surgescript                       |0.5.4.4-1.1
12  |Tcl                         |tcl                               |8.6.14build1
13  |tcsh                        |tcsh                              |6.24.10-4build1
14  |Thue                        |*N/A*                             |-
15  |TypeScript                  |node-typescript                   |4.8.4+ds1-2
16  |Unlambda                    |*N/A*                             |-
17  |Vala                        |valac                             |0.56.16-3build1
18  |Velato                      |*N/A*                             |-
19  |Verilog                     |iverilog                          |12.0-2build2
20  |Vimscript                   |vim                               |2:9.1.0016-1ubuntu7
21  |Visual Basic                |dotnet8                           |8.0.104-8.0.4-0ubuntu1
22  |WebAssembly (Binary format) |wabt                              |1.0.34+dsfg2+\~cs1.0.32-1ubuntu2
23  |WebAssembly (Text format)   |wabt                              |1.0.34+dsfg2+\~cs1.0.32-1ubuntu2
24  |Whitespace                  |*N/A*                             |-
25  |XSLT                        |xsltproc                          |1.1.39-0exp1build1
26  |Yabasic                     |yabasic                           |1:2.90.3-1
27  |Yorick                      |yorick                            |2.2.04+dfsg1-12build3
28  |Zoem                        |zoem                              |21-341-1
29  |zsh                         |zsh                               |5.9-6ubuntu2
30  |A+                          |aplus-fsf                         |4.22.1-12
31  |Ada                         |gnat                              |13.2ubuntu2
32  |AFNIX                       |afnix                             |3.8.0-1
33  |Aheui                       |*N/A*                             |-
34  |ALGOL 68                    |algol68g                          |3.1.2-1
35  |Ante                        |*N/A*                             |-
36  |AspectJ                     |aspectj                           |1.9.5-2
37  |Asymptote                   |asymptote                         |2.87+ds-1build2
38  |ATS                         |ats2-lang                         |0.4.2-1.1
39  |Awk                         |gawk                              |1:5.2.1-2build3
40  |bash                        |bash                              |5.2.21-2ubuntu4
41  |bc                          |bc                                |1.07.1-3ubuntu4
42  |BeanShell                   |bsh                               |2.0b4-20
43  |Befunge                     |*N/A*                             |-
44  |BLC8                        |*N/A*                             |-
45  |Brainfuck                   |*N/A*                             |-
46  |C                           |gcc                               |4:13.2.0-7ubuntu1
47  |C++                         |g++                               |4:13.2.0-7ubuntu1
48  |C#                          |dotnet8                           |8.0.104-8.0.4-0ubuntu1
49  |Chef                        |*N/A*                             |-
50  |Clojure                     |clojure                           |1.11.1-2
51  |CMake                       |cmake                             |3.28.3-1build7
52  |Cobol                       |gnucobol4                         |4.0\~early\~20200606-6.1build1
53  |CoffeeScript                |coffeescript                      |2.7.0+dfsg1-1
54  |Common Lisp                 |clisp                             |1:2.49.20210628.gitde01f0f-3.1build3
55  |Crystal                     |crystal, libevent-dev             |1.11.2+dfsg-1build3
56  |D                           |gdc                               |4:13.2.0-7ubuntu1
57  |Dafny                       |dafny                             |2.3.0+dfsg-0.1
58  |dc                          |dc                                |1.07.1-3ubuntu4
59  |Dhall                       |dhall                             |1.41.2-1
60  |Elixir                      |elixir                            |1.14.0.dfsg-2
61  |Emacs Lisp                  |emacs-nox                         |1:29.3+1-1ubuntu2
62  |Erlang                      |erlang                            |1:25.3.2.8+dfsg-1ubuntu4
63  |F#                          |dotnet8                           |8.0.104-8.0.4-0ubuntu1
64  |FALSE                       |*N/A*                             |-
65  |Flex                        |flex                              |2.6.4-8.2build1
66  |Fish                        |fish                              |3.7.0-1
67  |Forth                       |gforth                            |0.7.3+dfsg-9build4.1
68  |FORTRAN77                   |f2c                               |20200916-1
69  |Fortran90                   |gfortran                          |4:13.2.0-7ubuntu1
70  |Gambas script               |gambas3-scripter, gambas3-gb-pcre |3.19.0-2ubuntu10
71  |GAP                         |gap                               |4.12.1-2build2
72  |GDB                         |gdb                               |15.0.50.20240403-0ubuntu1
73  |GEL (Genius)                |genius                            |1.0.27-1build4
74  |Gnuplot                     |gnuplot                           |6.0.0+dfsg1-1ubuntu3
75  |Go                          |golang                            |2:1.22\~2build1
76  |GolfScript                  |*N/A*                             |-
77  |G-Portugol                  |gpt                               |1.1-8
78  |Grass                       |*N/A*                             |-
79  |Groovy                      |groovy                            |2.4.21-10
80  |Gzip                        |gzip                              |1.12-1ubuntu3
81  |Haskell                     |ghc                               |9.4.7-3
82  |Haxe                        |haxe                              |1:4.3.3-1build2
83  |Icon                        |icont, iconx                      |9.4.3-7ubuntu1
84  |INTERCAL                    |intercal                          |30:0.30-6
85  |Jasmin                      |jasmin-sable                      |2.5.0-2
86  |Java                        |openjdk-11-jdk                    |11.0.23+9-1ubuntu1
87  |JavaScript                  |nodejs                            |18.19.1+dfsg-6ubuntu5
88  |Jq                          |jq                                |1.7.1-3build1
89  |JSFuck                      |nodejs                            |18.19.1+dfsg-6ubuntu5
90  |Kotlin                      |kotlin                            |1.3.31+ds1-1ubuntu1
91  |ksh                         |ksh                               |20240113
92  |Lazy K                      |*N/A*                             |-
93  |Lisaac                      |lisaac                            |1:0.39\~rc1-3.1
94  |LiveScript                  |livescript                        |1.6.1+dfsg-3
95  |LLVM asm                    |llvm                              |1:18.0-59\~exp2
96  |LOLCODE                     |*N/A*                             |-
97  |Lua                         |lua5.3                            |5.3.6-2build2
98  |M4                          |m4                                |1.4.19-4build1
99  |Makefile                    |make                              |4.3-4.1build2
100 |Maxima                      |maxima                            |5.46.0-11build3
101 |MiniZinc                    |minizinc                          |2.8.2+dfsg1-1build2
102 |Modula-2                    |gm2                               |4:13.2.0-7ubuntu1
103 |MSIL                        |mono-devel                        |6.8.0.105+dfsg-3.6ubuntu2
104 |Mustache                    |ruby-mustache                     |1.1.1-2
105 |NASM                        |nasm                              |2.16.01-1build1
106 |Neko                        |neko                              |2.3.0-2build2
107 |Nickle                      |nickle                            |2.97build2
108 |Nim                         |nim                               |1.6.14-1ubuntu2
109 |Objective-C                 |gobjc                             |4:13.2.0-7ubuntu1
110 |OCaml                       |ocaml                             |4.14.1-1ubuntu1
111 |Octave                      |octave                            |8.4.0-1build5
112 |Ook!                        |*N/A*                             |-
113 |PARI/GP                     |pari-gp                           |2.15.4-2.1build1
114 |Parser 3                    |parser3-cgi                       |3.4.6-4
115 |Pascal                      |fp-compiler                       |3.2.2+dfsg-32
116 |Perl 5                      |perl                              |5.38.2-3.2build2
117 |Perl 6                      |rakudo                            |2022.12-1
118 |PHP                         |php-cli                           |2:8.3+93ubuntu2
119 |Piet                        |*N/A*                             |-
120 |Pike                        |pike8.0                           |8.0.1738-1.3ubuntu1
121 |PostScript                  |ghostscript                       |10.02.1\~dfsg1-0ubuntu7
122 |Prolog                      |swi-prolog                        |9.0.4+dfsg-3.1ubuntu4
123 |Promela (Spin)              |spin                              |6.5.2+dfsg-1
124 |Python                      |python3                           |3.12.3-0ubuntu1
125 |R                           |r-base                            |4.3.3-2build2
126 |Ratfor                      |ratfor                            |1.05-2
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

## License

The MIT License applies to all resources
*except* the files in the `vendor/` directory.

The files in the `vendor/` directory are from third-parties
and are distributed under different licenses.
See `vendor/README` in detail.

---

The MIT License (MIT)

Copyright (c) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 Yusuke Endoh (@mametter), @hirekoke

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
