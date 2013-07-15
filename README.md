# Quine Relay

### What this is

This is a Ruby program that generates
Scala program that generates
Scheme program that generates
...(through 50 languages)...
REXX program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

### Usage

#### 1. Install all interpreters/compilers.

You are fortunate if you are using Ubuntu 13.04 (Raring Ringtail).
You just have to type the following apt-get command to install all of them.

    $ apt-get install algol68g bash beef boo clisp clojure1.4 coffeescript \
      fp-compiler g++ gauche gawk gcc gforth gfortran ghc gnat gnu-smalltalk \
      gobjc golang groovy icont intercal iverilog jasmin-sable llvm lua5.2 \
      make mono-devel mono-mcs nodejs ocaml octave open-cobol openjdk-6-jdk \
      parrot perl php5-cli pike7.8 python r-base regina-rexx ruby1.9.3 scala \
      swi-prolog tcl8.5 ucblogo valac

If you are not using Ubuntu, please find your way yourself.
If you could do it, please let me know.  Good luck.

#### 2. Run each program on each interpreter/compiler.

    $ ruby QR.rb > QR.scala
    $ scalac QR.scala && scala QR > QR.scm
    $ gosh QR.scm > QR.bash
    $ bash QR.bash > QR.st
    $ gst QR.st > QR.tcl
    $ tclsh QR.tcl > QR.unl
    $ ruby unlambda.rb QR.unl > QR.vala
    $ valac QR.vala && ./QR > QR.v
    $ iverilog -o QR QR.v && ./QR -vcd-none > QR.ws
    $ ruby whitespace.rb QR.ws > QR.adb
    $ gnatmake QR.adb && ./QR > QR.a68
    $ a68g QR.a68 > QR.awk
    $ awk -f QR.awk > QR.boo
    $ booi QR.boo > QR.bf
    $ beef QR.bf > QR.c
    $ gcc -o QR QR.c && ./QR > QR.cpp
    $ g++ -o QR QR.cpp && ./QR > QR.cs
    $ mcs QR.cs && mono QR.exe > QR.clj
    $ clojure QR.clj > QR.cob
    $ cobc -x QR.cob && ./QR > QR.coffee
    $ coffee QR.coffee > QR.lisp
    $ clisp QR.lisp > QR.fs
    $ gforth QR.fs > QR.f
    $ gfortran -o QR QR.f && ./QR > QR.f90
    $ gfortran -o QR QR.f90 && ./QR > QR.go
    $ go run QR.go > QR.groovy
    $ groovy QR.groovy > QR.hs
    $ runghc QR.hs > QR.icn
    $ icont -s QR.icn && ./QR > QR.i
    $  ick -b QR.i &&  ./QR > QR.j
    $ jasmin QR.j && java QR > QR.java
    $ javac QR.java && java QR > QR.ll
    $ llvm-as QR.ll && lli QR.bc > QR.logo
    $ ucblogo QR.logo > QR.lua
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

### Tested interpreter/compiler versions

As I said above, I tested the program on Ubuntu.
It does not provide Unlambda and Whitespace interpreters,
so this repository includes my own implementations.
For other languages, I used the following deb packages:

language     |ubuntu package |version
-------------|---------------|-----------------------------------
Ruby         |ruby1.9.3      |1.9.3.194-8.1ubuntu1.1
Scala        |scala          |2.9.2+dfsg-1
Scheme       |gauche         |0.9.3.3-8
Shell        |bash           |4.2-5ubuntu3
Smalltalk    |gnu-smalltalk  |3.2.4-2
Tcl          |tcl8.5         |8.5.13-1ubuntu4
Unlambda     |(none)         |-
Vala         |valac          |0.18.1-0ubuntu4
Verilog      |iverilog       |0.9.6-1
Whitespace   |(none)         |-
Ada          |gnat           |4.6ubuntu1
ALGOL68      |algol68g       |2.4.1-1
Awk          |gawk           |1:4.0.1+dfsg-2ubuntu1
Boo          |boo            |0.9.5~git20110729.r1.202a430-2
Brainfuck    |beef           |0.0.6-2
C            |gcc            |4:4.7.3-1ubuntu10
C++          |g++            |4:4.7.3-1ubuntu10
C#           |mono-mcs       |2.10.8.1-5ubuntu1
Clojure      |clojure1.4     |1.4.0+dfsg-2ubuntu2
Cobol        |open-cobol     |1.1-1
CoffeeScript |coffeescript   |1.4.0-1
CommonLisp   |clisp          |1:2.49-8.1ubuntu1
Forth        |gforth         |0.7.0+ds2-0.1
FORTRAN77    |gfortran       |4:4.7.3-1ubuntu10
Fortran90    |gfortran       |4:4.7.3-1ubuntu10
Go           |golang         |2:1.0.2-2
Groovy       |groovy         |2.0.0~beta2+isreally1.8.6-0ubuntu1
Haskell      |ghc            |7.6.2-1ubuntu1
Icon         |icont          |9.4.3-4
INTERCAL     |intercal       |29:0.29-2
Jasmin       |jasmin-sable   |2.4.0-1ubuntu1
Java         |openjdk-6-jdk  |6b27-1.12.5-1ubuntu1
LLVM asm     |llvm           |1:3.2-16~exp1
Logo         |ucblogo        |5.5-2.1
Lua          |lua5.2         |5.2.1-3
Makefile     |make           |3.81-8.2ubuntu2
MSIL         |mono-devel     |2.10.8.1-5ubuntu1
NodeJS       |nodejs         |0.6.19~dfsg1-5ubuntu1
Objective-C  |gobjc          |4:4.7.3-1ubuntu10
OCaml        |ocaml          |3.12.1-2ubuntu3
Octave       |octave         |3.6.4-1
Parrot asm   |parrot         |4.6.0-1
Pascal       |fp-compiler    |2.6.0-9
Perl         |perl           |5.14.2-21
PHP          |php5-cli       |5.4.9-4ubuntu2.1
Pike         |pike7.8        |7.8.352-dfsg-7ubuntu1
Prolog       |swi-prolog     |5.10.4-5ubuntu1
Python       |python         |2.7.4-0ubuntu1
R            |r-base         |2.15.2-1ubuntu1
REXX         |regina-rexx    |3.6-2

### How to re-generate the source

    $ cd src
    $ rake

### License

Copyright (c) 2013 Yusuke Endoh (@mametter), @hirekoke

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
