require_relative "code-gen"
require "erb"
require "cairo"

other_packages = %w(cmake libpng-dev libgd-dev groff bison curl)
other_packages.each do |package|
  `dpkg -s #{ package }` # just check the packages
end

pkg_versions = {}
apts = RunSteps.map {|s| s.apt }
`which apt-get >/dev/null && dpkg -s #{ apts.join(" ") }`.b.split("\n\n").each do |s|
  name = s[/^Package: (.*)$/, 1]
  version = s[/^Version: (.*)$/, 1]
  pkg_versions[name] = version if name && version
end

rows = [["\\#", "language", "ubuntu package", "version"]]
rows += RunSteps.map.with_index do |s, idx|
  if s.apt.is_a?(Array)
    apt = s.apt.join(", ")
    ver = pkg_versions.values_at(*s.apt)
    #raise if ver.uniq.size > 1
    ver = ver.first
  else
    apt = s.apt || "*N/A*"
    ver = pkg_versions[apt]
  end
  ver = ver.gsub("~") { "\\~" } if ver
  [(idx + 1).to_s, s.name, apt, ver || "-"]
end

ws = rows.transpose.map {|row| row.map {|s| s.size }.max + 1 }
rows[1, 0] = [ws.map {|w| "-" * w }]
rows = rows.map do |col|
  (col.zip(ws).map {|s, w| s.ljust(w) } * "|").rstrip
end

apt_get = "sudo apt-get install #{ [*apts.flatten.compact.uniq].sort * " " }"
apt_get.gsub!(/.{,70}( |\z)/) do
  $&[-1] == " " ? $& + "\\\n      " : $&
end

cmds = [*RunSteps, RunStep["Ruby", "QR2.rb"]].each_cons(2).map do |s1, s2|
  cmd = s1.cmd_raw
  src = s2.src
  cmd = cmd.gsub("OUTFILE", src)

  cmd = cmd.gsub(/^!/, "")
  cmd = cmd.gsub(/.{60,}?&&/, "\\0\n     ")

  cmd
end

File.write("../README.md", ERB.new(DATA.read, trim_mode: "%").result(binding))


__END__
# Quine Relay

[![CI](https://github.com/mame/quine-relay/workflows/CI/badge.svg)](https://github.com/mame/quine-relay/actions?query=workflow%3ACI)

## What this is

[QR.rb](https://github.com/mame/quine-relay/blob/master/QR.rb) is a <%= RunSteps[0].name %> program that generates
a <%= RunSteps[1].name %> program that generates
a <%= RunSteps[2].name %> program that generates
...(through <%= RunSteps.size %> languages in total)...
a <%= RunSteps[-1].name %> program that generates
the original <%= RunSteps[0].name %> code again.

![Language Uroboros][langs]

[langs]: langs.png

(If you want to see the old 50-language version, see the [50](https://github.com/mame/quine-relay/tree/50) branch.)

## Usage

### Ubuntu

If you are using Ubuntu <%= `bash -c 'source /etc/os-release && echo $VERSION'`.chomp %>, you can follow these steps.

#### 1. Install all the interpreters/compilers.

First, you need to type the following apt-get command to install them all.

    $ <%= apt_get %>

Then, build the bundled interpreters.

    $ sudo apt-get install <%= other_packages.join(" ") %>
    $ make -C vendor

#### 2. Run each program on each interpreter/compiler.

    $ ulimit -s unlimited
% cmds.each do |cmd|
    $ <%= cmd %>
% end

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

% rows.each do |row|
<%= row %>
% end

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

## 2013/04 (for Ubuntu 13.04)

Added: Ruby, Scala, Scheme, bash, Smalltalk, Unlambda, Tcl, Whitespace, Verilog, Vala, Ada, ALGOL 68, Awk, Brainfuck, Boo, C, C++, C#, Cobol, Clojure, Fortran90, FORTRAN77, Forth, Common Lisp, CoffeeScript, Groovy, Go, INTERCAL, Icon, Haskell, Jasmin, Java, LLVM asm, Logo, Lua, Makefile, MSIL, Objective-C, JavaScript, OCaml, Octave, Parrot asm, Pascal, Perl, PHP, Pike, Prolog, Python, R, REXX

## 2013/10 (for Ubuntu 13.10)

## 2014/10 (for Ubuntu 14.10)

## 2013/10 (for Ubuntu 13.10)

## 2014/10 (for Ubuntu 14.10)

Added: Scilab, S-Lang, SPL, LOLCODE, Maxima, NASM, Neko, Nickle, Ook!, PARI/GP, Piet, PPT (Punched tape), PostScript, Ratfor

## 2015/04 (for Ubuntu 15.04)

Added: Subleq, Standard ML, Thue, Visual Basic, XSLT, Yorick, Zoem, A+, AFNIX, Ante, Asymptote, ATS, BLC8, Befunge, bc, Chef, CDuce, D, dc, eC, Emacs Lisp, Erlang, F#, Falcon, FALSE, Gambas script, GAP, GEL (Genius), Gnuplot, G-Portugol, Gri, Haxe, Julia, Lisaac, Lazy K, Kaya

## 2015/10 (for Ubuntu 15.10)

Removed: Boo, Falcon, Kaya

Added: Elixir, Jq, Nim

## 2017/04 (for Ubuntu 17.04)

Removed: SPL, Gri, Logo, Parrot asm

Added: Squirrel, Dafny, Grass, MiniZinc

## 2017/10 (for Ubuntu 17.10)

Removed: CDuce

Added: Rust

## 2018/04 (for Ubuntu 18.04)

Removed: Gambas script, Perl

Added: Shakespeare, sed, tcsh, TypeScript, Velato, Vimscript, Yabasic, zsh, Aheui, AspectC++, AspectJ, BeanShell, CMake, Flex, Fish, GDB, GolfScript, Gzip, Gri, JSFuck, ksh, LiveScript, M4, Mustache, nesC, Parser 3, Perl 5, Perl 6, Promela (Spin), rc

## 2019/04 (for Ubuntu 19.04)

Removed: Scilab, G-Portugol, nesC

Added: Curry, Gambas script, GeneratorScriptingLanguage

## 2019/10 (for Ubuntu 19.10)

Removed: Gri

Added: Scilab

## 2020/10 (for Ubuntu 20.10)

Removed: AspectC++, eC

Added: SurgeScript, Dhall

## 2021/04 (for Ubuntu 21.04)

Removed: Curry

Added: G-Portugol

## 2022/04 (for Ubuntu 22.04)

Removed: Julia, Nim, Pike

Added: WebAssembly (Text format), WebAssembly (Binary format), Kotlin

## 2023/04 (for Ubuntu 23.04)

Removed: Squirrel, GeneratorScriptingLanguage

Added: Crystal, Nim

## 2023/10 (for Ubuntu 23.10)

Removed: Smalltalk

Added: Modula-2

## License

The MIT License applies to all resources
*except* the files in the `vendor/` directory.

The files in the `vendor/` directory are from third-parties
and are distributed under different licenses.
See `vendor/README` in detail.

---

The MIT License (MIT)

Copyright (c) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Yusuke Endoh (@mametter), @hirekoke

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
