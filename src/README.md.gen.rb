require_relative "code-gen"
require "erb"
require "cairo"

langs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.name } }
cmds = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.cmd } }
srcs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.src } }
apts = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.apt } }

pkg_versions = {}
`which apt-get >/dev/null && dpkg -p #{ apts.join(" ") }`.b.split("\n\n").each do |s|
  name = s[/^Package: (.*)$/, 1]
  version = s[/^Version: (.*)$/, 1]
  pkg_versions[name] = version if name && version
end

rows = [["\\#", "language", "ubuntu package", "version"]]
rows += langs.zip(apts).flat_map.with_index do |(lang, apt), idx|
  apt = [apt] unless apt.is_a?(Array)
  apt.map.with_index do |apt, i|
    [i == 0 ? (idx + 1).to_s : "", i == 0 ? lang : "", apt || "(none)", pkg_versions[apt] || '-']
  end
end

ws = rows.transpose.map {|row| row.map {|s| s.size }.max + 1 }
rows[1, 0] = [ws.map {|w| "-" * w }]
rows = rows.map do |col|
  (col.zip(ws).map {|s, w| s.ljust(w) } * "|").rstrip
end

apt_get = "sudo apt-get install #{ apts.flatten.compact.uniq.sort * " " }"
apt_get.gsub!(/.{,70}( |\z)/) do
  $&[-1] == " " ? $& + "\\\n      " : $&
end

cmds = cmds.zip(srcs.drop(1) + ["QR.rb"]).map do |cmd, src|
  cmd = cmd.gsub("OUTFILE", src).gsub(/mv QR\.c(\.bak)? QR\.c(\.bak)? && /, "")

  cmd = cmd.gsub("$(SCHEME)", "gosh")
  cmd = cmd.gsub("$(NODE)", "nodejs")
  cmd = cmd.gsub("$(BF)", "bf")
  cmd = cmd.gsub("${CC}", "gcc")
  cmd = cmd.gsub("${CXX}", "g++")

  cmd
end
cmds[-1].gsub!("QR.rb", "QR2.rb")

File.write("../README.md", ERB.new(DATA.read, nil, "%").result(binding))


__END__
# Quine Relay

### What this is

This is a <%= langs[0] %> program that generates
<%= langs[1] %> program that generates
<%= langs[2] %> program that generates
...(through <%= langs.size %> languages in total)...
<%= langs[-1] %> program that generates
the original <%= langs[0] %> code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

### Usage

#### 1. Install all interpreters/compilers.

You are fortunate if you are using Ubuntu 14.04 (Trusty Tahr).
You just have to type the following apt-get command to install all of them.

    $ <%= apt_get %>

You are even more fortunate if you are using Arch Linux,
as you can just install the [quine-relay-git](https://aur.archlinux.org/packages/quine-relay-git/) package from AUR,
either manually or by using your favorite AUR helper.

You may find [instructions for other platforms in the wiki](https://github.com/mame/quine-relay/wiki/Installation).

If you are not using these Linux distributions, please find your way yourself.
If you could do it, please let me know.  Good luck.

#### 2. Run each program on each interpreter/compiler.

% cmds.each do |cmd|
    $ <%= cmd %>
% end

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

% rows.each do |row|
<%= row %>
% end

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
