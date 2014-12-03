require_relative "code-gen"
require "erb"
require "cairo"

other_packages = %w(cmake libpng12-dev libgd2-xpm-dev groff)
other_packages.each do |package|
  `dpkg -p #{ package }` # just check the packages
end

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
    [i == 0 ? (idx + 1).to_s : "", i == 0 ? lang : "", apt || "*N/A*", pkg_versions[apt] || '-']
  end
end

ws = rows.transpose.map {|row| row.map {|s| s.size }.max + 1 }
rows[1, 0] = [ws.map {|w| "-" * w }]
rows = rows.map do |col|
  (col.zip(ws).map {|s, w| s.ljust(w) } * "|").rstrip
end

apt_get = "sudo apt-get install #{ [*apts.flatten.compact.uniq, *other_packages].sort * " " }"
apt_get.gsub!(/.{,70}( |\z)/) do
  $&[-1] == " " ? $& + "\\\n      " : $&
end

cmds = cmds.zip(srcs.drop(1) + ["QR.rb"]).map do |cmd, src|
  cmd = cmd.gsub("OUTFILE", src).gsub(/mv QR\.c(\.bak)? QR\.c(\.bak)? && /, "")

  cmd = cmd.gsub("$(SCHEME)", "gosh")
  cmd = cmd.gsub("$(JAVASCRIPT)", "rhino")
  cmd = cmd.gsub("$(BF)", "bf")
  cmd = cmd.gsub("$(CC)", "gcc")
  cmd = cmd.gsub("$(CXX)", "g++")

  cmd
end
cmds[-1].gsub!("QR.rb", "QR2.rb")

File.write("../README.md", ERB.new(DATA.read, nil, "%").result(binding))


__END__
# Quine Relay

[![Build Status](https://travis-ci.org/mame/quine-relay.svg?branch=master)](https://travis-ci.org/mame/quine-relay)

### What this is

This is a <%= langs[0] %> program that generates
<%= langs[1] %> program that generates
<%= langs[2] %> program that generates
...(through <%= langs.size %> languages in total)...
<%= langs[-1] %> program that generates
the original <%= langs[0] %> code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

(If you want to see the old 50-language version, see [50](https://github.com/mame/quine-relay/tree/50) branch.)

### Usage

#### 1. Install all interpreters/compilers.

If you are using Ubuntu 14.10 "Utopic Unicorn", you can perform the following steps:

First, you have to type the following apt-get command to install all of them.

    $ <%= apt_get %>

Then, you have to build the bundled interpreters.

    $ cd vendor
    $ make

If you are using Arch Linux, just install [quine-relay-git](https://aur.archlinux.org/packages/quine-relay-git/) from AUR and run `quine-relay`.
Report any problems as comments to the AUR package or to the respective packages, if one of the many compilers should have issues.

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

I used the following Ubuntu deb packages to test this program.

% rows.each do |row|
<%= row %>
% end

Note that some languages are not available in Ubuntu (marked as *N/A*).
This repository includes their implementations in `vendor/`.
See also `vendor/README` in detail.

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
