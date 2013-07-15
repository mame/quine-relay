require_relative "code-gen"
require "erb"
require "cairo"

langs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.name } }
cmds = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.cmd } }
exts = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.ext } }
apts = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.apt } }.compact

rows = [["language", "ubuntu package", "version"]] + CodeGen::List.reverse.flat_map do |c|
  c.steps.map do |step|
    version = step.apt ? `dpkg -p #{ step.apt }`.b[/^Version: (.*)/, 1] : "-"
    [step.name, step.apt || "(none)", version]
  end
end
ws = rows.transpose.map {|row| row.map {|s| s.size }.max + 1 }
rows[1, 0] = [ws.map {|w| "-" * w }]
rows = rows.map do |col|
  (col.zip(ws).map {|s, w| s.ljust(w) } * "|").rstrip
end

apts = "apt-get install #{ apts.uniq.sort * " " }".gsub(/.{,70}( |\z)/) do
  $&[-1] == " " ? $& + "\\\n      " : $&
end

cmds = cmds.zip(exts.drop(1) + [".rb"]).map do |cmd, ext|
  cmd.gsub("OUTFILE", "QR" + ext).gsub(/mv QR\.c(\.bak)? QR\.c(\.bak)? &&/, "")
end
cmds[-1].gsub!("QR.rb", "QR2.rb")

File.write("../README.md", ERB.new(DATA.read, nil, "%").result(binding))


__END__
# Quine Relay

### What this is

This is a <%= langs[0] %> program that generates
<%= langs[1] %> program that generates
<%= langs[2] %> program that generates
...(through <%= langs.size %> languages)...
<%= langs[-1] %> program that generates
the original Ruby code again.

![Language Uroboros][langs]

[langs]: https://raw.github.com/mame/quine-relay/master/langs.png

### Usage

#### 1. Install all interpreters/compilers.

You are fortunate if you are using Ubuntu 13.04 (Raring Ringtail).
You just have to type the following apt-get command to install all of them.

    $ <%= apts %>

If you are not using Ubuntu, please find your way yourself.
If you could do it, please let me know.  Good luck.

#### 2. Run each program on each interpreter/compiler.

% cmds.each do |cmd|
    $ <%= cmd %>
% end

You will see that `QR.rb` is the same as `QR2.rb`.

    $ diff QR.rb QR2.rb

Alternatively, just type `make`.

    $ make

### Tested interpreter/compiler versions

As I said above, I tested the program on Ubuntu.
It does not provide Unlambda and Whitespace interpreters,
so this repository includes my own implementations.
For other languages, I used the following deb packages:

% rows.each do |row|
<%= row %>
% end

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
