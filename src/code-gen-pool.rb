original_list_size = CodeGen::List.size

#class NesC < CodeGen
#  Name = "nesC"
#  File = "QR.nc"
#  Cmd = "nescc -o QR QR.nc && ./QR > OUTFILE"
#  Apt = "nescc"
#  def code
#    <<-'END'.lines.map {|l| l.strip }.join
#      %(
#        #include<stdio.h>\n
#        module QR{}implementation{
#          int main()__attribute__((C,spontaneous)){
#            puts#{E[PREV]};
#            return 0;
#        } }
#      )
#    END
#    # avoid "}}" because of Mustache
#  end
#end

#class Nim_NVSPL2 < CodeGen
#  After = Nim
#  Obsoletes = Nim
#  File = ["QR.nim", "QR.nvspl2"]
#  Cmd = ["nim c QR.nim && ./QR > OUTFILE", "ruby vendor/nvspl2.rb QR.nvspl2 > OUTFILE"]
#  Apt = ["nim", nil]
#  Code = %q(%((for i, c in#{E[PREV]}:echo ",",int(c),"CO");echo "Q"))
#end

# Zig: systems programming language
class Zig < CodeGen
  After = Zsh
  File = "QR.zig"
  Cmd = "zig run QR.zig > OUTFILE"
  Apt = "zig"
  Code = %q(%(pub fn main()!void{_=try @import("std").io.getStdOut().writeAll#{E[PREV]};}))
end

# Elvish: expressive shell language
class Elvish < CodeGen
  After = Elixir
  File = "QR.elv"
  Cmd = "elvish QR.elv > OUTFILE"
  Apt = "elvish"
  Code = %q("print \"#{e[PREV]}\"")
end

# Hare: simple systems programming language
class Hare < CodeGen
  After = Groovy_Gzip
  File = "QR.ha"
  Cmd = "hare run QR.ha > OUTFILE"
  Apt = "hare"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        use fmt;
        export fn main() void = {
          fmt::print#{E[PREV]}!;
        };
      )
    END
  end
end

# Snek: embedded system programming language (Python-like)
# Note: has very limited string length (~256 bytes), cannot handle quine relay output.
#class Snek < CodeGen
#  After = Scilab_Sed_Shakespeare_SLang
#  File = "QR.snek"
#  Cmd = "snek QR.snek > OUTFILE"
#  Apt = "snek-bin"
#  Code = %q(%(print#{E[PREV]},end=""))
#end

# Maxima: computer algebra system (uses its own scripting language)
class Maxima < CodeGen
  After = Makefile
  File = "QR.mac"
  Cmd = "maxima --very-quiet < QR.mac > OUTFILE"
  Apt = "maxima"
  Code = %q(%(load(stringproc)$ display2d:false$ printf(true,"~a","#{e[PREV]}")$ quit();))
end

# CLIPS: rule-based expert system language
# Note: -f2 reads one form per line, and it reads stdin after the file.
class CLIPS < CodeGen
  After = CSharp_Chef
  Name = "CLIPS"
  File = "QR.clp"
  Cmd = "clips -f2 QR.clp < /dev/null > OUTFILE"
  Apt = "clips"
  Code = %q("(printout t \"#{e[PREV]}\")\n(exit)")
end

# Racket: modern Scheme-family language (but a distinct language, not just a Scheme)
# Note: while descended from Scheme, Racket is its own language with #lang.
class Racket < CodeGen
  After = Promela
  File = "QR.rkt"
  Cmd = "racket QR.rkt > OUTFILE"
  Apt = "racket"
  Code = %q("#lang racket/base\n(display \"#{e[PREV]}\")")
end

# newlisp: Lisp-like general purpose scripting language
# Note: distinct dialect, not Common Lisp or Scheme.
class Newlisp < CodeGen
  After = Neko
  Name = "newLISP"
  File = "QR.nl"
  Cmd = "newlisp -n QR.nl > OUTFILE"
  Apt = "newlisp"
  Code = %q("(print \"#{e[PREV]}\")(exit)")
end

# Excluded: not suitable for quine relay
# - Elm: no stdout I/O (web-only)
# - Coq, Agda: proof assistants, not general-purpose
# - Futhark: GPU-oriented, outputs arrays not strings
# - CafeOBJ: algebraic specification, not general-purpose
# - goo: appears abandoned, hard to script
# - golf: command not found / different purpose
# - chuck: audio programming language
# - basic256, brandy (BBC BASIC): banner/GUI issues
# - BWBASIC: banner to stdout
# - Aribas, Numbat: string literals have no escapes, so a " cannot be written

CodeGen::List.slice!(original_list_size..-1).each do |s|
  i = CodeGen::List.find_index(s::After)
  CodeGen::List.insert(i, s)
  [*s::Obsoletes].each {|s_| CodeGen::List.delete(s_) } if defined?(s::Obsoletes)
end
