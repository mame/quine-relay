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

CodeGen::List.slice!(original_list_size..-1).each do |s|
  i = CodeGen::List.find_index(s::After)
  CodeGen::List.insert(i, s)
  [*s::Obsoletes].each {|s_| CodeGen::List.delete(s_) } if defined?(s::Obsoletes)
end

CodeGen::List.delete(GeneratorScriptingLanguage)

class Zig < CodeGen
  File = "QR.zig"
  Cmd = "zig run QR.zig > OUTFILE"
  Apt = "zig"
  Code = %q(%(pub fn main()!void{_=try @import("std").io.getStdOut().write#{E[PREV]};}))
end

# aribas
# elvish
# hare
# raku
# snek
# storm-lang
