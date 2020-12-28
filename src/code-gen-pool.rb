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

# dhall (need "dhall text" subcommand to show arbitrary text since dhall-1.25.0)

# G-Portugol
class GolfScript_GPortugol_Grass < CodeGen
  After = Go
  Obsoletes = GolfScript_Grass
  Name = ["GolfScript", "G-Portugol", "Grass"]
  File = ["QR.gs", "QR.gpt", "QR.grass"]
  Cmd = ["ruby vendor/golfscript.rb QR.gs > OUTFILE", "gpt -o QR QR.gpt && ./QR > OUTFILE", "ruby vendor/grass.rb QR.grass > OUTFILE"]
  Apt = [nil, "gpt", nil]
  def code
    r = <<-'END'.lines.map {|l| l.strip }.join
      %(
        @@BASE@@:j;
        {
          119:i;
          {
            206i-:i;
            .48<{71+}{[i]\\48-*}if
          }%
        }:t;
        "algoritmo QR;in"[195][173]++'cio imprima("'
        @@PROLOGUE@@
        "#{e[PREV]}"
        {
          "W""w"@j 1+:j\\- @@MOD@@%1+*
        }%
        @@EPILOGUE@@
        '");fim'
      )
    END
    mod, prologue, epilogue = ::File.read(::File.join(__dir__, "grass-boot.dat")).lines
    prologue += "t"
    epilogue += "t"
    prologue = prologue.gsub(/(\/12131)+/) { "\"t\"/12131\"t #{ $&.size / 6 }*\"" }
    mod = mod.to_i
    r.gsub(/@@\w+@@/, {
      "@@PROLOGUE@@" => prologue.chomp,
      "@@EPILOGUE@@" => epilogue.chomp,
      "@@BASE@@" => 119 + mod - 1,
      "@@MOD@@" => mod,
    })
  end
end

CodeGen::List.slice!(original_list_size..-1).each do |s|
  i = CodeGen::List.find_index(s::After)
  CodeGen::List.insert(i, s)
  [*s::Obsoletes].each {|s_| CodeGen::List.delete(s_) } if defined?(s::Obsoletes)
end

CodeGen::List.delete(GeneratorScriptingLanguage)
