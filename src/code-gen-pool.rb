original_list_size = CodeGen::List.size

#class Nim_NVSPL2 < CodeGen
#  After = Nim
#  Obsoletes = Nim
#  File = ["QR.nim", "QR.nvspl2"]
#  Cmd = ["nim c QR.nim && ./QR > OUTFILE", "ruby vendor/nvspl2.rb QR.nvspl2 > OUTFILE"]
#  Apt = ["nim", nil]
#  Code = %q(%((for i, c in#{E[PREV]}:echo ",",int(c),"CO");echo "Q"))
#end

#class Curry < CodeGen
#  After = CommonLisp
#  File = "QR.curry"
#  Cmd = "touch ~/.pakcsrc && runcurry QR.curry > OUTFILE"
#  Apt = "pakcs"
#  Code = %q("main=putStr"+E[PREV])
#end

CodeGen::List.slice!(original_list_size..-1).each do |s|
  i = CodeGen::List.find_index(s::After)
  CodeGen::List.insert(i, s)
  [*s::Obsoletes].each {|s_| CodeGen::List.delete(s_) } if defined?(s::Obsoletes)
end
