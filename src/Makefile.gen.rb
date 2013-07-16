require_relative "code-gen"

OUT = []

def banner(s)
  s = "##  #{ s }  ##"
  OUT << "\t@echo"
  OUT << "\t@echo \"#{ "#" * s.size }\""
  OUT << "\t@echo \"#{ s }\""
  OUT << "\t@echo \"#{ "#" * s.size }\""
  OUT << "\t@echo"
end

OUT << "MAKEFLAGS += --no-print-directory"
OUT << ""
OUT << "all: QR2.rb"
banner("CHECK")
OUT << "\tdiff QR.rb QR2.rb"

langs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.name } } + ["Ruby"]
cmds = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.cmd } }
exts = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.ext } } + [".rb"]

cmds.size.times do |i|
  dst = "QR#{ exts[i + 1] }"
  dst = "QR2.rb" if dst == "QR.rb"
  cmd = cmds[i].gsub("OUTFILE", dst)

  OUT << ""
  OUT << "#{ dst }: QR#{ exts[i] }"
  banner(langs[i] + " -> " + langs[i + 1])
  cmd.split("&&").each {|c| OUT << "\t" + c.strip }
end

File.write("../Makefile", OUT.join("\n") + "\n")
