require_relative "code-gen"

OUT = []

def banner(s1, s2=nil, i=nil)
  s = "##  #{ "#{ i + 1 }: " if i }#{ s1 }#{ ' -> ' + s2 if s2 }  ##"
  OUT << "\t@echo"
  OUT << "\t@echo \"#{ "#" * s.size }\""
  OUT << "\t@echo \"#{ s }\""
  OUT << "\t@echo \"#{ "#" * s.size }\""
  OUT << "\t@echo"
end

OUT << <<-END
MAKEFLAGS += --no-print-directory

PATH := $(CURDIR)/vendor/local/bin:$(PATH)
CLASSPATH := .

JAVASCRIPT := $(shell which rhino nodejs node js 2>/dev/null | head -1)
ifeq ($(JAVASCRIPT),)
  $(warning JavaScript interpreter not found!)
endif

SCHEME := $(shell which guile csi gosh 2>/dev/null | head -1)
ifeq ($(SCHEME),)
  $(warning Scheme interpreter not found!)
endif
ifeq ($(SCHEME),$(shell which csi 2>/dev/null | head -1))
  SCHEME := csi -s
endif

BF := $(shell which bf beef 2>/dev/null | head -1)
ifeq ($(BF),)
  $(warning Brainfuck interpreter not found!)
endif

.DELETE_ON_ERROR:

END
OUT << "all: QR2.rb"
banner("CHECK")
OUT << "\tdiff QR.rb QR2.rb"

langs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.name } } + ["Ruby"]
cmds = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.cmd } }
srcs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.src } } + ["QR2.rb"]

cmds.size.times do |i|
  cmd = cmds[i].gsub("OUTFILE", srcs[i + 1])
  cmd = cmd.gsub("gosh", "$(SCHEME)")

  OUT << ""
  OUT << "#{ srcs[i + 1] }: #{ srcs[i] }"
  banner(langs[i], langs[i + 1], i)
  cmd.split("&&").each {|c| OUT << "\t" + c.strip }
end

OUT << <<-END

clean:
\t@mv QR.rb quine-relay.rb
\trm -f qr QR qr.* QR.* QR2.rb *.class gst.im
\t@mv quine-relay.rb QR.rb
END

File.write("../Makefile", OUT.join("\n"))
