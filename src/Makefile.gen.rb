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

GBS := $(shell which gbs3 gbs2 2>/dev/null | head -1)
ifeq ($(GBS),)
  $(warning Gambas Script interpreter not found!)
endif

.DELETE_ON_ERROR:

SRCS = #{ RunSteps.map {|s| s.src }.join(" ") }

END
OUT << "all: QR2.rb"
banner("CHECK")
OUT << "\tdiff QR.rb QR2.rb"
OUT << ""
OUT << "SHA1SUMS: $(SRCS)"
OUT << "\tsha1sum -b $+ > $@"

[*RunSteps, RunStep["Ruby", "QR2.rb"]].each_cons(2).with_index do |(s1, s2), i|
  cmd = s1.cmd_make.gsub("OUTFILE", s2.src)

  OUT << ""
  OUT << "#{ s2.src }: #{ s1.src }"
  banner(s1.name, s2.name, i)
  OUT << "\t@mv #{ s1.backup } #{ s1.backup }.bak" if s1.backup
  cmd.split("&&").each {|c| OUT << "\t" + c.strip }
  OUT << "\t@mv #{ s1.backup }.bak #{ s1.backup }" if s1.backup
end

OUT << <<-END

clean:
\t@mv QR.rb quine-relay.rb
\trm -f qr QR qr.* QR.* QR2.rb *.class gst.im
\t@mv quine-relay.rb QR.rb
END

File.write("../Makefile", OUT.join("\n"))
