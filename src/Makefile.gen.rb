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

find_any0 = $(firstword $(foreach x,$(1),$(if $(shell which $(x) 2>/dev/null),$(x),)))
check = $(if $(2),$(2),$(error $(1) interpreter not found!))
find_any = $(call check,$(1),$(call find_any0,$(2)))

JAVASCRIPT := $(call find_any,JavaScript,nodejs node js)
SCHEME     := $(call find_any,Scheme,guile csi gosh)
BF         := $(call find_any,Brainfuck,bf beef)
GBS        := $(call find_any,Gambas script,gbs3 gbs2)

ifeq ($(SCHEME),csi)
  SCHEME := csi -s
endif
ifeq ($(BF),bf)
  BF := bf -c500000
endif

.DELETE_ON_ERROR:
END
OUT << "all: QR2.rb"
banner("CHECK")
OUT << "\tdiff -s QR.rb QR2.rb"
OUT << ""
OUT << "check: all"
OUT << "\t@sha1sum --quiet -c SHA1SUMS"

[*RunSteps, RunStep["Ruby", "QR2.rb"]].each_cons(2).with_index do |(s1, s2), i|
  cmd = s1.cmd_make.gsub("OUTFILE", s2.src)

  OUT << ""
  OUT << "#{ s2.src }: #{ s1.src }"
  banner(s1.name, s2.name, i)
  OUT << "\t@mv #{ s1.backup } #{ s1.backup }.bak" if s1.backup
  cmd.split("&&").each {|c| OUT << "\t" + c.strip.gsub(/^!/, "ulimit -s unlimited && ") }
  OUT << "\t@mv #{ s1.backup }.bak #{ s1.backup }" if s1.backup
end

OUT << <<-END

clean:
\t@mv QR.rb quine-relay.rb
\trm -f qr QR qr.* QR.* QR2.rb *.class gst.im
\t@mv quine-relay.rb QR.rb
END

File.write("../Makefile", OUT.join("\n"))
