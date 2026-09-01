# A source for generating Quine Relay

GenStep = Struct.new(:name, :code, :run_steps, :check)
RunStep = Struct.new(:name, :src, :cmd_make, :cmd_raw, :backup, :apt)

# properly nested delimiters
PARENS   = /\A(?<b>[^()]*+(?:\(\g<b>\)[^()]*+)*+)\z/
BRACKETS = /\A(?<b>[^\[\]]*+(?:\[\g<b>\][^\[\]]*+)*+)\z/

# A class that generates Ruby code that generates a code (in a language X) that prints PREV.
class CodeGen
  # File = source file name of X
  # Cmd = command for execution/compilation
  # Apt = ubuntu package name of the interpreter/compiler of X

  List = []
  def self.inherited(c)
    List << c
  end

  def self.gen_step
    gen = new
    GenStep[name, gen.code, run_steps, gen.method(:check).to_proc]
  end

  def code
    self.class::Code
  end

  # check if PREV satisfies the assumption of the CodeGen
  def check(prev)
  end

  # The same escaping as e[] of GenPrologue, for check
  def escape(s)
    s.gsub(/[\\"]/) { "\\" + $& }.gsub("\n", "\\n")
  end

  def self.run_steps
    a = []
    a << (defined?(self::Name) ? [*self::Name] : self.to_s.split("_"))
    a << [*self::File]
    a << [*self::Cmd]
    a << (defined?(self::Backup) ? [*self::Backup] : [*self::Cmd].map { nil })
    a << [*self::Apt]
    a.transpose.map do |name, src, cmd_make, backup, apt|
      cmd_raw = cmd_make
      cmd_raw = cmd_raw.gsub("$(SCHEME)", "guile")
      cmd_raw = cmd_raw.gsub("$(JAVASCRIPT)", "nodejs")
      cmd_raw = cmd_raw.gsub("$(CC)", "gcc")
      cmd_raw = cmd_raw.gsub("$(CXX)", "g++")
      cmd_raw = cmd_raw.gsub("$(GBS)", "gbs3")
      RunStep[name, src, cmd_make, cmd_raw, backup, apt]
    end
  end
end

# Common part
GenPrologue = <<-'END'.lines.map {|l| l.strip }.join
  B=92.chr;
  g=32.chr;
  N=10.chr;
  n=0;
  e=->s{Q[Q[s,B],?"].gsub(N,B+?n)};
  E=->s{'("'+e[s]+'")'};
  d=->s,t=?"{s.gsub(t){t+t}};
  def f(s,n)s.gsub(/.{1,#{n*255}}/m){yield$S=E[$s=$&]}end;
  Q=->s,t=?${s.gsub(t){B+$&}};
  R=";return 0;";
  V=->s,a,z{s.gsub(/(#{B*4})+/){a+"#{$&.size/2}"+z}};
  C=%w(System.Console Write);
  $C=C*?.;
  $D="program QR";
  $L="public static";
  $W="s.WriteByte";
  rp=->s,r{
    v="";
    [r.inject(s){|s,j|
      o={};
      m=n=0;
      s.size.times{|i|
        o[f=s[i,2]]||=0;
        c=o[f]+=1;
        m<c&&(m=c;n=f)
      };
      v=n+v;
      s.gsub(n,(j%256).chr)
    },v]
  };
END
# rp: Re-Pair (Naive byte pair encoding)

class Python_R_Ratfor_Rc_REXX < CodeGen
  Name = ["Python", "R", "Ratfor", "rc", "REXX"]
  File = ["QR.py", "QR.R", "QR.ratfor", "QR.rc", "QR.rexx"]
  Cmd = [
    "python3 QR.py > OUTFILE",
    "R --slave -f QR.R > OUTFILE",
    "ratfor -o QR.ratfor.f QR.ratfor && gfortran -o QR QR.ratfor.f && ./QR > OUTFILE",
    "rc QR.rc > OUTFILE",
    "rexx ./QR.rexx > OUTFILE"
  ]
  Apt = ["python3", "r-base", "ratfor", "rc", "regina-rexx"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        print('cat("')\n
        for l in#{E[d[d[PREV,?'],?']]}.split("\\n"):
          [print('r=fput(char(%d))'%ord(c))for c in"echo 'say ''%s'''\\n"%l]\n
        print('end\\n")')#
      )
    END
  end

  def check(prev)
    # python3 reads the source as UTF-8 and hands ord(c) to Fortran's char(), which takes 0..255
    raise unless prev.bytes.all? {|c| c < 128 }
  end
end

class Promela < CodeGen
  Name = "Promela (Spin)"
  File = "QR.pr"
  Cmd = "spin -T QR.pr > OUTFILE"
  Apt = "spin"
  Code = %q("init{#{f(PREV,6){"printf#{d[$S,?%]};"}}}")

  def check(prev)
    # spin appends "1 process created" to stdout on the same line, and the "#" comments it out
    raise unless prev.end_with?(?#)
  end
end

class Prolog < CodeGen
  File = "QR.prolog"
  Cmd = "swipl -q -t qr -f QR.prolog > OUTFILE"
  Apt = "swi-prolog"
  Code = %q("qr:-write#{E[PREV]}.")
end

class PostScript < CodeGen
  Name = "PostScript"
  File = "QR.ps"
  Cmd = "gs -dNODISPLAY -q QR.ps > OUTFILE"
  Apt = "ghostscript"
  Code = %q("(#{Q[PREV,B]})print quit")

  def check(prev)
    # a PostScript (...) literal counts nested parens, and only the backslash is escaped here
    raise unless prev.match?(PARENS)
  end
end

class Pike < CodeGen
  File = "QR.pike"
  Cmd = "pike QR.pike > OUTFILE"
  Apt = "pike8.0"
  Code = %q("int main(){write#{E[PREV]};}")

  def check(prev)
    # the Piet image reads a byte as a column height, and Perl 5's Re-Pair takes 0-31 and 128-255
    raise unless prev.bytes.all? {|c| c == 10 || (0x20..0x7e).cover?(c) }
  end
end

class PHP_Piet < CodeGen
  File = ["QR.php", "QR.png"]
  Cmd = ["php QR.php > OUTFILE", "npiet QR.png > OUTFILE"]
  Apt = ["php-cli", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        <?php $z=3+$w=strlen($s=#{Q[E[PREV]]})*3;
          echo"\\x89PNG\\r\\n\\32\\n";
          $m="";
          $t="\\xc0\\0\\xff";
          for($i=-1;++$i<$z<<7;
              $m.=$c--?
                ($w-$c||$i>$z)&&$i/$z<($c<$w?ord($s[$c/3|0]):$c--%3+2)?
                  $t[2].$t[$c%3%2].$t[$c%3]:"\\0\\0\\0":"\\0"
          )
            $c=$i%$z;
          foreach([
            "IHDR".pack("NNCV",$w+2,128,8,2),
            "IDAT".gzcompress($m),
            "IEND"
          ]as$d)
            echo pack("NA*N",strlen($d)-4,$d,crc32($d));
      )
    END
  end

  def check(prev)
    # npiet reads a byte as a column height in a 128-row image, and the PHP source carries it raw
    raise unless prev.bytes.all? {|c| c == 10 || (0x20..0x7e).cover?(c) }
  end
end

class Perl6 < CodeGen
  Name = "Perl 6"
  File = "QR.pl6"
  Cmd = "perl6 QR.pl6 > OUTFILE"
  Apt = "rakudo"
  Code = %q("print '#{Q[d[PREV,B],?']}'")

  def check(prev)
    # it goes raw into a '...' literal, and Perl 5's Re-Pair takes bytes 0-31 and 128-255
    raise unless prev.bytes.all? {|c| (0x20..0x7e).cover?(c) }
  end
end

class Perl5 < CodeGen
  Name = "Perl 5"
  File = "QR.pl"
  Cmd = "perl QR.pl > OUTFILE"
  Apt = "perl"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      (
        p="eval";
        %(
          $_="#{
            s,v=rp[PREV,128..287];
            ["
              $_='#{Q[s,c=/['\\\\]/]}';
              $n=32;
              for$a(unpack'(a2)*','#{Q[v,c]}'){
                $b=chr(--$n&255);
                s/$b/$a/g
              }
              print
            "].pack("u").tr(" -a",",-:A-Za-y")
          }";
          tr/,-:A-Za-y/ -a/;
          eval unpack u
        ).scan(/[ ,-:A-z]+|(.)/m){p="s++#{$1?"chr #{$1.ord}+e":$&+?+};"+p};
        p
      )
    END
  end

  def check(prev)
    # rp[] allocates its Re-Pair codewords from bytes 0-31 and 128-255
    raise unless prev.bytes.all? {|c| (0x20..0x7f).cover?(c) }
  end
end

class Pascal < CodeGen
  File = "QR.pas"
  Cmd = "fpc QR.pas && ./QR > OUTFILE"
  Apt = "fp-compiler"
  Code = %q("begin write('#{PREV}')end.")

  def check(prev)
    # it goes into a '...' literal unescaped, and the literal cannot span lines
    raise if prev =~ /['\n]/
  end
end

class Parser3 < CodeGen
  Name = "Parser 3"
  File = "QR.p"
  Cmd = "parser3 QR.p > OUTFILE"
  Apt = "parser3-cgi"
  Code = %q("$console:line[#{PREV.gsub(?;,"^;")}]")

  def check(prev)
    # "$" starts a variable reference and "^" is the escape character, but only ";" is escaped here
    raise if prev =~ /[$^]/
    raise if prev.include?("\r") # parser3 turns it into a newline
    # it is placed inside $console:line[...], and brackets cannot be escaped
    raise unless prev.match?(BRACKETS)
  end
end

class PARIGP < CodeGen
  Name = "PARI/GP"
  File = "QR.gp"
  Cmd = "gp -f -q QR.gp > OUTFILE"
  Apt = "pari-gp"
  Code = %q("print#{E[PREV]};quit")
end

class Octave_Ook < CodeGen
  Name = ["Octave", "Ook!"]
  File = ["QR.octave", "QR.ook"]
  Cmd = [
    "mv QR.m QR.m.bak && octave -qf QR.octave > OUTFILE && mv QR.m.bak QR.m",
    "ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf && ruby vendor/bf.rb QR.ook.bf > OUTFILE"
  ]
  Apt = ["octave", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        puts([arrayfun(@(k)
            [repmat(['Ook' char(46-13*(k<0)) ' '],1,2*abs(k)) 'Ook! Ook. '],
            diff([0 #{E[PREV]}]+0),'un',0){:}]);
      "
    END
  end
end

class OCaml < CodeGen
  File = "QR.ml"
  Cmd = "ocaml QR.ml > OUTFILE"
  Apt = "ocaml"
  Code = %q("print_string#{E[PREV]}")
end

class ObjC < CodeGen
  Name = "Objective-C"
  File = "QR.m"
  Cmd = "gcc -o QR QR.m && ./QR > OUTFILE"
  Apt = "gobjc"
  Code = %q("#import<stdio.h>#{N}int main(){puts#{E[PREV]+R}}")
end

class Nim < CodeGen
  File = "QR.nim"
  Cmd = "nim compile QR.nim && ./QR > OUTFILE"
  Apt = "nim"
  Code = %q("echo#{E[PREV]}")
end

class Nickle < CodeGen
  File = "QR.5c"
  Cmd = "nickle QR.5c > OUTFILE"
  Apt = "nickle"
  Code = %q("printf#{E[PREV]};")

  def check(prev)
    # it becomes printf's format string
    raise if prev.include?(?%)
  end
end

class Neko < CodeGen
  File = "QR.neko"
  Cmd = "nekoc QR.neko && neko QR.n > OUTFILE"
  Apt = "neko"
  Code = %q("$print#{E[PREV]}")
end

class Mustache_NASM < CodeGen
  File = ["QR.mustache", "QR.asm"]
  Cmd = [
    "mustache QR.mustache QR.mustache > OUTFILE",
    "nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > OUTFILE",
  ]
  Apt = ["ruby-mustache", "nasm"]
  def code
    <<-'END'.lines.map {|l| l.strip.gsub("^^^", " ") }.join("\\n")
      "_start{{!:
      q: |
      ^^^:mov edx,z-d
      ^^^global _start
      ^^^mov ecx,d
      ^^^inc ebx
      ^^^mov al,4
      ^^^int 128
      ^^^mov eax,1
      ^^^dec ebx
      ^^^int 128
      ^^^d:db\x60#{e[PREV]}\x60
      ^^^z:;}}{{{q}}}"
    END
  end

  def check(prev)
    raise if prev.include?(?`) # it closes NASM's `...` string, and e[] does not escape it
    # it closes the {{! ... }} comment that hides the YAML from the mustache pass
    raise if prev.include?("}}")
  end
end

class MSIL < CodeGen
  File = "QR.il"
  Cmd = "ilasm QR.il && mono QR.exe > OUTFILE"
  Apt = "mono-devel"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        .assembly t{}
        .method #$L void m()
        {
          .entrypoint ldstr"#{e[PREV]}"
          call void [mscorlib]#{C*"::"}(string)
          ret
        }
      )
    END
  end
end

class Modula2 < CodeGen
  Name = "Modula-2"
  File = "QR.mod"
  Cmd = "gm2 -fiso QR.mod -o QR && ./QR > OUTFILE"
  Apt = "gm2"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        MODULE Q;
        IMPORT StrIO;
        BEGIN #{
          PREV.gsub(/()[#{i=?^}"]+|[#{i}']+/){
            ["StrIO.WriteString(",$&,");"]*($1??":?')
          }
        }END Q.
      "
    END
  end

  def check(prev)
    # it is cut into "..." and '...' literals without escaping, and neither can span lines
    raise if prev.include?("\n")
  end
end

class MiniZinc < CodeGen
  File = "QR.mzn"
  Cmd = "minizinc --solver COIN-BC --soln-sep '' QR.mzn > OUTFILE"
  Apt = "minizinc"
  Code = %q("output#{E[PREV]}")
end

class Makefile < CodeGen
  File = "QR.mk"
  Cmd = "make -f QR.mk > OUTFILE"
  Apt = "make"
  Code = %q("a:;@echo '#{d[PREV,?$].gsub(?'){"'\\\\''"}}'")

  def check(prev)
    raise if prev.include?("\n") # the whole payload is one echo recipe line
  end
end

class M4 < CodeGen
  File = "QR.m4"
  Cmd = "m4 QR.m4 > OUTFILE"
  Apt = "m4"
  Code = %q("changequote(<@,@>)<@#{PREV}@>")

  def check(prev)
    # they are the quote delimiters set by changequote, and m4 quotes nest
    raise if prev.include?("<@") || prev.include?("@>")
  end
end

class Lua < CodeGen
  File = "QR.lua"
  Cmd = "lua5.3 QR.lua > OUTFILE"
  Apt = "lua5.3"
  Code = %q("print((#{V[E[PREV],?&,?&]}:gsub('&(%d+)&',load[[return('\\\\\\\\'):rep(...)]])))")

  def check(prev)
    raise if prev.include?(?&) # it delimits the backslash run-length encoding "&n&"
  end
end

class LOLCODE < CodeGen
  File = "QR.lol"
  Cmd = "lci QR.lol > OUTFILE"
  Apt = [nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        HAI 1,
        VISIBLE "#{
          PREV.gsub(/[:"]/,":\\0")
        }",
        KTHXBYE
      )
    END
  end

  def check(prev)
    raise if prev.include?("\n") # lci's VISIBLE "..." cannot span lines
  end
end

class LLVMAsm < CodeGen
  Name = "LLVM asm"
  File = "QR.ll"
  Cmd = "llvm-as QR.ll && lli QR.bc > OUTFILE"
  Apt = "llvm"
  Backup = "QR.bc"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        @s=global[#{(s=PREV).size+1}x i8]
          c"#{s.gsub(/[\\"\n\t]/){"\\%02x"%$&.ord}}\\00"
        declare i32@puts(ptr)
        define i32@main(){
          call i32@puts(ptr@s)
          ret i32 0
        }
      )
    END
  end

  def check(prev)
    raise if prev.include?("\0") # @s is a NUL-terminated C string printed by puts
  end
end

class Ksh_LazyK_LiveScript < CodeGen
  Name = ["ksh", "Lazy K", "LiveScript"]
  File = ["QR.ksh", "QR.lazy", "QR.ls"]
  Cmd = [
    "ksh QR.ksh > OUTFILE",
    "lazyk QR.lazy > OUTFILE",
    "lsc QR.ls > OUTFILE",
  ]
  Apt = ["ksh", nil, "livescript"]
  Backup = [nil, nil, "QR.c"]
  def code
    lazyk = ::File.read(::File.join(__dir__, "lazyk-boot.dat"))
    lazyk = lazyk.tr("ski`","0123")
    lazyk += "0" * (-lazyk.size % 3)
    lazyk = lazyk.scan(/.{3}/).map do |n|
      [*93..124,*42..73][n.to_i(4)]
    end.pack("C*")
    lazyk = lazyk.gsub(/[ZHJK\^`~X]/) {|c| "\\x%02x" % c.ord }
    <<-'END'.lines.map {|l| l.strip }.join.sub("LAZYK"){lazyk}
      %(
        p(){ print -rn $1;};
        f(){ for x in $(p "$1"|od -vAn -tu1);do;
            p $4;
            for((j=$3;j--;));do;
              h $2 $x $j;
            done;
          done;
        };
        p k\\`;
        h(){ p \\`${1:$2>>$3&1:2};};
        f 'console.log#{Q[E[PREV],?#].gsub(?',%('"'"'))}' kki 7 '#{"``s"*8}i';
        h(){ p ${1:$2%83-10>>$3*2&3:1};};
        f 'LAZYK' ski\\` 3
      )
    END
  end

  def check(prev)
    # p(){ print -rn $1;} expands $1 unquoted, so IFS collapses each run of whitespace into one space
    raise if prev =~ /\t|  /
  end
end

class Kotlin < CodeGen
  File = "QR.kt"
  Cmd = "kotlinc QR.kt -include-runtime -d QR.jar && kotlin QR.jar > OUTFILE"
  Apt = "kotlin"
  Code = %q("fun main()=print#{Q[E[PREV]]}")
end

class JavaScript_Jq_JSFuck < CodeGen
  File = ["QR.js", "QR.jq", "QR.jsfuck"]
  Cmd = [
    "$(JAVASCRIPT) QR.js > OUTFILE",
    "jq -r -n -f QR.jq > OUTFILE",
    "!$(JAVASCRIPT) --stack_size=100000 QR.jsfuck > OUTFILE",
  ]
  Apt = ["nodejs", "jq", "nodejs"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        P={0:'[+[]]'};
        for(R of
          (
            '![]@!![]@'+
            (A="[]['at']")+
            "@([]+[])['link']()@(+('11e20')+[])['split']([])@(+[])"+
            (C="['constructor']")
          ).split('@')
        )
          for(E in D=eval(G='('+R+'+[])'))
            P[D[E]]||=G+"['"+E+"']";
        for(G='[',B=0;++B<36;)
          P[D=B.toString(36)]=
            B<10?
              (G+='+!+[]')+']'
            :
              P[D]||"(+('"+B+"'))['to'+([]+[])"+C+"['name']]('36')";
        A+=C+"('console.log(unescape(\\"";
        for(E of #{E[PREV]})
          A+="'+![]+'"+E.charCodeAt().toString(16);
        for(A+="\\".replace(/false/g,escape(\\" \\")[0])))')()",R=9;R--;)
          A=A.replace(/'(.*?)'/g,(B,D)=>[...D].map(E=>P[E]).join('+'));
        console.log('"'+A+'"')
      )
    END
  end

  def check(prev)
    # JSFuck encodes each byte as %XX with charCodeAt().toString(16), which is not zero-padded,
    # and unescape() cannot yield a non-ASCII byte
    raise if prev.bytes.any? {|b| b < 0x10 || b > 0x7f }
  end
end

class Java_ < CodeGen
  Name = "Java"
  File = "QR.java"
  Cmd = "javac QR.java && java QR > OUTFILE"
  Apt = "openjdk-25-jdk"
  def code
    # LZ78-like compression
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        void main(){
            String c[]=new String[99999],z="",s="#{
              z=t=(0..r=q=127).map{|n|[n,[]]};
              a="";
              b=->n{a<<(n%78+55)%84+42};
              (PREV).bytes{|n|
                r,z=z[n]||(
                  b[r/78];b[r];
                  q<6083&&z[n]=[q+=1,[]];
                  t[n])
              };
              b[r/78];b[r]
            }";
            int i=0,n=0,q=0;
            for(;++n<127;)c[n]=""+(char)n;
            for(;i<#{a.size};){
              q=q*78+(s.charAt(i)-13)%84;
              if(i++%2>0){
                c[n]=z+(q<n++?c[q]:z).charAt(0);
                IO.print(z=c[q]);
                q=0;
              }
            }
          }
      )
    END
  end

  def check(prev)
    raise if prev.bytes.any? {|b| b < 1 || b > 126 } # the LZ78 dictionary is seeded with c[1..126]
  end
end

class Jasmin < CodeGen
  File = "QR.j"
  Cmd = "jasmin QR.j && java QR > OUTFILE"
  Apt = "jasmin-sable"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        .class QR\n
        .super #{S="java/lang/"}Object\n
        .method #$L main()V\n
        .limit stack 1\n
        ldc "#{e[PREV]}"\n
        invokestatic #{S}IO/print(L#{S}Object;)V\n
        return\n
        .end method
      )
    END
  end

  def check(prev)
    raise if prev.bytesize > 65535 # it becomes one JVM CONSTANT_Utf8 via ldc
    # the dc stage carries the whole program inside a [...] string, which counts nesting
    raise unless prev.match?(BRACKETS)
  end
end

class Icon_INTERCAL < CodeGen
  File = ["QR.icn", "QR.i"]
  Cmd = [
    "icont -s QR.icn && ./QR > OUTFILE",
    "ick -bfOc QR.i && gcc -std=c99 -static QR.c -I /usr/include/ick-* -o QR -lick && ./QR > OUTFILE"
  ]
  Backup = [nil, "QR.c"]
  Apt = [["icont", "iconx"], "intercal"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        procedure main();
          i:=c:=0;
          s:=#{E[PREV+N]};
          write("DO,1<-#",*s);
          every t:=ord(!s)do{
            i+:=1;
            u:=-i;
            every 0to 7do{u:=u*2+t%2;t/:=2};
            write("PLEASE")\\(i%4/3);
            write("DO,1SUB#",i,"<-#",(c-u)%256);
            c:=u;
          };
          write("PLEASEREADOUT,1PLEASEGIVEUP");
        end
      )
    END
  end

  def check(prev)
    # the length is emitted as the INTERCAL constant DO,1<-#N, and constants are 16bit
    raise if prev.bytesize + 1 > 65535
  end
end

class Haxe < CodeGen
  File = "QR.hx"
  Cmd = "haxe -main QR -neko QR.n && neko QR.n > OUTFILE"
  Apt = "haxe"
  Code = %q("class QR{#$L function main()Sys.print#{E[PREV]};}")
end

class Haskell < CodeGen
  File = "QR.hs"
  Cmd = "rm -f QR.o && ghc QR.hs && ./QR > OUTFILE"
  Apt = "ghc"
  Code = %q("main=putStr"+E[PREV])

  def check(prev)
    # only \, " and newline are escaped, and GHC rejects a raw control character in a literal
    raise if prev =~ /[^\n\x20-\x7e]/
  end
end

class Groovy_Gzip < CodeGen
  File = ["QR.groovy", "QR.gz"]
  Cmd = ["groovy QR.groovy > OUTFILE", "gzip -cd QR.gz > OUTFILE"]
  Apt = ["groovy", "gzip"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        z=new java.util.zip.GZIPOutputStream(System.out);
        z<<'#{PREV.tr(?"+B,"&_")}'.tr('_&','\\\\"');
        z.close()
      )
    END
  end

  def check(prev)
    raise if prev.include?(?') # it would close Groovy's '...' literal
    raise if prev =~ /[&_]/ # they are the placeholders that '"' and "\\" are moved to
    raise if prev.include?("\n") # Groovy's '...' literal cannot span lines
  end
end

class GolfScript_GPortugol_Grass < CodeGen
  Name = ["GolfScript", "G-Portugol", "Grass"]
  File = ["QR.gs", "QR.gpt", "QR.grass"]
  Cmd = ["ruby vendor/golfscript.rb QR.gs > OUTFILE", "mv QR.c QR.c.bak && gpt -t QR.c QR.gpt && gcc -o QR QR.c && ./QR > OUTFILE && mv QR.c.bak QR.c", "ruby vendor/grass.rb QR.grass > OUTFILE"]
  Apt = [nil, "gpt", nil]
  def code
    r = <<-'END'.lines.map {|l| l.strip }.join
      %(
        @@BASE@@:j;
        {
          119:i;
          {
            206i-:i;
            .43<{76+}{[i]\\43-*}if
          }%
        }:t;
        "algoritmo A;in\\u00edcio imprima(\\""
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
    prologue = prologue.gsub(/(\*,-,\.,)+/) { "\"t\"*,-,.,\"#{ $&.size / 6 }*t\"" }
    mod = mod.to_i
    r.gsub(/@@\w+@@/, {
      "@@PROLOGUE@@" => prologue.chomp,
      "@@EPILOGUE@@" => epilogue.chomp,
      "@@BASE@@" => 119 + mod - 1,
      "@@MOD@@" => mod,
    })
  end

  def check(prev)
    # vendor/golfscript.rb passes a "..." literal to Ruby's eval, so these would interpolate
    raise if prev =~ /#[{$@]/
  end
end

class Go < CodeGen
  File = "QR.go"
  Cmd = "go run QR.go > OUTFILE"
  Apt = "golang"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        package main;
        import"fmt";
        func main(){
          fmt.Print#{E[PREV]}
        }
      )
    END
  end

  def check(prev)
    # the Go compiler rejects a source file that is not valid UTF-8
    raise unless prev.dup.force_encoding("UTF-8").valid_encoding?
  end
end

class Gnuplot < CodeGen
  File = "QR.plt"
  Cmd = "gnuplot QR.plt > OUTFILE"
  Apt = "gnuplot"
  Code = %q('set print"-";print'+E[PREV])
end

class GEL < CodeGen
  Name = "GEL (Genius)"
  File = "QR.gel"
  Cmd = "genius QR.gel > OUTFILE"
  Apt = "genius"
  Code = %q(f(PREV,61){"printn#$S;"})

  def check(prev)
    # genius is flex-based and its scanner overflows above 16383 bytes per token
    prev.scan(/.{1,#{61*255}}/m) do |s|
      raise if s.size + s.scan(/[\\"\n]/).size + 2 > 16383
    end
  end
end

class GDB < CodeGen
  File = "QR.gdb"
  Cmd = "gdb -q -x QR.gdb > OUTFILE"
  Apt = "gdb"
  Code = %q('echo '+e[PREV]+"\nquit")
end

class GAP < CodeGen
  File = "QR.g"
  Cmd = "gap -q QR.g > OUTFILE"
  Apt = "gap"
  Code = %q("WriteAll(OutputTextUser(),#{E[PREV]});QUIT;")
end

class Gambas < CodeGen
  Name = "Gambas script"
  File = "QR.gbs"
  Cmd = "$(GBS) QR.gbs > OUTFILE"
  Apt = [["gambas3-scripter", "gambas3-gb-pcre"]]
  Code = %q(%(print"#{e[PREV]}"))
end

class Forth_FORTRAN77_Fortran90 < CodeGen
  File = ["QR.fs", "QR.f", "QR.f90"]
  Cmd = [
    "gforth QR.fs > OUTFILE",
    "gfortran -o QR QR.f && ./QR > OUTFILE",
    "gfortran -o QR QR.f90 && ./QR > OUTFILE"
  ]
  Backup = [nil, "QR.c", nil]
  Apt = ["gforth", "f2c", "gfortran"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join(" ")
      %(
        : B ."#{g*9}print*,'" ;
        : C B type ." '" CR ;
        : D
          S" print ''(*(A))'',&" C
          S\\" #{e[PREV]}" 0 DO B ." char(" COUNT . ." ),&'" CR LOOP
          S" '''';end" C
          ."#{g*9}end" CR BYE ;
        D
      )
    END
  end
end

class Fish < CodeGen
  File = "QR.fish"
  Cmd = "fish QR.fish > OUTFILE"
  Apt = "fish"
  Code = %q("echo '#{Q[PREV,/['\\\\]/]}'")
end

class Flex < CodeGen
  File = "QR.fl"
  Cmd = "flex -o QR.fl.c QR.fl && gcc -o QR QR.fl.c && ./QR > OUTFILE"
  Apt = "flex"
  Code = %q("%option noyywrap\n int main(){puts#{E[PREV]};}\n%%")

  def check(prev)
    raise if prev.include?("\0") # it becomes a C string literal printed by puts
  end
end

class Fennel < CodeGen
  File = "QR.fnl"
  Cmd = "fennel QR.fnl > OUTFILE"
  Apt = "fennel"
  Code = %q("(io.write \"#{e[PREV]}\")")
end

class FALSELang < CodeGen
  Name = "FALSE"
  File = "QR.false"
  Cmd = "ruby vendor/false.rb QR.false > OUTFILE"
  Apt = [nil]
  Code = %q(?"+PREV.gsub(?",'"34,"')+?")
end

class FSharp < CodeGen
  Name = "F#"
  File = "QR.fsx"
  fsproj = <<-END
    <Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net10.0</TargetFramework>
        <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
      </PropertyGroup>
      <ItemGroup>
        <Compile Include="QR.fsx" />
      </ItemGroup>
    </Project>
  END
  Cmd = %(echo '#{ fsproj.lines.map {|s| s.strip }.join }' > tmp.fsproj && DOTNET_NOLOGO=1 dotnet run --project tmp.fsproj > OUTFILE)
  Apt = "dotnet-sdk-10.0"
  Code = %q(%(#$C """#{PREV} """))

  def check(prev)
    raise if prev.include?('"""') # it closes the triple-quoted string
  end
end

class Execline < CodeGen
  File = "QR.e"
  Cmd = "execlineb QR.e > OUTFILE"
  Apt = "execline"
  Code = %q(%(echo "#{e[PREV]}"))

  def check(prev)
    # MAX_ARG_STRLEN, the Linux limit on one argument; echo takes PREV as one argument
    raise if prev.bytesize >= 131072
  end
end

class Erlang < CodeGen
  File = "QR.erl"
  Cmd = "escript QR.erl > OUTFILE"
  Apt = "erlang"
  Code = %q("\nmain(_)->io:put_chars#{E[PREV]}.")
end

class EmacsLisp < CodeGen
  Name = "Emacs Lisp"
  File = "QR.el"
  Cmd = "emacs -Q --script QR.el > OUTFILE"
  Apt = "emacs-nox"
  Code = %q(%((princ"#{e[PREV]}")))
end

class Elixir < CodeGen
  File = "QR.exs"
  Cmd = "elixir QR.exs > OUTFILE"
  Apt = "elixir"
  Code = %q("IO.write#{E[PREV]}")

  def check(prev)
    raise if prev.include?('#{') # Elixir interpolates it in a "" string
  end
end

#class Dhall < CodeGen
#  Name = "dhall"
#  File = "QR.dhall"
#  Cmd = "dhall text --file QR.dhall > OUTFILE"
#  Apt = "dhall"
#  Code = %q(E[PREV])
#end
#
#class Dc < CodeGen
#  Name = "dc"
#  File = "QR.dc"
#  Cmd = "dc QR.dc > OUTFILE || true" # XXX
#  Apt = "dc"
#  Code = %q("[#{PREV}]pq")
#end

class Dc_Dhall < CodeGen
  Name = ["dc", "Dhall"]
  File = ["QR.dc", "QR.dhall"]
  Cmd = [
    "dc QR.dc > OUTFILE || true", # XXX
    "dhall text --file QR.dhall > OUTFILE",
  ]
  Apt = ["dc", "dhall"]
  Code = %q("[''\n#{PREV}'']p")

  def check(prev)
    raise unless prev.match?(BRACKETS) # dc reads the whole program as a [...] string
    raise if prev.include?("''") # it ends the Dhall multi-line string
    raise if prev.include?("${") # Dhall interpolates it
    raise if prev.start_with?(" ", "\t") # Dhall strips the common indentation
  end
end

class D < CodeGen
  File = "QR.d"
  Cmd = "gdc -o QR QR.d && ./QR > OUTFILE"
  Apt = "gdc"
  Code = %q(%(import std;void main(){write(q"$#{PREV}$");}))

  def check(prev)
    raise if prev.include?("$") # it delimits the q"$...$" string
  end
end

# pakcs package is broken in Ubuntu 20.10; I guess it will be fixed in Ubuntu 21.04
# it was fixed since 21.04, but it is broken again in Ubuntu 22.04
#class Curry < CodeGen
#  Disabled = true
#  File = "QR.curry"
#  Cmd = "pakcs --nocypm :load QR.curry :save :quit && ./QR > OUTFILE"
#  Apt = "pakcs"
#  Code = %q("main=putStr"+E[PREV])
#end

class Crystal < CodeGen
  Name = "Crystal"
  File = "QR.cr"
  Cmd = "crystal QR.cr > OUTFILE"
  Apt = [["crystal", "libevent-dev"]]
  Code = %q("print#{E[PREV]}")

  def check(prev)
    raise if prev.include?('#{') # Crystal interpolates it in a "" string
  end
end

class CommonLisp < CodeGen
  Name = "Common Lisp"
  File = "QR.lisp"
  Cmd = "clisp QR.lisp > OUTFILE"
  Apt = "clisp"
  Code = %q(%((princ"#{e[PREV]}")))

  def check(prev)
    raise if prev.include?("\n") # Common Lisp has no \n escape
  end
end

class CoffeeScript < CodeGen
  File = "QR.coffee"
  Cmd = "coffee --nodejs --stack_size=100000 QR.coffee > OUTFILE"
  Apt = "coffeescript"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        console.log#{V[E[PREV],'"+"\\\\".repeat(',')+"']}
      "
    END
  end
end

class Clojure_CMake_Cobol < CodeGen
  File = ["QR.clj", "QR.cmake", "QR.cob"]
  Cmd = [
    "clojure QR.clj > OUTFILE",
    "cmake -P QR.cmake > OUTFILE",
    "cobc -O2 -x QR.cob && ./QR > OUTFILE",
  ]
  Apt = ["clojure", "cmake", "gnucobol4"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        (doseq[s
          (concat
            ["program-id. q. procedure division. display"]
             (map #(str \\"
                  (.replace %"\\"""\\"\\"")
                  \\")
               (re-seq #".{1,31}"
                  "#{e[PREV]}"))
             ["."])]
          (println"message(STATUS \\"    \\""(pr-str s)")"))
        )
    END
  end

  def check(prev)
    raise if prev.include?("\n") # re-seq #".{1,31}" would drop it
  end
end

class CSharp_Chef < CodeGen
  Name = ["C#", "Chef"]
  File = ["QR.cs", "QR.chef"]
  csproj = <<-END
    <Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net10.0</TargetFramework>
        <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
      </PropertyGroup>
      <ItemGroup>
        <Compile Include="QR.cs" />
      </ItemGroup>
    </Project>
  END
  Cmd = [
    %(echo '#{ csproj.lines.map {|s| s.strip }.join }' > tmp.csproj && DOTNET_NOLOGO=1 dotnet run --project tmp.csproj > OUTFILE),
    "PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl && perl QR.chef.pl > OUTFILE"
  ]
  Apt = ["dotnet-sdk-10.0", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        #$C("Quine Relay Coffee.\\n\\nIngredients.\\n");
        for(int i=9;i++<126;)#$C($"{i} l {i}\\n");
        #$C("\\nMethod.\\n");
        foreach(int c in#{E[PREV.reverse]})#$C($"Put {c}.");
        #$C("Pour contents of the mixing bowl into the baking dish.\\n\\nServes 1.");
      )
    END
  end

  def check(prev)
    # the Chef recipe declares no other ingredient
    raise unless prev.bytes.all? {|b| (10..126).cover?(b) }
  end
end

class Cplusplus < CodeGen
  Name = "C++"
  File = "QR.cpp"
  Cmd = "$(CXX) -o QR QR.cpp && ./QR > OUTFILE"
  Apt = "g++"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        #include<cstdio>\n
        int main(){{
          puts#{E[PREV]};;;;;
        };;;;;}
      "
    END
  end
end

class C < CodeGen
  File = "QR.c"
  Cmd = "$(CC) -o QR QR.c && ./QR > OUTFILE"
  Apt = "gcc"
  def code
    # LZ77-like compression
    <<-'END'.lines.map {|l| l.strip.gsub(/^_+/) { " " * $&.size } }.join
    (
      s=PREV;
      t={};b="";L="";n=i=0;D=->n{L<<(n+62)%92+35;D};
      s.bytes{|c|
        n>0?
          n-=1:
          (x=(t[c]=(t[c]||[]).reject{|j|j<i-3560}).map{|j|
             [(0..90).find{|k|not s[i+1+k]==s[j+k]}||91,j]
           }.max;
           x&&x[0]>4)?
          (
            n,j=x;
            b.gsub(/.{1,3999}/m){|u|D[(z=u.size)%87][z/87];L<<u};b="";
            x=4001+i-j;D[x%87][x/87][n-5]
          ):b<<c;
        t[c]+=[i+=1]
      };
      "
        #include<stdio.h>\n
        char*p=#{E[L]},s[99999],*q=s;
        int main(){
          for(int n,m;*p;){
            n=(*p-5)%92+(p[1]-5)%92*87;
            p+=2;
            for(m=n>3999?(*p++-5)%92+6:n;m--;q++)*q=n>3999?q[4000-n]:*p++;
          }
          puts(s);
        }
      "
    )
    END
  end

  def check(prev)
    raise if prev.bytesize >= 99999 # the decoder buffer is char s[99999]
    raise unless prev[-6..] == prev[-12..-7] # the LZ77 encoder drops the trailing literals
  end
end

class BeanShell_Befunge_BLC8_Brainfuck < CodeGen
  Name = ["BeanShell", "Befunge", "BLC8", "Brainfuck"]
  File = ["QR.bsh", "QR.bef", "QR.Blc", "QR.bf"]
  Cmd = [
    "bsh QR.bsh > OUTFILE",
    "cfunge QR.bef > OUTFILE",
    "ruby vendor/blc.rb < QR.Blc > OUTFILE",
    "ruby vendor/bf.rb QR.bf > OUTFILE",
  ]
  Apt = ["bsh", nil, nil, nil]
  def code
    blc = ::File.read(::File.join(__dir__, "blc-boot.dat"))
    <<-'END'.lines.map {|l| l.strip }.join.sub("BLC", [blc].pack("m0"))
      %(
        f(s){System.out.print(s);}
        s="ef*c+45*,";
        for(c:#{E[PREV]}){
          s+="d9+,";
          for(m=1;m<256;m*=2)
            s+="d9+,4,:"+c/m%2*4+"+,";
          f(s);
          s="4,:,";
        }
        f(s+s);
        for(c:Base64.getDecoder().decode("BLC")){
          c&=255;
          f(""+c%8+c/8%8+c/64+"8*+8*+,");
        }
        f("@");
      )
    END
  end

  def check(prev)
    raise unless prev.ascii_only? # each character is emitted as 8 bits
  end
end

class Bash_Bc < CodeGen
  Name = ["bash", "bc"]
  File = ["QR.bash", "QR.bc"]
  Cmd = [
    "bash QR.bash > OUTFILE",
    "BC_LINE_LENGTH=4000000 bc -q QR.bc > OUTFILE",
  ]
  Apt = ["bash", "bc"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(sed 's/\\\\/\\\\\\\\/g;s/"/\\\\q/g;s/.*/print "&"\\nquit/'<<\\E\n#{PREV}\nE)
    END
  end

  def check(prev)
    raise if prev.include?("\n") # sed appends "quit" to each line
  end
end

class Awk < CodeGen
  Name = "Awk"
  File = "QR.awk"
  Cmd = "awk -f QR.awk > OUTFILE"
  Apt = "gawk"
  Code = %q("BEGIN{print#{E[PREV]}}")
end

class ATS < CodeGen
  File = "QR.dats"
  Cmd = "patscc -o QR QR.dats && ./QR > OUTFILE"
  Apt = "ats2-lang"
  Code = %q("implement main0()=print#{E[PREV]}")

  def check(prev)
    # patscc compiles the C it emits in a strict ISO mode, where a trigraph is translated
    raise if prev =~ %r{\?\?[=/'()!<>-]}
  end
end

class Asymptote < CodeGen
  File = "QR.asy"
  Cmd = "asy QR.asy > OUTFILE"
  Apt = "asymptote"
  Code = %q("write('#{Q[e[PREV],?']}');")
end

class AspectJ < CodeGen
  File = "QR.aj"
  Cmd = "ajc QR.aj && java QR > OUTFILE"
  Apt = "aspectj"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class QR{
          #$L void main(String[]a){
            a=#{E[PREV.gsub(/\\+/){"^#{$&.size}^"}]}.split("\\\\^");
            for(int i=1;i<a.length;a[0]+=a[i+1],i+=2)
              a[0]+="\\\\".repeat(Integer.parseInt(a[i]));
            System.out.print(a[0]);
          }
        }
      )
    END
  end

  def check(prev)
    raise if prev.include?(?^) # it marks the backslash run-length encoding
  end
end

class ALGOL68_Ante < CodeGen
  Name = ["ALGOL 68", "Ante"]
  File = ["QR.a68", "QR.ante"]
  Cmd = [
    "a68g QR.a68 > OUTFILE",
    "ruby vendor/ante.rb QR.ante > OUTFILE",
  ]
  Apt = ["algol68g", nil]
  def code
    <<-'end'.lines.map {|l| l.strip }.join
      %W[
        STRINGz:= 226+ 153,a:=z+ 166,b:=a+"2"+z+ 160,c:=b+"8"+z+ 165,t:="#{d[PREV]}";
        FORiTO\ UPBtDO\ INTn:=ABSt[i];
          print( (50+n%64)+c+ (50+n%8%*8)+c+ (50+nMOD8)+b+ 74+a)
        OD
      ]*"REPR"
    end
  end

  def check(prev)
    raise if prev.include?("\n") # a string denotation is one line
  end
end

class AFNIX_Aheui < CodeGen
  File = ["QR.als", "QR.aheui"]
  Cmd = ["LANG=C LD_LIBRARY_PATH=/usr/lib/afnix axi QR.als > OUTFILE", "ruby vendor/aheui.rb QR.aheui > OUTFILE"]
  Apt = ["afnix", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        trans o(interp:get-output-stream)\n
        o:set-encoding-mode"utf-8"\n
        trans f(v n){
          print(Character(+ 45056(+(* 64 n)v)))
        }\n
        trans D(n){
          if(< n 4){
            f(+(* 6 n)9)48
          }{
            if(n:odd-p){
              D(- n 3)\n
              f 27 48\n
              f 36 11
            }{
              D(/ n 2)\n
              f 21 48\n
              f 48 20
            }
          }
        }\n
        trans S(Buffer"#{e[PREV]}")\n
        while(>(S:length)0){
          trans c(S:read)\n
          D(c:to-integer)\n
          f 35 39
        }\n
        f 24 149
      )
    END
  end

  def check(prev)
    raise unless prev.ascii_only? # the AFNIX Buffer is read as characters
  end
end

class Ada < CodeGen
  File = "qr.adb"
  Cmd = "gnatmake qr.adb && ./qr > OUTFILE"
  Apt = "gnat"
  def code
    <<-'END'.lines.map {|l| l.strip }.join.gsub("$$$", " ")
      %(
        with Text_Io;
        procedure qr is$$$
        begin$$$
          Text_Io.Put("#{d[PREV].gsub(N,'"&ASCII.LF&"')}");
        end;
      )
    END
    #<<-'END'.lines.map {|l| l.strip }.join.gsub("$$$", " ")
    #  %(
    #    with Ada.Text_Io;
    #    procedure qr is$$$
    #    begin$$$
    #      #{f(PREV,120){
    #        %(Ada.Text_Io.Put("#{d[$s].gsub(N,'"&Character'+?'+'Val(10)&"')}");\n)
    #      }}
    #      Ada.Text_Io.Put_Line("");
    #    end;
    #  )
    #END
  end

  def check(prev)
    # 55 = the program without the string, 11 = the extra chars of "&ASCII.LF&"
    # gnat allows up to 32766 characters per line
    raise if prev.size + prev.count(?") + prev.count("\n") * 11 + 55 > 32766
  end
end

class Zsh < CodeGen
  Name = "zsh"
  File = "QR.zsh"
  Cmd = "zsh QR.zsh > OUTFILE"
  Apt = "zsh"
  Code = %q("cat<<\\\\Q\n#{PREV}\nQ")
end

class Zoem < CodeGen
  File = "QR.azm"
  Cmd = "zoem -i QR.azm > OUTFILE"
  Apt = "zoem"
  Code = %q("\\\\write{-}{txt}{#{Q[PREV,/[\\\\{}]/]}}")
end

class Yorick < CodeGen
  File = "QR.yorick"
  Cmd = "yorick -batch QR.yorick > OUTFILE"
  Apt = "yorick"
  Code = %q(%(write,format="%s",#{f(PREV,35){$S+"+\n"}}""))

  def check(prev)
    # yorick allows up to 16360 characters per line, and a chunk becomes ("...")+
    prev.scan(/.{1,#{ 35 * 255 }}/m) { raise if escape($&).size + 5 > 16360 }
  end
end

class Yabasic < CodeGen
  File = "QR.yab"
  Cmd = "yabasic QR.yab > OUTFILE"
  Apt = "yabasic"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        sub f(s$,n)
          ?s$;:
          for i=1to n?"\\\\";
          next end sub:
        f("#{V[e[PREV],'",','):f("']}",0)
      )
    END
  end

  def check(prev)
    # yabasic allows up to 16383 characters per "..." literal
    escape(prev).split(/(?:\\\\)+/) { |s| raise if s.size + 2 > 16383 }
  end
end

class XSLT < CodeGen
  File = "QR.xslt"
  Cmd = "xsltproc QR.xslt > OUTFILE"
  Apt = "xsltproc"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        <?xml-#{I="stylesheet"} type='text/xsl'href=''?>
        <#{I} version='1.0' xmlns='http://www.w3.org/1999/XSL/Transform'>
          <output method='text'/>
          <x xmlns='z:'><![CDATA[#{PREV}]]></x>
        </#{I}>
      "
    END
  end

  def check(prev)
    raise if prev.include?("]]>") # it closes the CDATA section
    raise if prev =~ /[\x00-\x08\x0b\x0c\x0e-\x1f]/ # XML 1.0 forbids it and CDATA cannot escape it
  end
end

class VisualBasic_WebAssemblyBinary_WebAssemblyText_Whitespace < CodeGen
  Name = ["Visual Basic", "WebAssembly (Binary format)", "WebAssembly (Text format)", "Whitespace"]
  File = ["QR.vb", "QR.wasm", "QR.wat", "QR.ws"]
  vbproj = <<-END
    <Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net10.0</TargetFramework>
        <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
      </PropertyGroup>
      <ItemGroup>
        <Compile Include="QR.vb" />
      </ItemGroup>
    </Project>
  END
  Cmd = [
    %(echo '#{ vbproj.lines.map {|s| s.strip }.join }' > tmp.vbproj && DOTNET_NOLOGO=1 dotnet run --project tmp.vbproj > OUTFILE),
    "$(WASI_RUNTIME) QR.wasm > OUTFILE",
    "wat2wasm QR.wat -o QR.wat.wasm && $(WASI_RUNTIME) QR.wat.wasm > OUTFILE",
    "ruby vendor/whitespace.rb QR.ws > OUTFILE"
  ]
  Apt = ["dotnet-sdk-10.0", "wabt", "wabt", nil]
  def code
    r = <<-'END'.lines.map {|l| l.strip }.join(?:)
      %(Module QR\nSub main()\nDim c,n:Dim s=#{C[0]}.openstandardoutput(),t={@@TBL@@}
          For Each d in"@@DATA1@@}@@DATA2@@~@@DATA3@@$@@DATA4@@"
            c=Asc(d)
            If c=36
              For c=0To 11
                #$W(If(c mod 3,Asc(#{s=PREV;s.size*16+3}.ToString("x8")(7-c*2\\3 xor 1)),92))
              Next
            Else
              n=(c>124)*(@@CONST1@@*c-#{s.size+@@CONST2@@})
              Do While n>127
                #$W(n mod 128+128)
                n\\=128
              Loop
              #$W(If(c<125,If((c-1)\\7-8,c-66*(c>65)*(c<91),t(c-57)),n))
            End If
          Next
          For Each c in"#{d[s].gsub N,'"& VbLf &"'}"
            #$W(Asc(c))
          Next
        End Sub
      end module)
    END
    tbl, data1, data2, data3, data4 = ::File.read(::File.join(__dir__, "wasm-tmpl.dat")).lines.map {|s| s.chomp }
    raise unless data3[0] == '('
    r.gsub(/@@\w+@@/, {
      "@@TBL@@" => tbl,
      "@@DATA1@@" => data1.gsub("\\"){"\\\\"},
      "@@DATA2@@" => data2.gsub("\\"){"\\\\"},
      "@@DATA3@@" => '#{40.chr}'+data3[1..].gsub("\\"){"\\\\"},
      "@@DATA4@@" => data4.gsub("\\"){"\\\\"},

      # K = the constant part of the wasm data section size
      # precompute some expressions by assuming that 2**14 <= (K+s.size) < 2**21
    # "@@CONST1@@" => '#{6+((K+s.size).bit_length-1)/7}',
      "@@CONST1@@" => "8",
    # "@@CONST2@@" => '#{m=((K+s.size).bit_length-1)/7;m+K+125*(6+m)}',
      "@@CONST2@@" => (2 + (data3.size + data4.size + 18) + 125 * 8).to_s
    })
  end

  def check(prev)
    # the length is emitted as a 2 or 3 byte LEB128, so it must not fall outside that range
    data3, data4 = ::File.read(::File.join(__dir__, "wasm-tmpl.dat")).lines.values_at(3, 4)
    k = data3.chomp.size + data4.chomp.size + 18
    raise unless (2**14...2**21).cover?(k + prev.size)
  end
end

class VimScript < CodeGen
  Name = ["Vimscript"]
  Apt = "vim"
  File = "QR.vim"
  Cmd = "vim -EsS QR.vim > OUTFILE"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "let s=#{E[PREV]}\nput=s\n2,$p\nq!"
    END
  end
end

class Verilog < CodeGen
  File = "QR.v"
  Cmd = "iverilog -o QR QR.v && ./QR -vcd-none > OUTFILE"
  Apt = "iverilog"
  Code = %q(%(module QR;initial begin #{f(PREV,3){%($write("%s",#$S);)}}end endmodule))
end

class Vala_Velato < CodeGen
  File = ["QR.vala", "QR.mid"]
  Cmd = [
    "valac QR.vala && ./QR > OUTFILE",
    "mono vendor/local/bin/Vlt.exe /s QR.mid && mono QR.exe > OUTFILE",
  ]
  Apt = ["valac", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        void p(int[]c){
          foreach(int v in c)
            stdout.printf("%c%c",v>>8,v);
        }
        void main(){
          int[]a;
          p({19796,26724,0,6,0,1,480,19796,29291,#{s=PREV;(s.size*72+4).divmod(65536)*?,}});
          foreach(int c in#{E[s]}.data)
            foreach(int v in a={0,9,7,4,5,c/100*7/6+1,c%100/10*7/6+1,c%10*7/6+1,7})
              p({144,v=15450+v*256,384,v});
          p({255,12032});
        }
      )
    END
  end
end

class TypeScript_Unlambda < CodeGen
  File = ["QR.ts", "QR.unl"]
  Cmd = ["tsc --outFile QR.ts.js QR.ts && $(JAVASCRIPT) QR.ts.js > OUTFILE", "ruby vendor/unlambda.rb QR.unl > OUTFILE"]
  Apt = ["node-typescript", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "let t='k',c;for(c of#{E[PREV]})t='\\x60.'+c+t;console.log(t)"
    END
  end

  def check(prev)
    raise if prev.include?("\n") # `.<newline> is not a valid token
  end
end

class Tcsh_Thue < CodeGen
  Name = ["tcsh", "Thue"]
  File = ["QR.tcsh", "QR.t"]
  Cmd = ["tcsh QR.tcsh > OUTFILE", "ruby vendor/thue.rb QR.t > OUTFILE"]
  Apt = ["tcsh", nil]
  Code = %q(%(cat<<\\\\E\na::=~#{PREV}\n::=\na\n\\\\E))

  def check(prev)
    raise if prev.include?("\n") # a Thue rule is one line
  end
end

class Tcl < CodeGen
  File = "QR.tcl"
  Cmd = "tclsh QR.tcl > OUTFILE"
  Apt = "tcl"
  Code = %q(%(puts "#{Q[PREV,/["$\[\\\\]/]}"))
end

class Swift < CodeGen
  File = "QR.swift"
  Cmd = "swiftc QR.swift && ./QR > OUTFILE"
  Apt = "swiftlang"
  Code = %q("print"+E[PREV])
end

class SurgeScript < CodeGen
  File = "QR.ss"
  Cmd = "surgescript QR.ss > OUTFILE"
  Apt = "surgescript"
  Code = %q(%(object"Application"{state"main"{#{f(PREV,4){%(Console.write#$S;)}}exit();}}))
end

class StandardML_Subleq < CodeGen
  Name = ["Standard ML", "Subleq"]
  File = ["QR.sml", "QR.sq"]
  Cmd = ["polyc -o QR QR.sml && ./QR > OUTFILE", "ruby vendor/subleq.rb QR.sq > OUTFILE"]
  Apt = [["polyml", "libpolyml-dev"], nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        fun p n=print(Int.toString n^" ");
        fun main()=(
          print"0 0 130 ";
          List.tabulate(127,p);
          String.map(fn c=>(p(3+ord c);print"-1 0 ";c))#{E[PREV]}
        )
      )
    END
  end

  def check(prev)
    raise if prev.bytes.max.to_i > 126
  end
end

class Squirrel < CodeGen
  File = "QR.nut"
  Cmd = "squirrel QR.nut > OUTFILE"
  Apt = "squirrel3"
  Code = %q("print"+E[PREV])
end

class Scilab_Sed_Shakespeare_SLang < CodeGen
  Name = ["Scilab", "sed", "Shakespeare", "S-Lang"]
  File = ["QR.sci", "QR.sed", "QR.spl", "QR.sl"]
  Cmd = [
    "scilab-cli -nb -f QR.sci > OUTFILE",
    "sed -E -f QR.sed QR.sed > OUTFILE",
    "spl2c < QR.spl > QR.spl.c && gcc -z muldefs -o QR -I ./vendor/local/include -L ./vendor/local/lib QR.spl.c -lspl -lm && ./QR > OUTFILE",
    "slsh QR.sl > OUTFILE",
  ]
  Apt = ["scilab-cli", "sed", nil, "slsh"]
  def code
    # NOTE: This code does not work for a short or simple text.
    # This assumes the input is so complex enough that
    # the compressed result won't be one character.
    #
    # * The Scheme program generates the encoded Shakespeare code.
    # * sed program decodes and completes Shakespeare code.
    # * The S-Lang program includes 8-bit characters and decompress the compression.
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        #{
          s,v=rp[PREV,127..255];
          c=->x{x.gsub(/.{1,99}/m){"+\n"+E[$&]}};
          f(
            "1d;
            s/.//;
            s/1/ the sum of a son and0/g;
            s/0/ twice/g\n
            #The Relay of Quine.\n
            #Ajax, a man.\n
            #Ford, a man.\n
            #Act i: Quine.\n
            #Scene i: Relay.\n
            #[Enter Ajax and Ford]\n
            #Ajax:\n
            #"+
            %(
              variable s=""#{c[s]},v=""#{c[v]},i;
              _for i(0,128,1)
                s=strreplace(s,pack("C",255-i),v[[i*2:i*2+1]]);
              printf("%s",s)
            ).bytes.map{|b|"You are as bad as"+("%b"%b)[1..].reverse+" a son.Speak thy mind."}*""+
            "\n#[Exeunt]",7
          ){
            "printf#$S\n"
          }
        }
        quit
      )
    END
  end

  def check(prev)
    raise if prev.bytes.max.to_i > 126
  end
end

class Scheme < CodeGen
  File = "QR.scm"
  Cmd = "$(SCHEME) QR.scm > OUTFILE"
  Apt = "guile-3.0"
  Code = %q(%((display"#{e[PREV]}")))
end

class Scala < CodeGen
  File = "QR.scala"
  Cmd = "scalac QR.scala && scala QR > OUTFILE"
  Apt = "scala"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        object QR extends App{
          #{f(PREV,196){%(print#$S;)}}
        }
      "
    END
  end

  def check(prev)
    # the JVM allows up to 65535 bytes per CONSTANT_Utf8
    prev.scan(/.{1,#{ 196 * 255 }}/m) { raise if $&.bytesize > 65535 }
  end
end

class Rust < CodeGen
  File = "QR.rs"
  Cmd = "rustc QR.rs && ./QR > OUTFILE"
  Apt = "rustc"
  Code = %q(%(fn main(){print!("{}",#{E[PREV]})}))
end

class Ruby_ < CodeGen
  Name = "Ruby"
  File = "QR.rb"
  Cmd = "ruby QR.rb > OUTFILE"
  Apt = "ruby"
  Code = nil
end

load "code-gen-pool.rb" if ENV["ALL"]

GenSteps = CodeGen::List.map {|s| s.gen_step }
RunSteps = CodeGen::List.reverse.flat_map {|s| s.run_steps }
