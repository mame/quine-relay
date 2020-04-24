# A source for generating Quine Relay

GenStep = Struct.new(:name, :code, :run_steps)
RunStep = Struct.new(:name, :src, :cmd_make, :cmd_raw, :backup, :apt)

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
    GenStep[name, new.code, run_steps]
  end

  def code
    self.class::Code
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
      cmd_raw = cmd_raw.gsub("$(BF)", "bf -c500000")
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
  $G=" contents of"+$F=" the mixing bowl";
  $L="public static";
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
        for c in"".join(["echo 'say ''%s'''\\n"%l for l in#{E[d[d[PREV,?'],?']]}.split("\\n")]):
          print('r=fput(char(%d))'%ord(c))\n
        print('end\\n")')#
      )
    END
  end
end

class Promela < CodeGen
  Name = "Promela (Spin)"
  File = "QR.pr"
  Cmd = "spin -T QR.pr > OUTFILE"
  Apt = "spin"
  Code = %q("init{#{f(PREV,6){"printf#{d[$S,?%]};"}}}")
end

class Prolog < CodeGen
  File = "QR.prolog"
  Cmd = "swipl -q -t qr -f QR.prolog > OUTFILE"
  Apt = "swi-prolog"
  Code = %q("qr:-write('#{Q[e[PREV],?']}').")
end

class PostScript_PPT < CodeGen
  Name = ["PostScript", "PPT (Punched tape)"]
  File = ["QR.ps", "QR.ppt"]
  Cmd = ["gs -dNODISPLAY -q QR.ps > OUTFILE", "ppt -d < QR.ppt > OUTFILE"]
  Apt = ["ghostscript", "bsdgames"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        (#{?_*11})dup =
        /s(|     .   |)def
        (#{Q[PREV,B]}){
          9 7{
            exch dup 1 and 79 mul 32 add exch 2 idiv 3 1 roll s exch 2 index exch put 1 sub dup 6 eq{1 sub}if
          }repeat s = pop pop
        }forall = quit
      "
    END
  end
end

class Pike < CodeGen
  File = "QR.pike"
  Cmd = "pike QR.pike > OUTFILE"
  Apt = "pike8.0"
  Code = %q("int main(){write#{E[PREV]+R}}")
end

class PHP_Piet < CodeGen
  File = ["QR.php", "QR.png"]
  Cmd = ["php QR.php > OUTFILE", "npiet QR.png > OUTFILE"]
  Apt = ["php-cli", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        <?php $z=3+$w=strlen($s=#{Q[E[PREV]]})*3;
          echo"\\x89PNG\\r\\n\\x1a\\n";
          $m="";
          $t="\\xc0\\0\\xff";
          for($i=-1;++$i<128*$z;
              $m.=$c--?
                ($w-$c||$i>$z)&&$i/$z<($c<$w?ord($s[(int)($c/3)]):$c--%3+2)?
                  $t[2].$t[$c%3%2].$t[$c%3]:"\\0\\0\\0":"\\0"
          )
            $c=$i%$z;
          foreach(array(
            "IHDR".pack("NNCV",$w+2,128,8,2),
            "IDAT".gzcompress($m),
            "IEND"
          )as$d)
            echo pack("NA*N",strlen($d)-4,$d,crc32($d));
      )
    END
  end
end

class Perl6 < CodeGen
  Name = "Perl 6"
  File = "QR.pl6"
  Cmd = "perl6 QR.pl6 > OUTFILE"
  Apt = "rakudo"
  Code = %q("$_='#{Q[PREV.gsub(B,"\x7f"),?']}';s:g/\\\\x7f/\\\\\\\\/;print $_")
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
            s="
              $_='#{Q[s,c=/['\\\\]/]}';
              $n=32;
              $s='#{Q[v,c]}';
              $s=~s{..}{
                $a=$&;
                $b=chr(--$n&255);
                ~s/$b/$a/g;
              }eg;
              print
            ";
            (s+N*(-s.size%6)).unpack("B*")[0].
              gsub(/.{6}/){n=$&.to_i 2;((n+14)/26*6+n+47).chr}
          }";
          s|.|$n=ord$&;substr unpack(B8,chr$n-int($n/32)*6-41),2|eg;
          eval pack'B*',$_
        ).scan(/[ ,-:A-z]+|(.)/){p="s++#{$1?"chr #{$1.ord}+e":$&+?+};"+p};
        p
      )
    END
  end
end

class Pascal < CodeGen
  File = "QR.pas"
  Cmd = "fpc QR.pas && ./QR > OUTFILE"
  Apt = "fp-compiler"
  Code = %q("#$D(output);begin write(#{f(PREV,1){"'#$s',"}}'')end.")
end

class Parser3 < CodeGen
  Name = "Parser 3"
  File = "QR.p"
  Cmd = "parser3 QR.p > OUTFILE"
  Apt = "parser3-cgi"
  Code = %q("$console:line[#{PREV.gsub(/[:;()]/){?^+$&}}]")
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
    "ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf && $(BF) QR.ook.bf > OUTFILE"
  ]
  Apt = ["octave", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        s=double#{E[PREV]};
        t=num2cell(b=11-ceil(s/13));
        for n=1:9
            m={};
            for i=1:141
              f=@(x,y,n)repmat(['Ook' char(x) ' Ook' char(y) ' '],[1 abs(n)]);
              m(i)=[f(z=46,63,n) f(q=z-(i<13)*13,q,i-13) f(33,z,1) f(63,z,n)];
            end;
            t(x=b==n)=m(diff([0 s(x)])+13);
        end;
        printf('%%s',t{:})
      "
    END
    # NOTE: %% is a hack for Nickle printf escaping.
  end
end

class OCaml < CodeGen
  File = "QR.ml"
  Cmd = "ocaml QR.ml > OUTFILE"
  Apt = "ocaml"
  Code = %q("print_string"+E[PREV])
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
  Cmd = "nim c QR.nim && ./QR > OUTFILE"
  Apt = "nim"
  Code = %q("echo"+E[PREV])
end

class Nickle < CodeGen
  File = "QR.5c"
  Cmd = "nickle QR.5c > OUTFILE"
  Apt = "nickle"
  Code = %q("printf#{E[PREV]}\\n")
end

class Neko < CodeGen
  File = "QR.neko"
  Cmd = "nekoc QR.neko && neko QR.n > OUTFILE"
  Apt = "neko"
  Code = %q("$print#{E[PREV]};")
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
      "m{{!: x
      qr: |-
      ^^^:db\x60#{e[s=PREV]}\x60
      ^^^global _start
      ^^^_start:mov edx,#{s.size}
      ^^^mov ecx,m
      ^^^mov ebx,1
      ^^^mov eax,4
      ^^^int 128
      ^^^mov ebx,0
      ^^^mov eax,1
      ^^^int 128
      x: |
      ^^^}}{{{qr}}}"
    END
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
        .method #$L void Main()
        {
          .entrypoint ldstr"#{e[PREV]}"
          call void [mscorlib]#{C*"::"}(string)
          ret
        }
      )
    END
  end
end

class MiniZinc < CodeGen
  File = "QR.mzn"
  Cmd = "minizinc --solver Gecode --soln-sep '' QR.mzn > OUTFILE"
  Apt = "minizinc"
  Code = %q("solve satisfy;output [#{E[PREV]}];")
end

class Maxima < CodeGen
  File = "QR.mac"
  Cmd = "maxima -q --init-mac=QR.mac > OUTFILE"
  Apt = "maxima"
  Backup = [[
    %(if [ $(CI) = "true" ]; then mv /tmp /tmp.bak && ln -s /dev/shm /tmp; fi),
    %(if [ $(CI) = "true" ]; then rm /tmp && mv /tmp.bak /tmp; fi),
  ]]
  Code = %q("linel:99999;print#{E[PREV]};quit();")
end

class Makefile < CodeGen
  File = "QR.mk"
  Cmd = "make -f QR.mk > OUTFILE"
  Apt = "make"
  Code = %q("all:\n\t@echo '#{d[PREV,?$].gsub(?'){"'\\\\''"}}'")
end

class M4 < CodeGen
  File = "QR.m4"
  Cmd = "m4 QR.m4 > OUTFILE"
  Apt = "m4"
  Code = %q("changequote(<@,@>)\ndefine(p,<@#{PREV}@>)\np")
end

class Lua < CodeGen
  File = "QR.lua"
  Cmd = "lua5.3 QR.lua > OUTFILE"
  Apt = "lua5.3"
  Code = %q("x=string.gsub(#{V[E[PREV],?&,?&]},'&(%d+)&',function(s)return string.rep('\\\\\\\\',tonumber(s))end);print(x)")
end

class LOLCODE < CodeGen
  File = "QR.lol"
  Cmd = "lci QR.lol > OUTFILE"
  Apt = [nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        HAI 1.2\n
        VISIBLE "#{
          PREV.gsub(/[:"]/,":\\0")
        }"\n
        KTHXBYE BYE
      )
    END
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
        @s=global[#{i=(s=PREV).size+1}x i8]
          c"#{s.gsub(/[\\"\n\t]/){"\\%02X"%$&.ord}}\\00"
        declare i32@puts(i8*)
        define i32@main(){
          %1=call i32@puts(i8*getelementptr([#{i}x i8],[#{i}x i8]*@s,i32 0,i32 0))
          ret i32 0
        }
      )
    END
  end
end

class LiveScript < CodeGen
  Name = "LiveScript"
  File = "QR.ls"
  Cmd = "lsc QR.ls > OUTFILE"
  Apt = "livescript"
  Code = %q("console.log"+Q[E[PREV],?#])
end

class Ksh_LazyK_Lisaac < CodeGen
  Name = ["ksh", "Lazy K", "Lisaac"]
  File = ["QR.ksh", "QR.lazy", "qr.li"]
  Cmd = [
    "ksh QR.ksh > OUTFILE",
    "lazyk QR.lazy > OUTFILE",
    "lisaac qr.li && ./qr > OUTFILE",
  ]
  Apt = ["ksh", nil, "lisaac"]
  def code
    lazyk = ::File.read(::File.join(__dir__, "lazyk-boot.dat"))
    lazyk = lazyk.tr("ski`","0123").scan(/.{1,3}/).map do |n|
      n = n.reverse.to_i(4)
      [*93..124,*42..73][n]
    end.pack("C*")
    lazyk = lazyk.gsub(/[ZHJK\^`~X]/) {|c| "\\x%02x" % c.ord }
    <<-'END'.lines.map {|l| l.strip }.join.sub("LAZYK"){lazyk}
      %(
        s=();
        a(){ s+=($(echo -n $1|od -An -tu1 -v) $2);};
        a "SectionHeader+name:=QR;SectionPublic-main<-(" 10;
        t='#{PREV.gsub(?',%('"'"'))}';
        for((i=0;i<${#t};i+=99));do;
          x=${t:$i:99};
          a "\\"${x//[\\\\\\\"]/\\\\\\0}\\".print;" 10;
        done;
        a ");";
        p(){ echo -n $1;};
        f(){ for x in ${s[*]};do;
            p $3;
            for((j=$2;j--;));do;
              h $1 $x $j;
            done;
          done;
        };
        p k\\`;
        h(){ p \\`${1:$(($2>>$3&1)):2};};
        f kki 7 '``s``s``s``s``s``s``s``si';
        s=();
        a 'LAZYK';
        h(){ p ${1:$(((($2%83-10)>>((2-$3)*2))%4)):1};};
        f ski\\` 3
      )
    END
  end
end

class Julia < CodeGen
  File = "QR.jl"
  Cmd = "julia QR.jl > OUTFILE"
  Apt = "julia"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(print("""#{Q[e[PREV]]}"""))
    END
  end
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
        P={0:'[+[]]',m:'((+[])'+(C="['constructor']")+"+[])['11']"};
        for(R in B=
          (
            '![]@!![]@[][[]]@'+
            (A="[]['fill']")+
            "@([]+[])['fontcolor']([])@(+('11e20')+[])['split']([])@"+
            A+C+"('return escape')()("+A+')'
          ).split('@')
        )
          for(E in D=eval(G='('+B[R]+'+[])'))
            P[T=D[E]]=P[T]||G+"['"+E+"']";
        for(G='[',B=0;++B<36;)
          P[D=B.toString(36)]=
            B<10?
              (G+='+!+[]')+']'
            :
              P[D]||"(+('"+B+"'))['to'+([]+[])"+C+"['name']]('36')";
        A+=C+"('console.log(unescape(\\"";
        for(E in G=#{E[PREV]})
          A+="'+![]+'"+G.charCodeAt(E).toString(16);
        for(A+="\\".replace(/'+![]+'/g,\\"%\\")))')()",R=0;R<9;R++)
          A=A.replace(/'.*?'/g,function(B){
            T=[];
            for(E=1;B[E+1];)
              T.push(P[B[E++]]);
            return T.join('+')
          });
        console.log('"'+A+'"')
      )
    END
  end
end

class Java_ < CodeGen
  Name = "Java"
  File = "QR.java"
  Cmd = "javac QR.java && java QR > OUTFILE"
  Apt = "openjdk-11-jdk"
  def code
    # LZ78-like compression
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class QR{
          #$L void main(String[]v){
            String c[]=new String[99999],y="",z=y,s="#{
              z=t=(0..r=q=126).map{|n|[n,[]]};
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
            for(;++n<126;)c[n]=""+(char)n;
            for(;i<#{a.size};){
              q=q*78+(s.charAt(i)-13)%84;
              if(i++%2>0){
                y=q<n?c[q]:y;
                c[n++]=z+y.charAt(0);
                System.out.print(z=c[q]);
                q=0;
              }
            }
          }
        }
      )
    END
  end
end

class Jasmin < CodeGen
  File = "QR.j"
  Cmd = "jasmin QR.j && java QR > OUTFILE"
  Apt = "jasmin-sable"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        .class public QR\n
        .super #{$T="java/io/PrintStream"}\n
        .method #$L main([L#{S="java/lang/S"}tring;)V ;]\n
        .limit stack 2\n
        getstatic #{S}ystem/out L#$T;\n
        ldc "#{e[PREV]}"\n
        invokevirtual #$T/println(L#{S}tring;)V\n
        return\n
        .end method
      )
    END
  end
end

class Icon_INTERCAL < CodeGen
  File = ["QR.icn", "QR.i"]
  Cmd = [
    "icont -s QR.icn && ./QR > OUTFILE",
    "ick -bfOc QR.i && gcc -static QR.c -I /usr/include/ick-* -o QR -lick && ./QR > OUTFILE"
  ]
  Backup = [nil, "QR.c"]
  Apt = [["icont", "iconx"], "intercal"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        procedure main();
          i:=c:=0;
          s:=#{E[PREV+N]};
          write("DO,1<-#"||*s);
          s?while t:=ord(move(1))do{
            i+:=1;
            u:=-i;
            every 0to 7do{u:=u*2+t%2;t/:=2};
            write("PLEASE")\\(i%4/3);
            write("DO,1SUB#"||i||"<-#"||((c-u)%256));
            c:=u;
          };
          write("PLEASEREADOUT,1\\nPLEASEGIVEUP");
        end
      )
    END
  end
end

class Haxe < CodeGen
  File = "QR.hx"
  Cmd = "haxe -main QR -neko QR.n && neko QR.n > OUTFILE"
  Apt = "haxe"
  Code = %q("class QR{#$L function main(){neko.Lib.print#{E[PREV]};}}")
end

class Haskell < CodeGen
  File = "QR.hs"
  Cmd = "ghc QR.hs && ./QR > OUTFILE"
  Apt = "ghc"
  Code = %q("main=putStr"+E[PREV])
end

class Groovy_Gzip < CodeGen
  File = ["QR.groovy", "QR.gz"]
  Cmd = ["groovy QR.groovy > OUTFILE", "gzip -cd QR.gz > OUTFILE"]
  Apt = ["groovy", "gzip"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        z=new java.util.zip.GZIPOutputStream(System.out);
        z.write('#{PREV.tr(?"+B,"!~")}'.tr('~!','\\\\\u0022')as byte[]);
        z.close()
      )
    END
  end
end

class GolfScript_Grass < CodeGen
  Name = ["GolfScript", "Grass"]
  File = ["QR.gs", "QR.grass"]
  Cmd = ["ruby vendor/golfscript.rb QR.gs > OUTFILE", "ruby vendor/grass.rb QR.grass > OUTFILE"]
  Apt = [nil, nil]
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
        @@PROLOGUE@@
        "#{e[PREV]}"
        {
          "W""w"@j 1+:j\\- @@MOD@@%1+*
        }%
        @@EPILOGUE@@
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
          fmt.Print#{E[PREV]};
        }
      )
    END
  end
end

class Gnuplot < CodeGen
  File = "QR.plt"
  Cmd = "gnuplot QR.plt > OUTFILE"
  Apt = "gnuplot"
  Code = %q('set print"-";print'+E[PREV])
end

class GeneratorScriptingLanguage < CodeGen
  Name = "GeneratorScriptingLanguage"
  File = "QR.gsl"
  Cmd = "gsl -q QR.gsl > OUTFILE"
  Apt = "generator-scripting-language"
  Code = %q(".template 1\n#{d[PREV,B]}\n.endtemplate")
end

class GEL < CodeGen
  Name = "GEL (Genius)"
  File = "QR.gel"
  Cmd = "genius QR.gel > OUTFILE"
  Apt = "genius"
  Code = %q(f(PREV,61){"printn#$S\n"})
end

class GDB < CodeGen
  File = "QR.gdb"
  Cmd = "gdb -q -x QR.gdb > OUTFILE"
  Apt = "gdb"
  Code = %q(%(printf"#{e[d[PREV,?%]]}"\nquit))
end

class GAP < CodeGen
  File = "QR.g"
  Cmd = "gap -q QR.g > OUTFILE"
  Apt = "gap"
  Code = %q("s:=OutputTextUser();WriteAll(s,#{E[PREV]});CloseStream(s);QUIT;")
end

class Gambas < CodeGen
  Name = "Gambas script"
  File = "QR.gbs"
  Cmd = "$(GBS) QR.gbs > OUTFILE"
  Apt = "gambas3-script"
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
    # assuming that PREV has no '
    <<-'END'.lines.map {|l| l.strip }.join(" ")
      %(
        : A ."#{g*9}" ;
        : B A ." WRITE(*,*)'" A ;
        : C B TYPE ." '" CR ;
        : D
          S" #$D" C
          S\\" print \\"(&" C
          S\\" #{e[PREV]}" DUP A ." DO 10 I=1," . CR
          S" &A,&" C
          ." 10      CONTINUE" CR
          S\\" &A)\\",&" C
          0 DO B ." &char(" COUNT . ." ),&'" CR LOOP
          S\\" &\\"\\"" C
          S" end #$D" C
          A ." STOP" CR
          A ." END" CR
          BYE ;
        D
      )
    END
  end
end

class Fish < CodeGen
  File = "QR.fish"
  Cmd = "fish QR.fish > OUTFILE"
  Apt = "fish"
  Code = %q("echo '#{Q[Q[PREV,B],?!].gsub(?',%('"'"'))}'")
end

class Flex < CodeGen
  File = "QR.fl"
  Cmd = "flex -o QR.fl.c QR.fl && gcc -o QR QR.fl.c && ./QR > OUTFILE"
  Apt = "flex"
  Code = %q("%option noyywrap\n%%\n%%\nint main(){puts#{E[PREV]};}")
end

class FALSELang < CodeGen
  Name = "FALSE"
  File = "QR.false"
  Cmd = "ruby vendor/false.rb QR.false > OUTFILE"
  Apt = [nil]
  Code = %q(?"+PREV.gsub(?"){'"34,"'}.gsub(N){'"10,"'}+?")
end

class FSharp < CodeGen
  Name = "F#"
  File = "QR.fsx"
  Cmd = "fsharpc QR.fsx -o QR.exe && mono QR.exe > OUTFILE"
  Apt = "fsharp"
  Code = %q('printfn("""'+d[PREV,?%]+' """)')
end

class Erlang < CodeGen
  File = "QR.erl"
  Cmd = "escript QR.erl > OUTFILE"
  Apt = "erlang"
  Code = %q("\nmain(_)->\nio:fwrite#{d[E[PREV],?~]}.")
end

class EmacsLisp < CodeGen
  Name = "Emacs Lisp"
  File = "QR.el"
  Cmd = "emacs -Q --script QR.el > OUTFILE"
  Apt = "emacs-nox"
  Code = %q(%((princ "#{e[PREV]}")))
end

class Elixir < CodeGen
  File = "QR.exs"
  Cmd = "elixir QR.exs > OUTFILE"
  Apt = "elixir"
  Code = %q("IO.puts"+E[PREV])
end

class EC < CodeGen
  Name = "eC"
  File = "QR.ec"
  Cmd = "ecp -c QR.ec -o QR.sym && ecc -c QR.ec -o QR.c && ecs -console QR.sym QR.imp -o QR.main.ec && ecp -c QR.main.ec -o QR.main.sym && ecc -c QR.main.ec -o QR.main.c && gcc -o QR QR.c QR.main.c -lecereCOM && ./QR > OUTFILE"
  Backup = "QR.c"
  Apt = "ecere-dev"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class QR:Application{
          void f(String const s,int n){for(Print(s);n;n--)Print("\\\\");}
          void Main(){#{f(PREV,15){"f(#{V[$S[1..-2],'",',');f("']},0);"}}}
        }
      )
    END
  end
end

class Dc < CodeGen
  Name = "dc"
  File = "QR.dc"
  Cmd = "dc QR.dc > OUTFILE || true" # XXX
  Apt = "dc"
  Code = %q("[#{PREV}]pq")
end

class Dafny < CodeGen
  File = "QR.dfy"
  Cmd = "dafny QR.dfy && mono QR.exe > OUTFILE"
  Apt = "dafny"
  Code = %q(%(method Main(){print(@"#{d[PREV]}");}))
end

class D < CodeGen
  File = "QR.d"
  Cmd = "gdc -o QR QR.d && ./QR > OUTFILE"
  Apt = "gdc"
  Code = %q("import std.stdio;void main(){write(`#{PREV}`);}")
end

class Curry < CodeGen
  Disabled = true
  File = "QR.curry"
  Cmd = "pakcs --nocypm :load QR.curry :save :quit && ./QR > OUTFILE"
  Apt = "pakcs"
  Code = %q("main=putStr"+E[PREV])
end

class CommonLisp < CodeGen
  Name = "Common Lisp"
  File = "QR.lisp"
  Cmd = "clisp QR.lisp > OUTFILE"
  Apt = "clisp"
  Code = %q(%((write-line"#{e[PREV]}")))
end

class CoffeeScript < CodeGen
  File = "QR.coffee"
  Cmd = "coffee --nodejs --stack_size=100000 QR.coffee > OUTFILE"
  Apt = "coffeescript"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        (f=(n)->Array(n+1).join '\\\\');
        console.log('%s',#{V[E[PREV].gsub(?`,"\\\\x60"),'#{f(',')}']})
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
  Apt = ["clojure", "cmake", "gnucobol"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        (doseq[s
          (lazy-cat
            ["IDENTIFICATION DIVISION."
             "PROGRAM-ID. QR."
             "PROCEDURE DIVISION."
             'DISPLAY]
             (map #(str
                  "    \\""
                  (.replace %1"\\"""\\"\\"")
                  "\\"")
               (re-seq #".{1,45}"
                  "#{e[PREV]}"))
             ["    \\" \\"."
              "STOP RUN."])]
          (println(str
            "message(STATUS \\"     "
            (.replace(.replace(str s)"\\\\""\\\\\\\\")"\\"""\\\\\\"")
            "\\")")))
        )
    END
  end
end

class CSharp_Chef < CodeGen
  Name = ["C#", "Chef"]
  File = ["QR.cs", "QR.chef"]
  Cmd = [
    "mcs QR.cs && mono QR.exe > OUTFILE",
    "PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl && perl QR.chef.pl > OUTFILE"
  ]
  Apt = ["mono-mcs", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class Program{
          #$L void Main(){
            #$C("Quine Relay Coffee.\\n\\nIngredients.\\n");
            for(int i=9;i++<126;)#$C($"{i} g caffeine {i}\\n");
            #$C("\\nMethod.\\n");
            foreach(char c in#{E[PREV.reverse]})#$C($"Put caffeine {(int)c} into#$F.\\n");
            #$C("Liquify#$G.\\nPour#$G into the baking dish.\\n\\nServes 1.\\n");
          }
        }
      )
    END
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
        #include<iostream>\n
        int main(){
          std::cout<<#{E[PREV]};
        }/****//****/
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
          (t[c]=(t[c]||[]).reject{|j|j<i-3560};
           x=[];
           t[c].map{|j|
             k=(0..90).find{|k|not s[i+1+k]==s[j+k]}||91;
             k>4&&x<<[k,j]
           };
           x=x.max)?
          (
            n,j=x;
            x=b.size;(u=[x,3999].min;D[u%87][u/87];L<<b[0,u];b[0,u]="";x-=u)while x>0;
            x=4001+i-j;D[x%87][x/87][n-5]
          ):b<<c;
        t[c]+=[i+=1]
      };
      "
        #include<stdio.h>\n
        char*p=#{E[L]},s[999999],*q=s;
        int main(){
          int n,m;
          for(;*p;){
            n=(*p-5)%92+(p[1]-5)%92*87;
            p+=2;
            if(n>3999)
              for(m=(*p++-5)%92+6;m--;q++)*q=q[4000-n];
            else for(;n--;)*q++=*p++;
          }
          puts(s)#{R}
        }
      "
    )
    END
  end
end

class BeanShell_Befunge_BLC8_Brainfuck < CodeGen
  Name = ["BeanShell", "Befunge", "BLC8", "Brainfuck"]
  File = ["QR.bsh", "QR.bef", "QR.Blc", "QR.bf"]
  Cmd = [
    "bsh QR.bsh > OUTFILE",
    "cfunge QR.bef > OUTFILE",
    "ruby vendor/blc.rb < QR.Blc > OUTFILE",
    "$(BF) QR.bf > OUTFILE",
  ]
  Apt = ["bsh", nil, nil, "bf"]
  def code
    blc = ::File.read(::File.join(__dir__, "blc-boot.dat"))
    <<-'END'.lines.map {|l| l.strip }.join.sub("BLC", [blc].pack("m0"))
      %(
        f(s){System.out.print(s);}
        s="389**6+44*6+00p45*,";
        for(c:#{E[PREV]}){
          s+="00g,";
          for(m=1;m<256;m*=2)
            s+="00g,4,:"+(c/m%2>0?"4+":"")+",";
          f(s);
          s="4,:,";
        }
        f(s+s);
        for(c:Base64.getDecoder().decode("BLC")){
          c=c<0?256+c:c;
          for(i=0;i++<3;c/=8)f(c%8);
          f("8*+8*+,");
        }
        f("@");
      )
    END
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
      %(echo '#{PREV.gsub(?',%('"'"'))}'|sed -e's/\\\\/\\\\\\\\/g' -e's/"/\\\\q/g' -e's/.*/print "&"\\nquit/')
    END
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
  Code = %q("implement main0()=print"+E[PREV])
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
            for(int i=1;i<a.length;a[0]+=a[i+1],i+=2){
              a[0]+="\\\\".repeat(Integer.parseInt(a[i]));
            }
            System.out.print(a[0]);
          }
        }
      )
    END
  end
end

class ALGOL68_Ante_AspectCpp < CodeGen
  Name = ["ALGOL 68", "Ante", "AspectC++"]
  File = ["QR.a68", "QR.ante", "QR.cc"]
  Cmd = [
    "a68g QR.a68 > OUTFILE",
    "ruby vendor/ante.rb QR.ante > OUTFILE",
    "ag++ -std=c++11 -o QR QR.cc && ./QR > OUTFILE",
  ]
  Apt = ["algol68g", nil, "aspectc++"]
  def code
    <<-'end'.lines.map {|l| l.strip }.join
      %W[
        STRINGz:= 226+ 153,a:=z+ 166,b:=a+"2"+z+ 160,c:=b+"8"+z+ 165,t:="#include<iostream>"+ (10)+"int"+ (32)+"main(){puts#{d[E[PREV]]};}";
        FORiTO\ UPBtDO\ INTn:=ABSt[i];
          print( (50+n%64)+c+ (50+n%8MOD8)+c+ (50+nMOD8)+b+"J"+a)
        OD
      ]*"REPR"
    end
  end
end

class AFNIX_Aheui < CodeGen
  File = ["QR.als", "QR.aheui"]
  Cmd = ["LANG=C LD_LIBRARY_PATH=/usr/lib/afnix axi QR.als > OUTFILE", "go run vendor/goaheui/main.go QR.aheui > OUTFILE"]
  Apt = ["afnix", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        trans B(Buffer)\n
        trans O(n){\n
          B:add(Byte(+ 128 n))
        }\n
        trans f(v n){\n
          O(+(/ n 64)107)\n
          O(n:mod 64)\n
          O v
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
        while(!=(S:length)0){\n
          trans c(S:read)\n
          D(c:to-integer)\n
          f 35 39
        }\n
        f 24 149\n
        interp:library"afnix-sio"\n
        trans o(afnix:sio:OutputTerm)\n
        o:write B
      )
    END
  end
end

class Ada < CodeGen
  File = "qr.adb"
  Cmd = "gnatmake qr.adb && ./qr > OUTFILE"
  Apt = "gnat"
  def code
    <<-'END'.lines.map {|l| l.strip }.join.gsub("$$$", " ")
      %(
        with Ada.Text_Io;
        procedure qr is$$$
        begin$$$
          Ada.Text_Io.Put("#{d[PREV].gsub(N,'"&Character'+?'+'Val(10)&"')}");
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
end

class Aplus < CodeGen
  Name = "A+"
  File = "QR.+"
  Cmd = "a+ QR.+ > OUTFILE"
  Apt = "aplus-fsf"
  Code = %q(E[PREV]+"\nsys.exit 0")
end

class Zsh < CodeGen
  Name = "zsh"
  File = "QR.zsh"
  Cmd = "zsh QR.zsh > OUTFILE"
  Apt = "zsh"
  Code = %q("echo -E $'#{Q[Q[PREV,B],?']}'")
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
  Code = %q(%(write,format="#{y="";f(PREV,35){y<<",\\n"+$S;"%s"}}")+y)
end

class Yabasic < CodeGen
  File = "QR.yab"
  Cmd = "yabasic QR.yab > OUTFILE"
  Apt = "yabasic"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        sub f(s$,n)
          print(s$);:
          for i=1to n print("\\\\");:
          next:
        end sub:
        f("#{V[e[PREV],'",','):f("']}",0)
      )
    END
  end
end

class XSLT < CodeGen
  File = "QR.xslt"
  Cmd = "xsltproc QR.xslt > OUTFILE"
  Apt = "xsltproc"
  def code
    <<-'END'.lines.map {|l| l.strip }.join.gsub("$$$", "\n")
      "
        <?xml#{O=" version='1.0'"}?>$$$
        <?xml-#{I="stylesheet"} type='text/xsl'href='QR.xslt'?>$$$
        <xsl:#{I+O} xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
          <xsl:output method='text'/>
          <#{U="xsl:template"} match='/'>
            <![CDATA[#{PREV}]]>
          </#{U}>
        </xsl:#{I}>
      "
    END
  end
end

class VisualBasic_Whitespace < CodeGen
  Name = ["Visual Basic", "Whitespace"]
  File = ["QR.vb", "QR.ws"]
  Cmd = [
    "vbnc QR.vb && mono ./QR.exe > OUTFILE",
    "ruby vendor/whitespace.rb QR.ws > OUTFILE"
  ]
  Apt = ["mono-vbnc", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join(?:)
      %(Module QR
        Sub Main()
          Dim s,n,i,c As Object
          n=Chr(10)
          For Each c in"#{d[PREV].gsub N,'"& VbLf &"'}"
            s="   "
            For i=0To 7
                s &=Chr(32-(Asc(c)>>7-i And 1)*23)
            Next
            #$C(s &n &Chr(9)&n &"  ")
          Next
          #$C(n &n &n)
        End Sub
      End Module)
    END
  end
end

class VimScript < CodeGen
  Name = ["Vimscript"]
  Apt = "vim"
  File = "QR.vim"
  Cmd = "vim -EsS QR.vim > OUTFILE"
  Code = %q("let s=#{E[PREV]}\nput=s\nprint\nqa!")
end

class Verilog < CodeGen
  File = "QR.v"
  Cmd = "iverilog -o QR QR.v && ./QR -vcd-none > OUTFILE"
  Apt = "iverilog"
  Code = %q(%(module QR;initial begin #{f(PREV,3){%($write("%s",#$S);)+N}}end endmodule))
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
            stdout.printf("%c%c",v/256,v%256);
        }
        void main(){
          int[]a;
          p({19796,26724,0,6,0,1,480,19796,29291,#{s=PREV;W=s.size*72+4;"%d,%d"%[W/65536,W%65536]}});
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
      "let s=#{E[PREV]},i=0,t='k';while(s[i])t='\\x60.'+s[i++]+t;console.log(t)"
    END
  end
end

class Tcsh_Thue < CodeGen
  Name = ["tcsh", "Thue"]
  File = ["QR.tcsh", "QR.t"]
  Cmd = ["tcsh QR.tcsh > OUTFILE", "ruby vendor/thue.rb QR.t > OUTFILE"]
  Apt = ["tcsh", nil]
  Code = %q(%(echo 'a::=~#{Q[Q[PREV,B],?!].gsub(?',%('"'"'))}'"\\\\n::=\\\\na"))
end

class Tcl < CodeGen
  File = "QR.tcl"
  Cmd = "tclsh QR.tcl > OUTFILE"
  Apt = "tcl"
  Code = %q(%(puts "#{Q[e[PREV],/[\[\]$]/]}"))
end

class StandardML_Subleq < CodeGen
  Name = ["Standard ML", "Subleq"]
  File = ["QR.sml", "QR.sq"]
  Cmd = ["mlton @MLton fixed-heap 200M -- QR.sml && ./QR > OUTFILE", "ruby vendor/subleq.rb QR.sq > OUTFILE"]
  Apt = ["mlton", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        fun p n=print(Int.toString n^" ");
        p 0;p 0;p 130;
        List.tabulate(127,p);
        String.map(fn c=>(p(3+ord c);print"-1 0 ";c))#{E[PREV]};
        print"0 0 -1";
      )
    END
  end
end

class Squirrel < CodeGen
  File = "QR.nut"
  Cmd = "squirrel QR.nut > OUTFILE"
  Apt = "squirrel3"
  Code = %q("print"+E[PREV])
end

class Smalltalk < CodeGen
  File = "QR.st"
  Cmd = "gst QR.st > OUTFILE"
  Apt = "gnu-smalltalk"
  Code = %q("Transcript show: '#{d[PREV,?']}';cr")
end

class Scilab_Sed_Shakespeare_SLang < CodeGen
  Name = ["Scilab", "sed", "Shakespeare", "S-Lang"]
  File = ["QR.sci", "QR.sed", "QR.spl", "QR.sl"]
  Cmd = [
    "scilab-cli -nb -f QR.sci > OUTFILE",
    "sed -E -f QR.sed QR.sed > OUTFILE",
    "./vendor/local/bin/spl2c < QR.spl > QR.spl.c && gcc -o QR -I ./vendor/local/include -L ./vendor/local/lib QR.spl.c -lspl -lm && ./QR > OUTFILE",
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
        printf("
          1d;
          s/.//;
          s/1/ the sum of a son and0/g;
          s/0/ twice/g;
          s/2/You are as bad as/g;
          s/3/ a son!Speak your mind!/g\\n
          #The Relay of Quine.\\n
          #Ajax, a man.\\n
          #Ford, a man.\\n
          #Act i: Quine.\\n
          #Scene i: Relay.\\n
          #[Enter Ajax and Ford]\\n
          #Ajax:\\n
          #");
        function[]=f(s);
          for i=1:2:length(s),
            printf("2%s3",part(dec2bin(hex2dec(part(s,i:i+1))),$:-1:2)),
          end;
        endfunction\n
        #{
          s,v=rp[PREV,127..255];
          f(
            %(
              variable s=`#{s.gsub(/.{1,234}/){$&.gsub("`",%(`+"`"+`))+"`+\n`"}}`,i;
              for(i=0;i<129;i++)
                s=strreplace(
                  s,
                  pack("C",255-i),
                  substrbytes(`#{v[0,99]}`+\n`#{v[99..-1]}`,i*2+1,2));
              printf("%s",s)
            ),7
          ){
            "f('%s')\n"%$s.unpack("H*")
          }
        }
        printf("\\n#[Exeunt]");
        quit
      )
    END
  end
end

class Scheme < CodeGen
  File = "QR.scm"
  Cmd = "$(SCHEME) QR.scm > OUTFILE"
  Apt = "guile-2.0"
  Code = %q(%((display "#{e[PREV]}")))
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
end

class Rust < CodeGen
  File = "QR.rs"
  Cmd = "rustc QR.rs && ./QR > OUTFILE"
  Apt = "rustc"
  Code = %q(%(fn main(){print!("{}",#{E[PREV]});}))
end

class Ruby < CodeGen
  File = "QR.rb"
  Cmd = "ruby QR.rb > OUTFILE"
  Apt = "ruby"
  Code = nil
end

load "code-gen-pool.rb" if ENV["ALL"]

GenSteps = CodeGen::List.map {|s| s.gen_step }
RunSteps = CodeGen::List.reverse.flat_map {|s| s.run_steps }
