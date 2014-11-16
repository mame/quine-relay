# A source for generating Quine Relay

# A class that generates Ruby code that generates a code (in a language X) that prints PREV.
class CodeGen
  # File = source file name of X
  # Cmd = command for execution/compilation
  # Apt = ubuntu package name of the interpreter/compiler of X

  List = []
  def self.inherited(c)
    List << c
  end

  def self.gen_code(s)
    new.code.sub("PREV"){ s }.chomp
  end

  def code
    self.class::Code
  end

  Step = Struct.new(:name, :src, :cmd, :apt)

  def self.steps
    a = []
    a << (defined?(self::Name) ? [*self::Name] : self.to_s.split("_"))
    a << [*self::File]
    a << [*self::Cmd]
    a << [*self::Apt]
    a.transpose.map do |name, src, cmd, apt|
      Step[name, src, cmd, apt]
    end
  end

  # Common part
  PROLOGUE = <<-'END'.lines.map {|l| l.strip }.join
  B=92.chr;
  g=32.chr;
  N=10.chr;
  n=0;
  e=->s{Q[Q[s,B],?"].gsub(N,B+?n)};
  E=->s{'("'+e[s]+'")'};
  d=->s,t=?"{s.gsub(t){t+t}};
  Q=->s,t=?${s.gsub(t){B+$&}};
  M=->s{"<stdio.h>#{N}int main(){puts#{E[s]};return 0;}"};
  V=->s,a,z{s.gsub(/(#{B*4})+/){a+"#{$&.size/2}"+z}};
  $D="program QR";
  END

  def self.setup_dir(name)
    dir = File.join(File.dirname(__dir__), name)
    Dir.mkdir(dir) unless File.directory?(dir)
    Dir.chdir(dir)
  end
end


class Python_R_Ratfor_REXX < CodeGen
  File = ["QR.py", "QR.R", "QR.r", "QR.rexx"]
  Cmd = [
    "python QR.py > OUTFILE",
    "R --slave -f QR.R > OUTFILE",
    "ratfor -o QR.r.f QR.r && gfortran -o QR QR.r.f && ./QR > OUTFILE",
    "rexx ./QR.rexx > OUTFILE"
  ]
  Apt = ["python", "r-base", "ratfor", "regina-rexx"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        for c in"".join(["say '%s'\\n"%l for l in#{E[d[PREV,?']]}.split("\\n")]):
          print('cat("r=fput(char(%d))\\n")'%ord(c))\n
        print('cat("end\\n")')
      )
    END
  end
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
      %(
        (___________)dup =
        /s(|     .   |)def
        (#{Q[PREV,B]}){
          9 7{
            exch dup 1 and 79 mul 32 add exch 2 idiv 3 1 roll s exch 2 index exch put 1 sub dup 6 eq{1 sub}if
          }repeat s = pop pop
        }forall = quit
      )
    END
  end
end

class Pike < CodeGen
  File = "QR.pike"
  Cmd = "pike QR.pike > OUTFILE"
  Apt = "pike7.8"
  Code = %q("int main(){write#{E[PREV]};return 0;}")
end

class PHP_Piet < CodeGen
  File = ["QR.php", "QR.png"]
  Cmd = ["php QR.php > OUTFILE", "vendor/npiet-*/npiet QR.png > OUTFILE"]
  Apt = ["php5-cli", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        <?php function f($n){return str_repeat("\\\\",$n);};
          $f="f";
          $z=3+$w=strlen($s=#{V[Q[E[PREV]],"{$f(",")}"]})*3;
          echo"\\x89PNG\\r\\n\\x1a\\n";
          $m="";
          $t="\\xc0\\0\\xff";
          for($i=-1;$i<128*$z;
              $m.=$c--?
                ($w-$c||$i>$z)&&$i/$z<($c<$w?ord($s[(int)($c/3)]):$c--%3+2)?
                  $t[2].$t[$c%3%2].$t[$c%3]:"\\0\\0\\0":"\\0"
          )
            $c=++$i%$z;
          foreach(array(
            "IHDR".pack("NNCV",$w+2,128,8,2),
            "IDAT".gzcompress($m),
            "IEND"
          )as$d)
            echo pack("NA*N",strlen($d)-4,$d,crc32($d));
        ?>
      )
    END
  end
end

class Perl < CodeGen
  File = "QR.pl"
  Cmd = "perl QR.pl > OUTFILE"
  Apt = "perl"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      (
        p="eval";
        %(
          $_="#{
            s=PREV;
            (s+N*(-s.size%6)).unpack("B*")[0].
              gsub(/.{6}/){n=$&.to_i 2;((n+14)/26*6+n+47).chr}
          }";
          s|.|$n=ord$&;substr unpack(B8,chr$n-int($n/32)*6-41),2|eg;
          print pack"B*",$_
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
  Code = %q("#$D(output);begin write(#{(PREV).gsub(/.{1,255}/){|s|"'#{s}',"}}'')end.")
end

class ParrotAsm < CodeGen
  Name = "Parrot asm"
  File = "QR.pasm"
  Cmd = "parrot QR.pasm > OUTFILE"
  Apt = "parrot"
  Code = %q(%(say"#{e[PREV]}"\nend))
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
  Cmd = ["octave -qf QR.octave > OUTFILE", "ruby vendor/ook.rb QR.ook > OUTFILE"]
  Apt = ["octave", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        s=toascii#{E[PREV]};
        t=num2cell(b=11-ceil(s/13));
        for n=1:9
            m={};
            for i=1:141
              f=@(x,y,n)repmat(["Ook" x " Ook" y 32],[1 abs(n)]);
              m(i)=[f(z=46,63,n) f(q=z-(i<13)*13,q,i-13) f(33,z,1) f(63,z,n)];
            end;
            t(x)=m(diff([0 s(x=b==n)])+13);
        end;
        printf(strjoin(t,""))
      )
    END
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
  Code = %q("#import"+M[PREV])
end

class NodeJS < CodeGen
  Name = "NodeJS"
  File = "QR.js"
  Cmd = "$(NODE) QR.js > OUTFILE"
  Apt = "nodejs"
  Code = %q("require('util').print#{E[PREV]}")
end

class Nickle < CodeGen
  File = "QR.5c"
  Cmd = "nickle QR.5c > OUTFILE"
  Apt = "nickle"
  Code = %q(%(printf#{E[PREV]}))
end

class Neko < CodeGen
  File = "QR.neko"
  Cmd = "nekoc QR.neko && neko QR.n > OUTFILE"
  Apt = "neko"
  Code = %q("$print#{E[PREV+N]};")
end

class NASM < CodeGen
  File = "QR.asm"
  Cmd = "nasm -felf QR.asm && ld -m elf_i386 -o QR QR.o && ./QR > OUTFILE"
  Apt = "nasm"
  def code
    <<-'END'.lines.map {|l| l.strip }.join("\\n")
      %(m:db\x60#{e[s=PREV+N]}\x60
      global _start
      _start:mov edx,#{s.size}
      mov ecx,m
      mov ebx,1
      mov eax,4
      int 128
      mov ebx,0
      mov eax,1
      int 128)
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
        .method static void Main()
        {
          .entrypoint ldstr"#{e[PREV]}"
          call void [mscorlib]System.Console::Write(string)
          ret
        }
      )
    END
  end
end

class Maxima < CodeGen
  File = "QR.mac"
  Cmd = "maxima -q --init-mac=QR.mac > OUTFILE"
  Apt = "maxima"
  Code = %q("linel:99999;print#{E[PREV]};quit();")
end

class Makefile < CodeGen
  File = "QR.makefile"
  Cmd = "make -f QR.makefile > OUTFILE"
  Apt = "make"
  Code = %q(%(all:\n\t@echo '#{d[Q[PREV,B],?$].gsub(?'){%('"'"')}}'))
end

class Lua < CodeGen
  File = "QR.lua"
  Cmd = "lua QR.lua > OUTFILE"
  Apt = "lua5.2"
  Code = %q("print"+E[PREV])
end

class Logo_LOLCODE < CodeGen
  File = ["QR.logo", "QR.lol"]
  Cmd = ["logo QR.logo > OUTFILE", "vendor/lci-*/lci QR.lol > OUTFILE"]
  Apt = ["ucblogo", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        PR "HAI\\ 1.2 PR "VISIBLE\\ "#{
          Q[PREV.gsub(/[:"]/,":\\0"),/[ \\\\\t;"(){}\[\]]/]
        }" PR "KTHXBYE BYE
      )
    END
  end
end

class LLVMAsm < CodeGen
  Name = "LLVM asm"
  File = "QR.ll"
  Cmd = "llvm-as QR.ll && lli QR.bc > OUTFILE"
  Apt = "llvm"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        @s=global[#{i=(s=PREV).size+1}x i8]
          c"#{s.gsub(/[\\"]/){"\\%X"%$&.ord}}\\00"
        declare i32@puts(i8*)
        define i32@main(){
          %1=call i32@puts(i8*getelementptr([#{i}x i8]*@s,i32 0,i32 0))
          ret i32 0
        }
      )
    END
  end
end

class Java < CodeGen
  File = "QR.java"
  Cmd = "javac QR.java && CLASSPATH=. java QR > OUTFILE"
  Apt = "openjdk-6-jdk"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class QR{
          public static void main(String[]v){
            String c[]=new String[8000],y="",z=y,s="#{
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
            for(;i<s.length();){
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
  Cmd = "jasmin QR.j && CLASSPATH=. java QR > OUTFILE"
  Apt = "jasmin-sable"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        .class public QR\n
        .super #{$T="java/io/PrintStream"}\n
        .method public static main([L#{S="java/lang/"}String;)V ;]\n
        .limit stack 2\n
        getstatic #{S}System/out L#$T;\n
        ldc "#{e[PREV]}"\n
        invokevirtual #$T/println(L#{S}String;)V\n
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
    "mv QR.c QR.c.bak && ick -bfO QR.i && mv QR.c.bak QR.c && ./QR > OUTFILE"
  ]
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

class Haskell < CodeGen
  File = "QR.hs"
  Cmd = "ghc QR.hs && ./QR > OUTFILE"
  Apt = "ghc"
  Code = %q(("main=putStr"+E[PREV]))
end

class Groovy < CodeGen
  File = "QR.groovy"
  Cmd = "groovy QR.groovy > OUTFILE"
  Apt = "groovy"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        print'#{e[PREV.tr(B,?&)]}'.tr('&','\\\\\\\\');
      )
    END
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

class Forth_FORTRAN77_Fortran90 < CodeGen
  File = ["QR.fs", "QR.f", "QR.f90"]
  Cmd = [
    "gforth QR.fs > OUTFILE",
    "mv QR.c QR.c.bak && f2c QR.f && ${CC} -o QR QR.c -L/usr/lib -lf2c && mv QR.c.bak QR.c && ./QR > OUTFILE",
    "gfortran -o QR QR.f90 && ./QR > OUTFILE"
  ]
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
          S\\" #{e[PREV]}" DUP FOR S" &A,&" C NEXT
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

class CommonLisp < CodeGen
  Name = "Common Lisp"
  File = "QR.lisp"
  Cmd = "clisp QR.lisp > OUTFILE"
  Apt = "clisp"
  Code = %q(%((write-line"#{e[PREV]}")))
end

class CoffeeScript < CodeGen
  File = "QR.coffee"
  Cmd = "coffee QR.coffee > OUTFILE"
  Apt = "coffeescript"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        (f=(n)->Array(n+1).join "\\\\");
        console.log("%s",#{V[E[PREV],'#{f(',')}']})
      )
    END
  end
end

class Clojure_Cobol < CodeGen
  File = ["QR.clj", "QR.cob"]
  Cmd = ["clojure QR.clj > OUTFILE", "cobc -O2 -x QR.cob && ./QR > OUTFILE"]
  Apt = ["clojure1.4", "open-cobol"]
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
                  "\\"&")
               (re-seq #".{1,45}"
                  "#{e[PREV]}"))
             ["    \\" \\"."
              "STOP RUN."])]
          (println(str"#{g*8}"s)))
        )
    END
  end
end

class CSharp < CodeGen
  Name = "C#"
  File = "QR.cs"
  Cmd = "mcs QR.cs && mono QR.exe > OUTFILE"
  Apt = "mono-mcs"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class Program{
          public static void Main(){
            System.Console.Write(#{E[PREV.tr B,?~]}.Replace("~","\\\\"));
          }
        }
      )
    END
  end
end

class Cplusplus < CodeGen
  Name = "C++"
  File = "QR.cpp"
  Cmd = "${CXX} -o QR QR.cpp && ./QR > OUTFILE"
  Apt = "g++"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        #include<iostream>\n
        int main(){
          std::cout<<#{E[PREV]};
        }
      "
    END
  end
end

class C < CodeGen
  File = "QR.c"
  Cmd = "${CC} -o QR QR.c && ./QR > OUTFILE"
  Apt = "gcc"
  Code = %q("#include"+M[PREV])
end

class Boo_Brainfuck < CodeGen
  File = ["QR.boo", "QR.bf"]
  Cmd = ["booi QR.boo > OUTFILE", "$(BF) QR.bf > OUTFILE"]
  Apt = ["boo", "bf"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        f={n as int|'\\\\'*n};
        a=0;
        s=#{V[Q[E[PREV]],"$(f(","))"]};
        for i in range(len(s)):
          b as int=s[i];
          a-=b;
          print(('+'*-a if 0>a else'-'*a)+'.');
          a=b;
      )
    END
  end
end

class Awk < CodeGen
  File = "QR.awk"
  Cmd = "awk -f QR.awk > OUTFILE"
  Apt = "gawk"
  Code = %q(%(BEGIN{s=#{E[PREV.tr B,?!]};gsub(/!/,"\\\\\\\\",s);print s}))
end

class ALGOL68 < CodeGen
  File = "QR.a68"
  Cmd = "a68g QR.a68 > OUTFILE"
  Apt = "algol68g"
  Code = %q(%(print("#{d[PREV]}")))
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
          Ada.Text_Io.Put_Line("#{d[PREV]}");
        end qr;
      )
    END
  end
end

class Vala_Verilog_Whitespace < CodeGen
  File = ["QR.vala", "QR.v", "QR.ws"]
  Cmd = [
    "valac QR.vala && ./QR > OUTFILE",
    "iverilog -o QR QR.v && ./QR -vcd-none > OUTFILE",
    "ruby vendor/whitespace.rb QR.ws > OUTFILE"
  ]
  Apt = ["valac", "iverilog", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %[
        int main(){
          string s=#{E[PREV]};
          int i,j;
          print("module QR;initial begin ");
          for(i=0;i<s.length;i++){
            print("$write(\\"   ");
            for(j=6;j>=0;j--)
              print((s[i]>>j)%2>0?"\\\\t":" ");
            print("\\\\n\\\\t\\\\n  \\\");");
          }
          print("$display(\\"\\\\n\\\\n\\");end endmodule");
          return 0;
        }
      ]
    END
  end
end

class Tcl_Unlambda < CodeGen
  File = ["QR.tcl", "QR.unl"]
  Cmd = ["tclsh QR.tcl > OUTFILE", "ruby vendor/unlambda.rb QR.unl > OUTFILE"]
  Apt = ["tcl", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        proc f {n} {string repeat "\\\\" $n} ;
        puts [regsub -all {.} "#{V[Q[e[PREV.reverse],/[\[\]$]/],"[f ",?]]}" \\x60.&]k
      )
    END
  end
end

class Smalltalk < CodeGen
  File = "QR.st"
  Cmd = "gst QR.st > OUTFILE"
  Apt = "gnu-smalltalk"
  Code = %q("Transcript show: '#{d[PREV,?']}';cr")
end

class Shell < CodeGen
  File = "QR.bash"
  Cmd = "bash QR.bash > OUTFILE"
  Apt = "bash"
  Code = %q(%(printf %s "#{Q[e[PREV]]}"))
end

class Scilab < CodeGen
  File = "QR.sci"
  Cmd = "scilab -nw -nb -f QR.sci > OUTFILE"
  Apt = "scilab"
  Code = %q(PREV.gsub(/.{1,3000}/){%(printf("%s","#{d[d[$&],?']}")\n)}+"quit")
end

class Scheme < CodeGen
  File = "QR.scm"
  Cmd = "$(SCHEME) QR.scm > OUTFILE"
  Apt = "gauche"
  Code = %q(%((display "#{e[PREV]}")))
end

class Scala < CodeGen
  File = "QR.scala"
  Cmd = "scalac QR.scala && CLASSPATH=. scala QR > OUTFILE"
  Apt = "scala"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        object QR extends App{
          print#{E[PREV]};
        }
      )
    END
  end
end

class Ruby < CodeGen
  File = "QR.rb"
  Cmd = "ruby QR.rb > OUTFILE"
  Apt = "ruby2.0"
end
