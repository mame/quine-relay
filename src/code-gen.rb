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
  def f(s,n)s.gsub(/.{1,#{n*255}}/m){yield$S=E[$s=$&]}end;
  Q=->s,t=?${s.gsub(t){B+$&}};
  M=->s{"<stdio.h>#{N}int main(){puts#{E[s]};return 0;}"};
  V=->s,a,z{s.gsub(/(#{B*4})+/){a+"#{$&.size/2}"+z}};
  C="Console.Write";
  $D="program QR";
  $G=" contents of"+$F=" the mixing bowl";
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
  Cmd = ["php QR.php > OUTFILE", "npiet QR.png > OUTFILE"]
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
  Code = %q("#$D(output);begin write(#{f(PREV,1){"'#$s',"}}'')end.")
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
  Cmd = [
    "octave -qf QR.octave > OUTFILE",
    "ruby vendor/ook-to-bf.rb QR.ook QR.ook.bf && $(BF) QR.ook.bf > OUTFILE"
  ]
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
        printf("%%s",t{:})
      )
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
  Code = %q("#import"+M[PREV])
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
  File = "QR.mk"
  Cmd = "make -f QR.mk > OUTFILE"
  Apt = "make"
  Code = %q(%(all:\n\t@echo '#{d[PREV,?$].gsub(?'){"'\\\\''"}}'))
end

class Lua < CodeGen
  File = "QR.lua"
  Cmd = "lua QR.lua > OUTFILE"
  Apt = "lua5.2"
  Code = %q("print"+E[PREV])
end

class Logo_LOLCODE < CodeGen
  File = ["QR.logo", "QR.lol"]
  Cmd = ["logo QR.logo > OUTFILE", "lci QR.lol > OUTFILE"]
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
  Cmd = "mv QR.bc QR.bc.bak && llvm-as QR.ll && lli QR.bc > OUTFILE && mv QR.bc.bak QR.bc"
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

class JavaScript < CodeGen
  File = "QR.js"
  Cmd = "$(JAVASCRIPT) QR.js > OUTFILE"
  Apt = "rhino"
  Code = %q("s=#{E[PREV]};typeof print=='function'?print(s):console.log('%s',s)")
end

class Java < CodeGen
  File = "QR.java"
  Cmd = "javac QR.java && java QR > OUTFILE"
  Apt = "openjdk-6-jdk"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        class QR{
          public static void main(String[]v){
            String c[]=new String[9000],y="",z=y,s="#{
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
  Cmd = "jasmin QR.j && java QR > OUTFILE"
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
    "mv QR.c QR.c.bak && f2c QR.f && $(CC) -o QR QR.c -L/usr/lib -lf2c -lm && mv QR.c.bak QR.c && ./QR > OUTFILE",
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

class Dc < CodeGen
  Name = "dc"
  File = "QR.dc"
  Cmd = "dc QR.dc > OUTFILE"
  Apt = "dc"
  Code = %q("[#{PREV}]pq")
end

class D < CodeGen
  File = "QR.d"
  Cmd = "gdc -o QR QR.d && ./QR > OUTFILE"
  Apt = "gdc"
  Code = %q("import std.stdio;void main(){write#{E[PREV]};}")
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

class CDuce_Chef < CodeGen
  File = ["QR.cd", "QR.chef"]
  Cmd = [
    "cduce QR.cd > OUTFILE",
    "PERL5LIB=vendor/local/lib/perl5 compilechef QR.chef QR.chef.pl && perl QR.chef.pl > OUTFILE"
  ]
  Apt = ["cduce", nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join("\\n")
%(let f(c :Int):Latin1=if c=127then""else(string_of c@" g caffeine "@string_of c@"
")@f(c+1)in print("Quine Relay Coffee.

Ingredients.
"@f 10@"
Method.
");let g(String ->[])
[c;t]->print("Put caffeine "@string_of(int_of_char c)@" into#$F.
");g t
|_ ->print("Liquify#$G.
Pour#$G into the baking dish.

Serves 1.
")in g#{E[PREV.reverse]})
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
            System.#{C+E[(PREV)]};
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
        }
      "
    END
  end
end

class C < CodeGen
  File = "QR.c"
  Cmd = "$(CC) -o QR QR.c && ./QR > OUTFILE"
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

class Awk_Bc_Befunge_BLC8 < CodeGen
  Name = ["Awk", "bc", "Befunge", "BLC8"]
  File = ["QR.awk", "QR.bc", "QR.bef", "QR.Blc"]
  Cmd = [
    "awk -f QR.awk > OUTFILE",
    "BC_LINE_LENGTH=3000000 bc -q QR.bc > OUTFILE",
    "cfunge QR.bef > OUTFILE",
    "ruby vendor/blc.rb < QR.Blc > OUTFILE"
  ]
  Apt = ["gawk", "bc", nil, nil]
  def code
    # 389**6+ = 22
    # 29*     = 18
    # 44*6+   = 22
    #
    # 18 x   = 00010010 x = \a (\b b) x = \a x = K x
    # 22 x y = 00010110 x y = \a a x y = cons x y
    # 4 222  = 00000100 11011110 = \a \b (\c b) (????) = \a \b b = false
    # 4 226  = 00000100 11100010 = \a \b (\c a) (\c c) = \a \b a = true
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        BEGIN{
          s=#{E[PREV.tr B,?!]};
          gsub(/!/,"\\\\",s);
          for(
            print"
              define void f(n){
                \\"00g,\\";
                for(m=128;m;m/=2){
                  \\"00g,4,:\\";
                  if(n/m%2<1)\\"4+\\";
                  \\",\\";
                };
                \\"4,:,\\"
              }
              \\"389**6+44*6+00p29*,\\";
            ";
            ++j<=length(s);
            print"f("n");"
          )
            for(n=9;substr(s,j,1)!=sprintf("%c",++n););
          print"\\"4,:,@\\"\\nquit"
        }
      )
    END
  end
end

class ATS < CodeGen
  File = "QR.dats"
  Cmd = "atscc -o QR QR.dats && ./QR > OUTFILE"
  Apt = "ats-lang-anairiats"
  Code = %q(%(implement main()=print)+E[PREV])
end

class Asymptote < CodeGen
  File = "QR.asy"
  Cmd = "asy QR.asy > OUTFILE"
  Apt = "asymptote"
  Code = %q(%(write('#{Q[e[PREV],?']}');))
end

class ALGOL68_Ante < CodeGen
  File = ["QR.a68", "QR.ante"]
  Cmd = ["a68g QR.a68 > OUTFILE", "ruby vendor/ante.rb QR.ante > OUTFILE"]
  Apt = ["algol68g", nil]
  def code
    <<-'end'.lines.map {|l| l.strip }.join
      %W[
        STRINGz:= 226+ 153,a:=z+ 166,b:=a+"2"+z+ 160,c:=b+"8"+z+ 165,t:="#{d[PREV]}";
        FORiTO\ UPBtDO\ INTn:=ABSt[i];
          print( (50+n%64)+c+ (50+n%8MOD8)+c+ (50+nMOD8)+b+"J"+a)
        OD
      ]*"REPR"
    end
  end
end

class AFNIX < CodeGen
  File = "QR.als"
  Cmd = "axi QR.als > OUTFILE"
  Apt = "afnix"
  Code = %q(%(print"#{e[PREV]}"))
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
        end;
      )
    END
  end
end

class Aplus < CodeGen
  Name = "A+"
  File = "QR.+"
  Cmd = "a+ QR.+ > OUTFILE"
  Apt = "aplus-fsf"
  Code = %q(E[PREV]+"\nsys.exit 0")
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
            #{C}(s &n &Chr(9)&n &"  ")
          Next
          #{C}(n &n &n)
        End Sub
      End Module)
    END
  end
end

class Verilog < CodeGen
  File = "QR.v"
  Cmd = "iverilog -o QR QR.v && ./QR -vcd-none > OUTFILE"
  Apt = "iverilog"
  Code = %q(%(module QR;initial begin #{f(PREV,3){%($write("%s",#$S);)+N}}end endmodule))
end

class Vala < CodeGen
  File = "QR.vala"
  Cmd = "valac QR.vala && ./QR > OUTFILE"
  Apt = "valac"
  Code = %q(%(int main(){print#{d[E[PREV],?%]};return 0;}))
end

class Tcl_Thue_Unlambda < CodeGen
  File = ["QR.tcl", "QR.t", "QR.unl"]
  Cmd = ["tclsh QR.tcl > OUTFILE", "ruby vendor/thue.rb QR.t > OUTFILE", "ruby vendor/unlambda.rb QR.unl > OUTFILE"]
  Apt = ["tcl", nil, nil]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        proc f {n} {string repeat "\\\\" $n};
        puts a::=~[regsub -all {.} "#{V[Q[e[PREV.reverse],/[\[\]$]/],"[f ",?]]}" \\x60.&]k\\n::=\\na
      )
    END
  end
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

class SPL < CodeGen
  File = "QR.spl"
  Cmd = "splrun QR.spl > OUTFILE"
  Apt = "spl-core"
  Code = %q(%(write#{Q[E[PREV]]};))
end

class Smalltalk < CodeGen
  File = "QR.st"
  Cmd = "gst QR.st > OUTFILE"
  Apt = "gnu-smalltalk"
  Code = %q("Transcript show: '#{d[PREV,?']}';cr")
end

class Shell_SLang < CodeGen
  Name = ["Shell (bash)", "S-Lang"]
  File = ["QR.bash", "QR.sl"]
  Cmd = ["bash QR.bash > OUTFILE", "slsh QR.sl > OUTFILE"]
  Apt = ["bash", "slsh"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %[echo -En "#{Q[e[e[PREV]]]}"|sed -E -e 's/([^\\\\]|\\\\.){1,120}/printf("%s","\\0");\\n/g']
    END
  end
end

class Scilab < CodeGen
  File = "QR.sci"
  Cmd = "scilab -nw -nb -f QR.sci > OUTFILE"
  Apt = "scilab"
  Code = %q(%(#{f(PREV,7){%(printf("%s","#{d[d[$s],?']}")\n)}}quit))
end

class Scheme < CodeGen
  File = "QR.scm"
  Cmd = "$(SCHEME) QR.scm > OUTFILE"
  Apt = "gauche"
  Code = %q(%((display "#{e[PREV]}")))
end

class Scala < CodeGen
  File = "QR.scala"
  Cmd = "scalac QR.scala && scala QR > OUTFILE"
  Apt = "scala"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        object QR extends App{
          #{f(PREV,196){%(print#$S;)}}
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
