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
  PROLOGUE = <<-END.split.join
  B=92.chr;
  N=?\\n;
  n=0;
  e=->(s){s.gsub(/[\#{B+B+N}"]/){B+(N==$&??n:$&)}};
  E=->(s){'("'+e[s]+'")'};
  d=->(s,t=?"){s.gsub(t){t+t}};
  D=->(s,t=?@){s.tr(B,t)};
  Q=->(s,t=?$){s.gsub(t){B+$&}};
  END

  def self.setup_dir(name)
    dir = File.join(File.dirname(__dir__), name)
    Dir.mkdir(dir) unless File.directory?(dir)
    Dir.chdir(dir)

    system("cp", File.join(__dir__, "unlambda.rb"), dir)
    system("cp", File.join(__dir__, "whitespace.rb"), dir)
  end
end


class REXX < CodeGen
  File = "QR.rexx"
  Cmd = "rexx ./QR.rexx > OUTFILE"
  Apt = "regina-rexx"
  Code = %q(PREV.gsub(/.+/){"say \"#{d[$&]}\""})
end

class R < CodeGen
  File = "QR.R"
  Cmd = "R --slave < QR.R > OUTFILE"
  Apt = "r-base"
  Code = %q("cat"+E[PREV])
end

class Python < CodeGen
  File = "QR.py"
  Cmd = "python QR.py > OUTFILE"
  Apt = "python"
  Code = %q("print"+E[PREV])
end

class Prolog < CodeGen
  File = "QR.prolog"
  Cmd = "swipl -q -t qr -f QR.prolog > OUTFILE"
  Apt = "swi-prolog"
  Code = %q("qr:-write('#{Q[e[PREV],?']}'),nl,halt.")
end

class Pike < CodeGen
  File = "QR.pike"
  Cmd = "pike QR.pike > OUTFILE"
  Apt = "pike7.8"
  Code = %q("int main(){write#{E[PREV]};return 0;}")
end

class PHP < CodeGen
  File = "QR.php"
  Cmd = "php QR.php > OUTFILE"
  Apt = "php5-cli"
  Code = %q(%(<?php echo"#{Q[e[PREV]]}"?>))
end

class Perl < CodeGen
  File = "QR.pl"
  Cmd = "perl QR.pl > OUTFILE"
  Apt = "perl"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      [
        *%(
          $_="#{
            s=PREV;
            (s+N*(-s.size%6)).bytes.map{|n|"%07b"%n}.join.
              scan(/.{6}/).map{|n|n=n.to_i(2);((n/26*6+n+19)%83+46).chr}*""
          }";
          s|.|$n=ord$&;substr unpack(B8,chr$n-($n<58?-6:$n<91?65:71)),2|eg;
          s/.{7}/0$&/g;
          print pack B.length,$_
        ).scan(%r(([ .0-9A-Za-z]+)|(.))).reverse.
          map{|a,b|(b)?"s//chr #{b.ord}/e":"s//#{a}/"},
        "eval"
      ]*" x "
    END
  end
end

class Pascal < CodeGen
  File = "QR.pas"
  Cmd = "fpc QR.pas && ./QR > OUTFILE"
  Apt = "fp-compiler"
  Code = %q("program QR(output);begin #{(PREV).scan(/.{1,255}/).map{|s|"write('#{s}');"}*""}end.")
end

class ParrotAsm < CodeGen
  Name = "Parrot asm"
  File = "QR.pasm"
  Cmd = "parrot QR.pasm > OUTFILE"
  Apt = "parrot"
  Code = %q(%(say"#{e[PREV]}"\nend\n))
end

class Octave < CodeGen
  File = "QR.octave"
  Cmd = "octave -qf QR.octave > OUTFILE"
  Apt = "octave"
  Code = %q("printf"+E[PREV])
end

class OCaml < CodeGen
  File = "QR.ml"
  Cmd = "ocaml QR.ml > OUTFILE"
  Apt = "ocaml"
  Code = %q("print_string"+E[PREV])
end

class NodeJS_ObjC < CodeGen
  Name = %w(NodeJS Objective-C)
  File = ["QR.js", "QR.m"]
  Cmd = ["nodejs QR.js > OUTFILE", "gcc -o QR QR.m && ./QR > OUTFILE"]
  Apt = ["nodejs", "gobjc"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        var u=require('util');
        u.print('#import<stdio.h>\\n');
        u.print(#{
          E[D[%(
            int main(){puts#{E[PREV]};return 0;}
          )]]
        }.replace(/@/g,String.fromCharCode(92)))
      "
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
          call void [mscorlib]System.Console::WriteLine(string)
          ret
        }
      )
    END
  end
end

class Makefile < CodeGen
  File = "QR.makefile"
  Cmd = "make -f QR.makefile > OUTFILE"
  Apt = "make"
  Code = %q(%(all:\n\t@printf %s "#{e[PREV]}"))
end

class Lua < CodeGen
  File = "QR.lua"
  Cmd = "lua QR.lua > OUTFILE"
  Apt = "lua5.2"
  Code = %q("print"+E[PREV])
end

class Logo < CodeGen
  File = "QR.logo"
  Cmd = "ucblogo QR.logo > OUTFILE"
  Apt = "ucblogo"
  Code = %q(%(PRINT "#{Q[PREV,/[ \\\\\t;"(){}\[\]]/]}\nBYE))
end

class LLVMAsm < CodeGen
  Name = "LLVM asm"
  File = "QR.ll"
  Cmd = "llvm-as QR.ll && lli QR.bc > OUTFILE"
  Apt = "llvm"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        @s=internal constant[#{i=(s=PREV).size+1} x i8]
          c"#{s.gsub(/[\\\n"]/){B+"%02\x58"%$&.ord}}\\00"
        declare i32@puts(i8*)
        define i32@main(){
          start:
          %0=call i32@puts(i8* getelementptr inbounds([#{i} x i8]*@s,i32 0,i32 0))
          ret i32 0
        }
      )
    END
  end
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
            String c[]=new String[8000],y="",z=y,s="#{
              z=t=(0..r=q=126).map{|n|[n,[]]};
              a=[];
              PREV.bytes{|n|
                r,z=z[n]||(
                  a<<r;
                  q<5624&&z[n]=[q+=1,[]];
                  t[n])
              };
              a<<r;
              t=[*43..123]-[64,*92..96];
              a.map{|n|t[n/75].chr+t[n%75].chr}*""
            }";
            int i,n,q=0,t;
            for(n=0;++n<126;)c[n]=""+(char)n;
            for(i=0;++i<s.length();){
              t=s.charAt(i);
              q=q*75+t-t/64-t/92*5-43;
              if(i%2>0){
                y=q<n?c[q]:y;
                c[n++]=z+=y.charAt(0);
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
        .super java/lang/Object\n
        .method public static main([Ljava/lang/String;)V\n
        .limit stack 2\n
        getstatic java/lang/System/out Ljava/io/PrintStream;\n
        ldc "#{e[PREV]}"\n
        invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n
        return\n
        .end method
      )
    END
  end
end

class Haskell_Icon_INTERCAL < CodeGen
  File = ["QR.hs", "QR.icn", "QR.i"]
  Cmd = [
    "runghc QR.hs > OUTFILE",
    "icont -s QR.icn && ./QR > OUTFILE",
    "mv QR.c QR.c.bak && CC=tcc ick -b QR.i && mv QR.c.bak QR.c && ./QR > OUTFILE"
  ]
  Apt = ["ghc", "icont", "intercal"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        import Data.Char\n
        main=
          putStrLn$"procedure main();write(\\"DO,1<-#"++show(length s)++f s 1 0;
        f(x:t)i c=
          let v=foldl(\\a x->a*2+(mod x 2))0$take 8$iterate(flip div 2)$Data.Char.ord x in
          (if mod i 4<1then"PLEASE"else"")++
          "DO,1SUB#"++show i++"<-#"++show(mod(c-v)256)++"\\\\n"++
          f t(i+1)v;
        f[]_ _=
          "PLEASEREADOUT,1\\\\nPLEASEGIVEUP\\");end";
        s=#{E[PREV+N]}
      )
    END
  end
end

class Go_Groovy < CodeGen
  File = ["QR.go", "QR.groovy"]
  Cmd = ["go run QR.go > OUTFILE", "groovy QR.groovy > OUTFILE"]
  Apt = ["golang", "groovy"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        package main;
        import("fmt";"strings");
        func main(){
          fmt.Print(
            "print\\x27"+
            strings.Replace("#{e[D[e[PREV]]]}\\x27\\n","@","\\\\",-1)
          )
        }
      )
    END
  end
end

class Forth_FORTRAN77_Fortran90 < CodeGen
  File = ["QR.fs", "QR.f", "QR.f90"]
  Cmd = [
    "gforth QR.fs > OUTFILE",
    "mv QR.c QR.c.bak && f2c QR.f && tcc -o QR QR.c -L/usr/lib -lf2c && mv QR.c.bak QR.c && ./QR > OUTFILE",
    "gfortran -o QR QR.f90 && ./QR > OUTFILE"
  ]
  Apt = ["gforth", "f2c", "gfortran"]
  def code
    # assuming that PREV has no '
    <<-'END'.lines.map {|l| l.strip }.join(" ")
      %(
        : A ."         " ;
        : B A ." WRITE(*,*)'" A ;
        : C B TYPE ." '" CR ;
        : D
          S" program QR" C
          S\\" print \\"(&" C
          S\\" #{e[PREV]}" DUP FOR S" &A,&" C NE\x58T
          S\\" &A)\\",&" C
          0 DO B ." &char(" COUNT . ." ),&'" CR LOOP
          S\\" &\\"\\"" C
          S" end program QR" C
          A ." STOP" CR
          A ." END" CR
          BYE ;
        D
      )
    END
  end
end

class CommonLisp < CodeGen
  File = "QR.lisp"
  Cmd = "clisp QR.lisp > OUTFILE"
  Apt = "clisp"
  # assuming that PREV is just one line
  Code = %q(%((write-line"#{Q[PREV,/([\\\\"])/]}")))
end

class CoffeeScript < CodeGen
  File = "QR.coffee"
  Cmd = "coffee QR.coffee > OUTFILE"
  Apt = "coffeescript"
  Code = %q("console.log"+E[PREV])
end

class Clojure_Cobol < CodeGen
  File = ["QR.clj", "QR.cob"]
  Cmd = ["clojure QR.clj > OUTFILE", "cobc -x QR.cob && ./QR > OUTFILE"]
  Apt = ["clojure1.4", "open-cobol"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        (defn f[l r]
          (if
            (>(count r)45)
            (lazy-seq
              (cons(str"    \\""r"\\"&")(f l"")))
            (let[c(first l)]
              (if c
                (f(next l)(if(= c \\")(str r c c)(str r c)))
                [(str"    \\""r"\\".")]))))
        (doall
          (map #(println(str"        "%1))
            (lazy-cat
              ["IDENTIFICATION DIVISION."
               "PROGRAM-ID. QR."
               "PROCEDURE DIVISION."]#{
                (PREV).gsub(/.+/){%((cons"DISPLAY"(f"#{e[$&]}""")))}
              }["STOP RUN."]))))
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
            System.Console.Write(#{E[D[PREV,?~]]}.Replace("~","\\\\"));
          }
        }
      )
    END
  end
end

class Cplusplus < CodeGen
  Name = "C++"
  File = "QR.cpp"
  Cmd = "g++ -o QR QR.cpp && ./QR > OUTFILE"
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
  Cmd = "gcc -o QR QR.c && ./QR > OUTFILE"
  Apt = "gcc"
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      "
        #include<stdio.h>\n
        int main(){
          puts#{E[PREV]};
          return 0;
        }
      "
    END
  end
end

class Boo_Brainfuck < CodeGen
  File = ["QR.boo", "QR.bf"]
  Cmd = ["booi QR.boo > OUTFILE", "beef QR.bf > OUTFILE"]
  Apt = ["boo", "beef"]
  def code
    <<-'END'.lines.map {|l| l.strip }.join
      %(
        for b in System.Text.ASCIIEncoding().GetBytes(#{Q[E[PREV]]}):
          print join(['+'for i in range(0,b)],"")+".>"
      )
    END
  end
end

class Awk < CodeGen
  File = "QR.awk"
  Cmd = "awk -f QR.awk > OUTFILE"
  Apt = "gawk"
  Code = %q(%(BEGIN{s=#{E[D[PREV,?!]]};gsub(/!/,"\\\\\\\\",s);print s}))
end

class ALGOL68 < CodeGen
  File = "QR.a68"
  Cmd = "a68g QR.a68 > OUTFILE"
  Apt = "algol68g"
  Code = %q(%(BEGIN print("#{d[PREV]}")END))
end

class Ada < CodeGen
  File = "qr.adb"
  Cmd = "gnatmake qr.adb && ./qr > OUTFILE"
  Apt = "gnat"
  def code
    <<-'END'.lines.map {|l| l.strip }.join.gsub("|", " ")
      %(
        with Ada.Text_Io;
        procedure qr is|
        begin|
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
    "ruby whitespace.rb QR.ws > OUTFILE"
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
  Cmd = ["tclsh QR.tcl > OUTFILE", "ruby unlambda.rb QR.unl > OUTFILE"]
  Apt = ["tcl8.5", nil]
  Code = %q(%(puts [regsub -all {.} "#{Q[e[PREV.reverse],/[\[\]$]/]}" \\\\x60.&]k))
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

class Scheme < CodeGen
  File = "QR.scm"
  Cmd = "gosh QR.scm > OUTFILE"
  Apt = "gauche"
  Code = %q(%((display "#{e[PREV]}")))
end

class Scala < CodeGen
  File = "QR.scala"
  Cmd = "scalac QR.scala && scala QR > OUTFILE"
  Apt = "scala"
  Code = %q("object QR extends App{println#{E[PREV]}}")
end

class Ruby < CodeGen
  File = "QR.rb"
  Cmd = "ruby QR.rb > OUTFILE"
  Apt = "ruby1.9.3"
end
