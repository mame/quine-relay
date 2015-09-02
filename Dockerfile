FROM ubuntu:15.04
ENV PATH /usr/games:$PATH
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y afnix algol68g aplus-fsf asymptote ats-lang-anairiats bash bc bf boo bsdgames cduce clisp clojure1.6 cmake coffeescript dc ecere-sdk emacs24 erlang f2c falconpl fp-compiler fsharp g++ gambas3-script gap gauche gawk gcc gdc genius gforth gfortran ghc ghostscript gnat gnu-smalltalk gnuplot gobjc golang gpt gri groff groovy haxe icont iconx intercal iverilog jasmin-sable julia kaya libgd2-xpm-dev libpng12-dev lisaac llvm lua5.2 make maxima mlton mono-devel mono-mcs mono-vbnc nasm neko nickle ocaml octave open-cobol openjdk-6-jdk pari-gp parrot perl php5-cli pike7.8 python r-base ratfor regina-rexx rhino ruby2.1 scala scilab slsh spl-core swi-prolog tcc tcl ucblogo valac xsltproc yorick zoem
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make CC=tcc check
