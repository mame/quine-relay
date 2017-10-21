FROM ubuntu:17.10
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf asymptote && chronic apt-get clean
RUN chronic apt-get -qq install -y ats2-lang bash bc bf && chronic apt-get clean
RUN chronic apt-get -qq install -y bison bsdgames clisp clojure && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript dafny dc ecere-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs24 erlang f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y flex fp-compiler fsharp g++ && chronic apt-get clean
RUN chronic apt-get -qq install -y gambas3-script gap gawk gcc && chronic apt-get clean
RUN chronic apt-get -qq install -y gdc genius gforth gfortran && chronic apt-get clean
RUN chronic apt-get -qq install -y ghc ghostscript gnat gnu-smalltalk && chronic apt-get clean
RUN chronic apt-get -qq install -y gnuplot gobjc golang gpt && chronic apt-get clean
RUN chronic apt-get -qq install -y groff groovy guile-2.0 haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq julia libgd-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpng-dev lisaac llvm lua5.3 && chronic apt-get clean
RUN chronic apt-get -qq install -y make maxima minizinc mlton && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-devel mono-mcs mono-vbnc nasm && chronic apt-get clean
RUN chronic apt-get -qq install -y neko nickle nim nodejs && chronic apt-get clean
RUN chronic apt-get -qq install -y ocaml octave open-cobol openjdk-8-jdk && chronic apt-get clean
RUN chronic apt-get -qq install -y pari-gp perl php-cli pike8.0 && chronic apt-get clean
RUN chronic apt-get -qq install -y python r-base ratfor regina-rexx && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby rustc scala scilab && chronic apt-get clean
RUN chronic apt-get -qq install -y slsh squirrel3 swi-prolog tcl && chronic apt-get clean
RUN chronic apt-get -qq install -y valac xsltproc yorick zoem && chronic apt-get clean
ENV PATH /usr/games:$PATH
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
