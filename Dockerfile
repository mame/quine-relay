FROM ubuntu:17.10
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ && chronic apt-get clean
RUN chronic apt-get -qq install -y aspectj asymptote ats2-lang bash && chronic apt-get clean
RUN chronic apt-get -qq install -y bc bf bison bsdgames && chronic apt-get clean
RUN chronic apt-get -qq install -y bsh clisp clojure cmake && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript dafny dc ecere-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs25 erlang f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex flex fp-compiler && chronic apt-get clean
RUN chronic apt-get -qq install -y fsharp g++ gambas3-script gap && chronic apt-get clean
RUN chronic apt-get -qq install -y gawk gcc gdb gdc && chronic apt-get clean
RUN chronic apt-get -qq install -y genius gforth gfortran ghc && chronic apt-get clean
RUN chronic apt-get -qq install -y ghostscript gnat gnu-smalltalk gnuplot && chronic apt-get clean
RUN chronic apt-get -qq install -y gobjc golang gpt gri && chronic apt-get clean
RUN chronic apt-get -qq install -y groff groovy guile-2.0 gzip && chronic apt-get clean
RUN chronic apt-get -qq install -y haxe icont iconx intercal && chronic apt-get clean
RUN chronic apt-get -qq install -y iverilog jasmin-sable jq julia && chronic apt-get clean
RUN chronic apt-get -qq install -y ksh libgd-dev libpng-dev lisaac && chronic apt-get clean
RUN chronic apt-get -qq install -y livescript llvm lua5.3 m4 && chronic apt-get clean
RUN chronic apt-get -qq install -y make maxima minizinc mlton && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-devel mono-mcs mono-vbnc nasm && chronic apt-get clean
RUN chronic apt-get -qq install -y neko nescc nickle nim && chronic apt-get clean
RUN chronic apt-get -qq install -y node-typescript nodejs ocaml octave && chronic apt-get clean
RUN chronic apt-get -qq install -y open-cobol openjdk-8-jdk pari-gp parser3-cgi && chronic apt-get clean
RUN chronic apt-get -qq install -y perl php-cli pike8.0 python && chronic apt-get clean
RUN chronic apt-get -qq install -y r-base rakudo ratfor rc && chronic apt-get clean
RUN chronic apt-get -qq install -y regina-rexx ruby ruby-mustache rustc && chronic apt-get clean
RUN chronic apt-get -qq install -y scala scilab sed slsh && chronic apt-get clean
RUN chronic apt-get -qq install -y spin squirrel3 swi-prolog tcl && chronic apt-get clean
RUN chronic apt-get -qq install -y tcsh valac vim xsltproc && chronic apt-get clean
RUN chronic apt-get -qq install -y yabasic yorick zoem zsh && chronic apt-get clean
ENV PATH /usr/games:$PATH
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
