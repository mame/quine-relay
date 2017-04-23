FROM ubuntu:17.04
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ aspectj asymptote ats2-lang bash bc bf bison bsdgames clisp clojure cmake coffeescript dafny dc ecere-dev elixir emacs24 erlang f2c fish flex fp-compiler fsharp g++ gambas3-script gap gawk gcc gdb gdc genius gforth gfortran ghc ghostscript gnat gnu-smalltalk gnuplot gobjc golang gpt gri groff groovy guile-2.0 gzip haxe icont iconx intercal iverilog jasmin-sable jq julia ksh libgd-dev libpng-dev lisaac livescript llvm lua5.3 m4 make maxima minizinc mlton mono-devel mono-mcs mono-vbnc nasm neko nescc nickle nim node-typescript nodejs ocaml octave open-cobol openjdk-8-jdk pakcs pari-gp parser3-cgi perl php-cli pike8.0 python r-base rakudo ratfor rc regina-rexx ruby ruby-mustache rustc scala scilab sed slsh spin squirrel3 swi-prolog tcl tcsh valac vim xsltproc yabasic yorick zoem zsh && apt-get clean
ENV PATH /usr/games:$PATH
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
