FROM ubuntu:17.04
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf asymptote ats2-lang bash bc bf bison bsdgames clisp clojure coffeescript dafny dc ecere-dev elixir emacs24 erlang f2c fp-compiler fsharp g++ gambas3-script gap gawk gcc gdc genius gforth gfortran ghc ghostscript gnat gnu-smalltalk gnuplot gobjc golang gpt groff groovy guile-2.0 haxe icont iconx intercal iverilog jasmin-sable jq julia libgd-dev libpng-dev lisaac llvm lua5.3 make maxima minizinc mlton mono-devel mono-mcs mono-vbnc nasm neko nickle nim nodejs ocaml octave open-cobol openjdk-8-jdk pari-gp perl php-cli pike8.0 python r-base ratfor regina-rexx ruby rustc scala scilab slsh squirrel3 swi-prolog tcl valac xsltproc yorick zoem && apt-get clean
ENV PATH /usr/games:$PATH
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
