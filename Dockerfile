FROM ubuntu:17.04
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ && chronic apt-get clean
RUN chronic apt-get -qq install -y aspectj asymptote ats2-lang bash && chronic apt-get clean
RUN chronic apt-get -qq install -y bc bf bison bsdgames && chronic apt-get clean
RUN chronic apt-get -qq install -y bsh clisp clojure cmake && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript dafny dc ecere-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs24 erlang f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex fp-compiler fsharp && chronic apt-get clean
RUN chronic apt-get -qq install -y g++ gambas3-script gap gawk && chronic apt-get clean
RUN chronic apt-get -qq install -y gcc gdb gdc genius && chronic apt-get clean
RUN chronic apt-get -qq install -y gforth gfortran ghc ghostscript && chronic apt-get clean
RUN chronic apt-get -qq install -y gnat gnu-smalltalk gnuplot gobjc && chronic apt-get clean
RUN chronic apt-get -qq install -y golang gpt gri groff && chronic apt-get clean
RUN chronic apt-get -qq install -y groovy guile-2.0 gzip haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq julia ksh && chronic apt-get clean
RUN chronic apt-get -qq install -y libgd-dev libpng-dev lisaac livescript && chronic apt-get clean
RUN chronic apt-get -qq install -y llvm lua5.3 m4 make && chronic apt-get clean
RUN chronic apt-get -qq install -y maxima minizinc mlton mono-devel && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-mcs mono-vbnc nasm neko && chronic apt-get clean
RUN chronic apt-get -qq install -y nescc nickle nim node-typescript && chronic apt-get clean
RUN chronic apt-get -qq install -y nodejs ocaml octave open-cobol && chronic apt-get clean
RUN chronic apt-get -qq install -y openjdk-8-jdk pakcs pari-gp parser3-cgi && chronic apt-get clean
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
