FROM ubuntu:18.04
ENV DEBIAN_FRONTEND noninteractive
RUN rm /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ && chronic apt-get clean
RUN chronic apt-get -qq install -y aspectj asymptote ats2-lang bash && chronic apt-get clean
RUN chronic apt-get -qq install -y bc bf bison bsdgames && chronic apt-get clean
RUN chronic apt-get -qq install -y bsh clisp clojure cmake && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript dafny dc ecere-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs25 erlang f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex fp-compiler fsharp && chronic apt-get clean
RUN chronic apt-get -qq install -y g++ gap gawk gcc && chronic apt-get clean
RUN chronic apt-get -qq install -y gdb gdc generator-scripting-language genius && chronic apt-get clean
RUN chronic apt-get -qq install -y gforth gfortran ghc ghostscript && chronic apt-get clean
RUN chronic apt-get -qq install -y gnat gnu-smalltalk gnuplot gobjc && chronic apt-get clean
RUN chronic apt-get -qq install -y golang gpt gri groff && chronic apt-get clean
RUN chronic apt-get -qq install -y groovy guile-2.0 gzip haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq ksh libgd-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpng-dev lisaac livescript llvm && chronic apt-get clean
RUN chronic apt-get -qq install -y lua5.3 m4 make maxima && chronic apt-get clean
RUN chronic apt-get -qq install -y minizinc mlton mono-devel mono-mcs && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-vbnc nasm neko nickle && chronic apt-get clean
RUN chronic apt-get -qq install -y nim node-typescript nodejs ocaml && chronic apt-get clean
RUN chronic apt-get -qq install -y octave open-cobol openjdk-8-jdk pakcs && chronic apt-get clean
RUN chronic apt-get -qq install -y pari-gp parser3-cgi perl php-cli && chronic apt-get clean
RUN chronic apt-get -qq install -y pike8.0 python r-base rakudo && chronic apt-get clean
RUN chronic apt-get -qq install -y ratfor rc regina-rexx ruby && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby-mustache rustc scala scilab && chronic apt-get clean
RUN chronic apt-get -qq install -y sed slsh spin squirrel3 && chronic apt-get clean
RUN chronic apt-get -qq install -y swi-prolog tcl tcsh valac && chronic apt-get clean
RUN chronic apt-get -qq install -y vim xsltproc yabasic yorick && chronic apt-get clean
RUN chronic apt-get -qq install -y zoem zsh && chronic apt-get clean
ENV PATH /usr/games:$PATH
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
