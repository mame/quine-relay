FROM ubuntu:19.04
ENV DEBIAN_FRONTEND noninteractive
RUN rm /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ && chronic apt-get clean
RUN chronic apt-get -qq install -y aspectj asymptote ats2-lang bash && chronic apt-get clean
RUN chronic apt-get -qq install -y bc bf bison bsdgames && chronic apt-get clean
RUN chronic apt-get -qq install -y bsh clisp clojure cmake && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript curl dafny dc && chronic apt-get clean
RUN chronic apt-get -qq install -y ecere-dev elixir emacs25 erlang && chronic apt-get clean
RUN chronic apt-get -qq install -y f2c fish flex fp-compiler && chronic apt-get clean
RUN chronic apt-get -qq install -y fsharp g++ gambas3-script gap && chronic apt-get clean
RUN chronic apt-get -qq install -y gawk gcc gdb gdc && chronic apt-get clean
RUN chronic apt-get -qq install -y generator-scripting-language genius gforth gfortran && chronic apt-get clean
RUN chronic apt-get -qq install -y ghc ghostscript gnat gnu-smalltalk && chronic apt-get clean
RUN chronic apt-get -qq install -y gnucobol gnuplot gobjc golang && chronic apt-get clean
RUN chronic apt-get -qq install -y gri groff groovy guile-2.0 && chronic apt-get clean
RUN chronic apt-get -qq install -y gzip haxe icont iconx && chronic apt-get clean
RUN chronic apt-get -qq install -y intercal iverilog jasmin-sable jq && chronic apt-get clean
RUN chronic apt-get -qq install -y julia ksh libgd-dev libpng-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y lisaac livescript llvm lua5.3 && chronic apt-get clean
RUN chronic apt-get -qq install -y m4 make maxima minizinc && chronic apt-get clean
RUN chronic apt-get -qq install -y mlton mono-devel mono-mcs mono-vbnc && chronic apt-get clean
RUN chronic apt-get -qq install -y nasm neko nickle nim && chronic apt-get clean
RUN chronic apt-get -qq install -y node-typescript nodejs ocaml octave && chronic apt-get clean
RUN chronic apt-get -qq install -y openjdk-13-jdk pakcs pari-gp parser3-cgi && chronic apt-get clean
RUN chronic apt-get -qq install -y perl php-cli pike8.0 python && chronic apt-get clean
RUN chronic apt-get -qq install -y r-base rakudo ratfor rc && chronic apt-get clean
RUN chronic apt-get -qq install -y regina-rexx ruby ruby-mustache rustc && chronic apt-get clean
RUN chronic apt-get -qq install -y scala sed slsh spin && chronic apt-get clean
RUN chronic apt-get -qq install -y squirrel3 swi-prolog tcl tcsh && chronic apt-get clean
RUN chronic apt-get -qq install -y valac vim xsltproc yabasic && chronic apt-get clean
RUN chronic apt-get -qq install -y yorick zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
