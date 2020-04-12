FROM ubuntu:20.04
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
RUN chronic apt-get -qq install -y ecere-dev elixir emacs-nox erlang && chronic apt-get clean
RUN chronic apt-get -qq install -y f2c fish flex fp-compiler && chronic apt-get clean
RUN chronic apt-get -qq install -y fsharp g++ gambas3-script gap && chronic apt-get clean
RUN chronic apt-get -qq install -y gawk gcc gdb gdc && chronic apt-get clean
RUN chronic apt-get -qq install -y generator-scripting-language genius gforth gfortran && chronic apt-get clean
RUN chronic apt-get -qq install -y ghc ghostscript gnat gnu-smalltalk && chronic apt-get clean
RUN chronic apt-get -qq install -y gnucobol gnuplot gobjc golang && chronic apt-get clean
RUN chronic apt-get -qq install -y groff groovy guile-2.0 gzip && chronic apt-get clean
RUN chronic apt-get -qq install -y haxe icont iconx intercal && chronic apt-get clean
RUN chronic apt-get -qq install -y iverilog jasmin-sable jq julia && chronic apt-get clean
RUN chronic apt-get -qq install -y ksh libgd-dev libpng-dev lisaac && chronic apt-get clean
RUN chronic apt-get -qq install -y livescript llvm lua5.3 m4 && chronic apt-get clean
RUN chronic apt-get -qq install -y make maxima minizinc mlton && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-devel mono-mcs mono-vbnc nasm && chronic apt-get clean
RUN chronic apt-get -qq install -y neko nickle nim node-typescript && chronic apt-get clean
RUN chronic apt-get -qq install -y nodejs ocaml octave openjdk-13-jdk && chronic apt-get clean
RUN chronic apt-get -qq install -y pakcs pari-gp parser3-cgi perl && chronic apt-get clean
RUN chronic apt-get -qq install -y php-cli pike8.0 python3 r-base && chronic apt-get clean
RUN chronic apt-get -qq install -y rakudo ratfor rc regina-rexx && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby ruby-mustache rustc scala && chronic apt-get clean
RUN chronic apt-get -qq install -y scilab-cli sed slsh spin && chronic apt-get clean
RUN chronic apt-get -qq install -y squirrel3 swi-prolog tcl tcsh && chronic apt-get clean
RUN chronic apt-get -qq install -y valac vim xsltproc yabasic && chronic apt-get clean
RUN chronic apt-get -qq install -y yorick zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
