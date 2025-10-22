FROM ubuntu:25.10
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectj && chronic apt-get clean
RUN chronic apt-get -qq install -y asymptote ats2-lang bash bc && chronic apt-get clean
RUN chronic apt-get -qq install -y bison bsh clisp clojure && chronic apt-get clean
RUN chronic apt-get -qq install -y cmake coffeescript crystal curl && chronic apt-get clean
RUN chronic apt-get -qq install -y dc dhall dotnet8 elixir && chronic apt-get clean
RUN chronic apt-get -qq install -y emacs-nox erlang execline f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex fp-compiler g++ && chronic apt-get clean
RUN chronic apt-get -qq install -y gambas3-gb-pcre gambas3-scripter gap gawk && chronic apt-get clean
RUN chronic apt-get -qq install -y gcc gdb gdc genius && chronic apt-get clean
RUN chronic apt-get -qq install -y gforth gfortran ghc ghostscript && chronic apt-get clean
RUN chronic apt-get -qq install -y gm2 gnat gnucobol4 gnuplot && chronic apt-get clean
RUN chronic apt-get -qq install -y gobjc golang gpt groff && chronic apt-get clean
RUN chronic apt-get -qq install -y groovy guile-3.0 gzip haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq kotlin ksh && chronic apt-get clean
RUN chronic apt-get -qq install -y libevent-dev libgd-dev libpng-dev libpolyml-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y livescript llvm lua5.3 m4 && chronic apt-get clean
RUN chronic apt-get -qq install -y make minizinc mono-devel nasm && chronic apt-get clean
RUN chronic apt-get -qq install -y neko nickle nim node-typescript && chronic apt-get clean
RUN chronic apt-get -qq install -y nodejs ocaml octave openjdk-11-jdk && chronic apt-get clean
RUN chronic apt-get -qq install -y pari-gp parser3-cgi perl php-cli && chronic apt-get clean
RUN chronic apt-get -qq install -y pike8.0 polyml python3 r-base && chronic apt-get clean
RUN chronic apt-get -qq install -y rakudo ratfor rc regina-rexx && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby ruby-mustache rustc scala && chronic apt-get clean
RUN chronic apt-get -qq install -y scilab-cli sed slsh spin && chronic apt-get clean
RUN chronic apt-get -qq install -y squirrel3 surgescript swi-prolog swiftlang && chronic apt-get clean
RUN chronic apt-get -qq install -y tcl tcsh valac vim && chronic apt-get clean
RUN chronic apt-get -qq install -y wabt xsltproc yabasic yorick && chronic apt-get clean
RUN chronic apt-get -qq install -y zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
