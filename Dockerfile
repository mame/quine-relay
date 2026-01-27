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
RUN chronic apt-get -qq install -y gap gawk gcc gdb && chronic apt-get clean
RUN chronic apt-get -qq install -y gdc genius gforth gfortran && chronic apt-get clean
RUN chronic apt-get -qq install -y ghc ghostscript gm2 gnat && chronic apt-get clean
RUN chronic apt-get -qq install -y gnucobol4 gnuplot gobjc golang && chronic apt-get clean
RUN chronic apt-get -qq install -y gpt groff groovy guile-3.0 && chronic apt-get clean
RUN chronic apt-get -qq install -y gzip haxe icont iconx && chronic apt-get clean
RUN chronic apt-get -qq install -y intercal iverilog jasmin-sable jq && chronic apt-get clean
RUN chronic apt-get -qq install -y kotlin ksh libevent-dev libgd-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpng-dev libpolyml-dev livescript llvm && chronic apt-get clean
RUN chronic apt-get -qq install -y lua5.3 m4 make minizinc && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-devel nasm neko nickle && chronic apt-get clean
RUN chronic apt-get -qq install -y nim nix-bin node-typescript nodejs && chronic apt-get clean
RUN chronic apt-get -qq install -y ocaml octave openjdk-11-jdk pari-gp && chronic apt-get clean
RUN chronic apt-get -qq install -y parser3-cgi perl php-cli pike8.0 && chronic apt-get clean
RUN chronic apt-get -qq install -y polyml python3 r-base rakudo && chronic apt-get clean
RUN chronic apt-get -qq install -y ratfor rc regina-rexx ruby && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby-mustache rustc scala scilab-cli && chronic apt-get clean
RUN chronic apt-get -qq install -y sed slsh spin squirrel3 && chronic apt-get clean
RUN chronic apt-get -qq install -y surgescript swi-prolog swiftlang tcl && chronic apt-get clean
RUN chronic apt-get -qq install -y tcsh valac vim wabt && chronic apt-get clean
RUN chronic apt-get -qq install -y xsltproc yabasic yorick zoem && chronic apt-get clean
RUN chronic apt-get -qq install -y zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
