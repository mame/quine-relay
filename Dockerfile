FROM ubuntu:25.04
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
RUN chronic apt-get -qq install -y lisaac livescript llvm lua5.3 && chronic apt-get clean
RUN chronic apt-get -qq install -y m4 make minizinc mono-devel && chronic apt-get clean
RUN chronic apt-get -qq install -y nasm neko nickle nim && chronic apt-get clean
RUN chronic apt-get -qq install -y node-typescript nodejs ocaml octave && chronic apt-get clean
RUN chronic apt-get -qq install -y openjdk-11-jdk pari-gp parser3-cgi perl && chronic apt-get clean
RUN chronic apt-get -qq install -y php-cli pike8.0 polyml python3 && chronic apt-get clean
RUN chronic apt-get -qq install -y r-base rakudo ratfor rc && chronic apt-get clean
RUN chronic apt-get -qq install -y regina-rexx ruby ruby-mustache rustc && chronic apt-get clean
RUN chronic apt-get -qq install -y scala scilab-cli sed slsh && chronic apt-get clean
RUN chronic apt-get -qq install -y spin squirrel3 surgescript swi-prolog && chronic apt-get clean
RUN chronic apt-get -qq install -y tcl tcsh valac vim && chronic apt-get clean
RUN chronic apt-get -qq install -y wabt xsltproc yabasic yorick && chronic apt-get clean
RUN chronic apt-get -qq install -y zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
