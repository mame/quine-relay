FROM ubuntu:26.04
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aspectj asymptote && chronic apt-get clean
RUN chronic apt-get -qq install -y ats2-lang bash bc bison && chronic apt-get clean
RUN chronic apt-get -qq install -y bsh clisp clojure cmake && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript crystal curl dc && chronic apt-get clean
RUN chronic apt-get -qq install -y dhall dotnet-sdk-10.0 elixir emacs-nox && chronic apt-get clean
RUN chronic apt-get -qq install -y erlang execline f2c fennel && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex fp-compiler g++ && chronic apt-get clean
RUN chronic apt-get -qq install -y gambas3-gb-pcre gambas3-scripter gap gawk && chronic apt-get clean
RUN chronic apt-get -qq install -y gcc gdb gdc genius && chronic apt-get clean
RUN chronic apt-get -qq install -y gforth gfortran ghc ghostscript && chronic apt-get clean
RUN chronic apt-get -qq install -y gm2 gnat gnucobol4 gnuplot && chronic apt-get clean
RUN chronic apt-get -qq install -y gobjc golang gpt groff && chronic apt-get clean
RUN chronic apt-get -qq install -y groovy guile-3.0 gzip haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq kotlin ksh && chronic apt-get clean
RUN chronic apt-get -qq install -y libevent-dev libfl-dev libgd-dev libpng-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpolyml-dev livescript llvm lua5.3 && chronic apt-get clean
RUN chronic apt-get -qq install -y m4 make minizinc mono-devel && chronic apt-get clean
RUN chronic apt-get -qq install -y nasm neko nickle nim && chronic apt-get clean
RUN chronic apt-get -qq install -y node-typescript nodejs ocaml octave && chronic apt-get clean
RUN chronic apt-get -qq install -y openjdk-25-jdk pari-gp parser3-cgi perl && chronic apt-get clean
RUN chronic apt-get -qq install -y php-cli pike8.0 polyml python3 && chronic apt-get clean
RUN chronic apt-get -qq install -y r-base rakudo ratfor rc && chronic apt-get clean
RUN chronic apt-get -qq install -y regina-rexx ruby ruby-mustache rustc && chronic apt-get clean
RUN chronic apt-get -qq install -y scala scilab-cli sed slsh && chronic apt-get clean
RUN chronic apt-get -qq install -y spin squirrel3 surgescript swi-prolog && chronic apt-get clean
RUN chronic apt-get -qq install -y swiftlang tcl tcsh valac && chronic apt-get clean
RUN chronic apt-get -qq install -y vim wabt xsltproc yabasic && chronic apt-get clean
RUN chronic apt-get -qq install -y yorick zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
