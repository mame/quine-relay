FROM ubuntu:24.04
ENV DEBIAN_FRONTEND noninteractive
RUN rm /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectj && chronic apt-get clean
RUN chronic apt-get -qq install -y asymptote ats2-lang bash bc && chronic apt-get clean
RUN chronic apt-get -qq install -y bison bsh clisp clojure && chronic apt-get clean
RUN chronic apt-get -qq install -y cmake coffeescript crystal curl && chronic apt-get clean
RUN chronic apt-get -qq install -y dafny dc dhall dotnet8 && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs-nox erlang f2c && chronic apt-get clean
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
RUN chronic apt-get -qq install -y m4 make maxima minizinc && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-devel nasm neko nickle && chronic apt-get clean
RUN chronic apt-get -qq install -y nim node-typescript nodejs ocaml && chronic apt-get clean
RUN chronic apt-get -qq install -y octave openjdk-11-jdk pari-gp parser3-cgi && chronic apt-get clean
RUN chronic apt-get -qq install -y perl php-cli pike8.0 polyml && chronic apt-get clean
RUN chronic apt-get -qq install -y python3 r-base rakudo ratfor && chronic apt-get clean
RUN chronic apt-get -qq install -y rc regina-rexx ruby ruby-mustache && chronic apt-get clean
RUN chronic apt-get -qq install -y rustc scala scilab-cli sed && chronic apt-get clean
RUN chronic apt-get -qq install -y slsh spin surgescript swi-prolog && chronic apt-get clean
RUN chronic apt-get -qq install -y tcl tcsh valac vim && chronic apt-get clean
RUN chronic apt-get -qq install -y wabt xsltproc yabasic yorick && chronic apt-get clean
RUN chronic apt-get -qq install -y zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
