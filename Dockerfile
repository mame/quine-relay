FROM ubuntu:20.10
ENV DEBIAN_FRONTEND noninteractive
RUN rm /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectj && chronic apt-get clean
RUN chronic apt-get -qq install -y asymptote ats2-lang bash bc && chronic apt-get clean
RUN chronic apt-get -qq install -y bf bison bsdgames bsh && chronic apt-get clean
RUN chronic apt-get -qq install -y clisp clojure cmake coffeescript && chronic apt-get clean
RUN chronic apt-get -qq install -y curl dafny dc dhall && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs-nox erlang f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex fp-compiler fsharp && chronic apt-get clean
RUN chronic apt-get -qq install -y g++ gambas3-scripter gap gawk && chronic apt-get clean
RUN chronic apt-get -qq install -y gcc gdb gdc generator-scripting-language && chronic apt-get clean
RUN chronic apt-get -qq install -y genius gforth gfortran ghc && chronic apt-get clean
RUN chronic apt-get -qq install -y ghostscript gnat gnu-smalltalk gnucobol && chronic apt-get clean
RUN chronic apt-get -qq install -y gnuplot gobjc golang groff && chronic apt-get clean
RUN chronic apt-get -qq install -y groovy guile-2.0 gzip haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq julia ksh && chronic apt-get clean
RUN chronic apt-get -qq install -y libgd-dev libpng-dev libpolyml-dev lisaac && chronic apt-get clean
RUN chronic apt-get -qq install -y livescript llvm lua5.3 m4 && chronic apt-get clean
RUN chronic apt-get -qq install -y make maxima minizinc mono-devel && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-mcs mono-vbnc nasm neko && chronic apt-get clean
RUN chronic apt-get -qq install -y nickle nim node-typescript nodejs && chronic apt-get clean
RUN chronic apt-get -qq install -y ocaml octave openjdk-11-jdk pakcs && chronic apt-get clean
RUN chronic apt-get -qq install -y pari-gp parser3-cgi perl php-cli && chronic apt-get clean
RUN chronic apt-get -qq install -y pike8.0 polyml python3 r-base && chronic apt-get clean
RUN chronic apt-get -qq install -y rakudo ratfor rc regina-rexx && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby ruby-mustache rustc scala && chronic apt-get clean
RUN chronic apt-get -qq install -y scilab-cli sed slsh spin && chronic apt-get clean
RUN chronic apt-get -qq install -y squirrel3 surgescript swi-prolog tcl && chronic apt-get clean
RUN chronic apt-get -qq install -y tcsh valac vim xsltproc && chronic apt-get clean
RUN chronic apt-get -qq install -y yabasic yorick zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
