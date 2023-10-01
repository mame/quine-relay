FROM ubuntu:23.04
ENV DEBIAN_FRONTEND noninteractive
RUN rm /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectj && chronic apt-get clean
RUN chronic apt-get -qq install -y asymptote ats2-lang bash bc && chronic apt-get clean
RUN chronic apt-get -qq install -y bison bsdgames bsh clisp && chronic apt-get clean
RUN chronic apt-get -qq install -y clojure cmake coffeescript crystal && chronic apt-get clean
RUN chronic apt-get -qq install -y curl dafny dc dhall && chronic apt-get clean
RUN chronic apt-get -qq install -y dotnet7 elixir emacs-nox erlang && chronic apt-get clean
RUN chronic apt-get -qq install -y f2c fish flex fp-compiler && chronic apt-get clean
RUN chronic apt-get -qq install -y g++ gambas3-gb-pcre gambas3-scripter gap && chronic apt-get clean
RUN chronic apt-get -qq install -y gawk gcc gdb gdc && chronic apt-get clean
RUN chronic apt-get -qq install -y genius gforth gfortran ghc && chronic apt-get clean
RUN chronic apt-get -qq install -y ghostscript gnat gnu-smalltalk gnucobol4 && chronic apt-get clean
RUN chronic apt-get -qq install -y gnuplot gobjc golang gpt && chronic apt-get clean
RUN chronic apt-get -qq install -y groff groovy guile-3.0 gzip && chronic apt-get clean
RUN chronic apt-get -qq install -y haxe icont iconx intercal && chronic apt-get clean
RUN chronic apt-get -qq install -y iverilog jasmin-sable jq kotlin && chronic apt-get clean
RUN chronic apt-get -qq install -y ksh libevent-dev libgd-dev libpng-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpolyml-dev lisaac livescript llvm && chronic apt-get clean
RUN chronic apt-get -qq install -y lua5.3 m4 make maxima && chronic apt-get clean
RUN chronic apt-get -qq install -y minizinc mono-devel nasm neko && chronic apt-get clean
RUN chronic apt-get -qq install -y nickle nim node-typescript nodejs && chronic apt-get clean
RUN chronic apt-get -qq install -y ocaml octave openjdk-11-jdk pari-gp && chronic apt-get clean
RUN chronic apt-get -qq install -y parser3-cgi perl php-cli polyml && chronic apt-get clean
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
