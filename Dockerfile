FROM ubuntu:22.04
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
RUN chronic apt-get -qq install -y g++ gambas3-gb-pcre gambas3-scripter gap && chronic apt-get clean
RUN chronic apt-get -qq install -y gawk gcc gdb gdc && chronic apt-get clean
RUN chronic apt-get -qq install -y generator-scripting-language genius gforth gfortran && chronic apt-get clean
RUN chronic apt-get -qq install -y ghc ghostscript gnat gnu-smalltalk && chronic apt-get clean
RUN chronic apt-get -qq install -y gnucobol4 gnuplot gobjc golang && chronic apt-get clean
RUN chronic apt-get -qq install -y gpt groff groovy guile-3.0 && chronic apt-get clean
RUN chronic apt-get -qq install -y gzip haxe icont iconx && chronic apt-get clean
RUN chronic apt-get -qq install -y intercal iverilog jasmin-sable jq && chronic apt-get clean
RUN chronic apt-get -qq install -y kotlin ksh libgd-dev libpng-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpolyml-dev lisaac livescript llvm && chronic apt-get clean
RUN chronic apt-get -qq install -y lua5.3 m4 make maxima && chronic apt-get clean
RUN chronic apt-get -qq install -y minizinc mono-devel mono-mcs mono-vbnc && chronic apt-get clean
RUN chronic apt-get -qq install -y nasm neko nickle node-typescript && chronic apt-get clean
RUN chronic apt-get -qq install -y nodejs ocaml octave openjdk-11-jdk && chronic apt-get clean
RUN chronic apt-get -qq install -y pari-gp parser3-cgi perl php-cli && chronic apt-get clean
RUN chronic apt-get -qq install -y polyml python3 r-base rakudo && chronic apt-get clean
RUN chronic apt-get -qq install -y ratfor rc regina-rexx ruby && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby-mustache rustc scala scilab-cli && chronic apt-get clean
RUN chronic apt-get -qq install -y sed slsh spin squirrel3 && chronic apt-get clean
RUN chronic apt-get -qq install -y surgescript swi-prolog tcl tcsh && chronic apt-get clean
RUN chronic apt-get -qq install -y valac vim wabt xsltproc && chronic apt-get clean
RUN chronic apt-get -qq install -y yabasic yorick zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
