FROM ubuntu:18.04
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
RUN chronic apt-get -qq install -y fsharp g++ gap gawk && chronic apt-get clean
RUN chronic apt-get -qq install -y gcc gdb gdc generator-scripting-language && chronic apt-get clean
RUN chronic apt-get -qq install -y genius gforth gfortran ghc && chronic apt-get clean
RUN chronic apt-get -qq install -y ghostscript gnat gnu-smalltalk gnuplot && chronic apt-get clean
RUN chronic apt-get -qq install -y gobjc golang gpt gri && chronic apt-get clean
RUN chronic apt-get -qq install -y groff groovy guile-2.0 gzip && chronic apt-get clean
RUN chronic apt-get -qq install -y haxe icont iconx intercal && chronic apt-get clean
RUN chronic apt-get -qq install -y iverilog jasmin-sable jq ksh && chronic apt-get clean
RUN chronic apt-get -qq install -y libgd-dev libpng-dev lisaac livescript && chronic apt-get clean
RUN chronic apt-get -qq install -y llvm lua5.3 m4 make && chronic apt-get clean
RUN chronic apt-get -qq install -y maxima minizinc mlton mono-devel && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-mcs mono-vbnc nasm neko && chronic apt-get clean
RUN chronic apt-get -qq install -y nickle nim node-typescript nodejs && chronic apt-get clean
RUN chronic apt-get -qq install -y ocaml octave open-cobol openjdk-8-jdk && chronic apt-get clean
RUN chronic apt-get -qq install -y pakcs pari-gp parser3-cgi perl && chronic apt-get clean
RUN chronic apt-get -qq install -y php-cli pike8.0 python r-base && chronic apt-get clean
RUN chronic apt-get -qq install -y rakudo ratfor rc regina-rexx && chronic apt-get clean
RUN chronic apt-get -qq install -y ruby ruby-mustache rustc scala && chronic apt-get clean
RUN chronic apt-get -qq install -y scilab sed slsh spin && chronic apt-get clean
RUN chronic apt-get -qq install -y squirrel3 swi-prolog tcl tcsh && chronic apt-get clean
RUN chronic apt-get -qq install -y valac vim xsltproc yabasic && chronic apt-get clean
RUN chronic apt-get -qq install -y yorick zoem zsh && chronic apt-get clean
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
