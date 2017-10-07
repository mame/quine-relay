FROM ubuntu:17.04
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ && chronic apt-get clean
RUN chronic apt-get -qq install -y aspectj asymptote ats2-lang bash && chronic apt-get clean
RUN chronic apt-get -qq install -y bc bf bison bsdgames && chronic apt-get clean
RUN chronic apt-get -qq install -y clisp clojure cmake coffeescript && chronic apt-get clean
RUN chronic apt-get -qq install -y dafny dc ecere-dev elixir && chronic apt-get clean
RUN chronic apt-get -qq install -y emacs24 erlang f2c fish && chronic apt-get clean
RUN chronic apt-get -qq install -y flex fp-compiler fsharp g++ && chronic apt-get clean
RUN chronic apt-get -qq install -y gambas3-script gap gawk gcc && chronic apt-get clean
RUN chronic apt-get -qq install -y gdb gdc genius gforth && chronic apt-get clean
RUN chronic apt-get -qq install -y gfortran ghc ghostscript gnat && chronic apt-get clean
RUN chronic apt-get -qq install -y gnu-smalltalk gnuplot gobjc golang && chronic apt-get clean
RUN chronic apt-get -qq install -y gpt gri groff groovy && chronic apt-get clean
RUN chronic apt-get -qq install -y guile-2.0 gzip haxe icont && chronic apt-get clean
RUN chronic apt-get -qq install -y iconx intercal iverilog jasmin-sable && chronic apt-get clean
RUN chronic apt-get -qq install -y jq julia ksh libgd-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y libpng-dev lisaac livescript llvm && chronic apt-get clean
RUN chronic apt-get -qq install -y lua5.3 m4 make maxima && chronic apt-get clean
RUN chronic apt-get -qq install -y minizinc mlton mono-devel mono-mcs && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-vbnc nasm neko nescc && chronic apt-get clean
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
ENV PATH /usr/games:$PATH
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay
RUN make -C vendor
CMD make check -j 10000
