FROM ubuntu:18.04
RUN apt-get update && apt-get upgrade -y
RUN apt-get -qq install -y apt-utils > /dev/null
RUN apt-get -qq install -y moreutils
RUN chronic apt-get -qq install -y afnix algol68g aplus-fsf aspectc++ && chronic apt-get clean
RUN chronic apt-get -qq install -y aspectj asymptote ats2-lang bash && chronic apt-get clean
RUN chronic apt-get -qq install -y bc bf bison bsdgames && chronic apt-get clean
RUN chronic apt-get -qq install -y bsh clisp clojure cmake && chronic apt-get clean
RUN chronic apt-get -qq install -y coffeescript dafny dc ecere-dev && chronic apt-get clean
RUN chronic apt-get -qq install -y elixir emacs25 erlang f2c && chronic apt-get clean
RUN chronic apt-get -qq install -y fish flex flex fp-compiler && chronic apt-get clean
RUN chronic apt-get -qq install -y fsharp g++ gap gawk && chronic apt-get clean
RUN chronic apt-get -qq install -y gcc gdb gdc genius && chronic apt-get clean
RUN chronic apt-get -qq install -y gforth gfortran ghc ghostscript && chronic apt-get clean
RUN chronic apt-get -qq install -y gnat gnu-smalltalk gnuplot gobjc && chronic apt-get clean
RUN chronic apt-get -qq install -y golang gpt gri groff && chronic apt-get clean
RUN chronic apt-get -qq install -y groovy guile-2.0 gzip haxe && chronic apt-get clean
RUN chronic apt-get -qq install -y icont iconx intercal iverilog && chronic apt-get clean
RUN chronic apt-get -qq install -y jasmin-sable jq julia ksh && chronic apt-get clean
RUN chronic apt-get -qq install -y libgd-dev libpng-dev lisaac livescript && chronic apt-get clean
RUN chronic apt-get -qq install -y llvm lua5.3 m4 make && chronic apt-get clean
RUN chronic apt-get -qq install -y maxima minizinc mlton mono-devel && chronic apt-get clean
RUN chronic apt-get -qq install -y mono-mcs mono-vbnc nasm neko && chronic apt-get clean
RUN chronic apt-get -qq install -y nescc nickle nim node-typescript && chronic apt-get clean
RUN chronic apt-get -qq install -y nodejs ocaml octave open-cobol && chronic apt-get clean
RUN chronic apt-get -qq install -y openjdk-8-jdk pari-gp parser3-cgi perl && chronic apt-get clean
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
