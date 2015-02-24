require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
other_packages = %w(cmake libpng12-dev libgd2-xpm-dev groff tcc)

apts = [*apts.flatten.compact.uniq, *other_packages].sort

ppas = %w(ecere-team/ppa staticfloat/juliareleases staticfloat/julia-deps)

dockerfile = []
dockerfile << "FROM ubuntu:14.10"
dockerfile << "ENV PATH /usr/games:$PATH"
dockerfile << "RUN apt-get update && apt-get upgrade -y"
dockerfile << "RUN apt-get install -y software-properties-common"

ppa_width = ppas.map {|ppa| ppa.size }.max
dockerfile << "RUN " + ppas.map do |ppa|
  "add-apt-repository -y ppa:#{ppa}#{" " * (ppa_width - ppa.size)}"
end.join(" && \\\n    ")
dockerfile << "RUN apt-get update"

apt_width = apts.map {|apt| apt.size}.max
dockerfile << "RUN " + apts.map do |apt|
  "apt-get install -y #{apt}#{" " * (apt_width - apt.size)}"
end.join(" && \\\n    ")
dockerfile << "ADD . /usr/local/share/quine-relay"
dockerfile << "WORKDIR /usr/local/share/quine-relay"
dockerfile << "RUN make -C vendor"
dockerfile << 'CMD make CC=tcc'

File.write("../Dockerfile", dockerfile.join("\n") + "\n")
