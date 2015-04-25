require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
other_packages = %w(cmake libpng12-dev libgd2-xpm-dev groff tcc)

apts = [*apts.flatten.compact.uniq, *other_packages].sort

dockerfile = []
dockerfile << "FROM ubuntu:15.04"
dockerfile << "ENV PATH /usr/games:$PATH"
dockerfile << "RUN apt-get update && apt-get upgrade -y"

apt_width = apts.map {|apt| apt.size}.max
dockerfile << "RUN " + apts.map do |apt|
  "apt-get install -y #{apt}#{" " * (apt_width - apt.size)}"
end.join(" && \\\n    ")
dockerfile << "ADD . /usr/local/share/quine-relay"
dockerfile << "WORKDIR /usr/local/share/quine-relay"
dockerfile << "RUN make -C vendor"
dockerfile << 'CMD make CC=tcc'

File.write("../Dockerfile", dockerfile.join("\n") + "\n")
