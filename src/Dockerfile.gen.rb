require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
other_packages = %w(libpng-dev libgd-dev groff flex bison curl)

apts = [*apts.flatten.compact.uniq, *other_packages].uniq.sort

dockerfile = []
dockerfile << "FROM ubuntu:22.04"
dockerfile << "ENV DEBIAN_FRONTEND noninteractive"
dockerfile << "RUN rm /etc/dpkg/dpkg.cfg.d/excludes" # maxima requires /usr/share/doc/maxima/...
dockerfile << "RUN apt-get update && apt-get upgrade -y"
dockerfile << "RUN apt-get -qq install -y apt-utils > /dev/null"
dockerfile << "RUN apt-get -qq install -y moreutils"
apts.each_slice(4) do |apt|
  dockerfile << "RUN chronic apt-get -qq install -y #{ apt.join(" ") } && chronic apt-get clean"
end
dockerfile << "ADD . /usr/local/share/quine-relay"
dockerfile << "WORKDIR /usr/local/share/quine-relay"
dockerfile << "RUN make -C vendor"
dockerfile << "CMD make check -j 10000"

File.write("../Dockerfile", dockerfile.join("\n") + "\n")
