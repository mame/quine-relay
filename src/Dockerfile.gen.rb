require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
other_packages = %w(cmake libpng12-dev libgd-dev groff)

apts = [*apts.flatten.compact.uniq, *other_packages].sort

dockerfile = []
dockerfile << "FROM ubuntu:15.10"
dockerfile << "ENV PATH /usr/games:$PATH"
dockerfile << "ADD . /usr/local/share/quine-relay"
dockerfile << "WORKDIR /usr/local/share/quine-relay"
dockerfile << "RUN apt-get update && apt-get upgrade -y"

apts.each do |apt|
  dockerfile << "RUN apt-get -qq install -y #{ apt } && apt-get clean"
end
dockerfile << "RUN make -C vendor"
dockerfile << "CMD make check"

File.write("../Dockerfile", dockerfile.join("\n") + "\n")
