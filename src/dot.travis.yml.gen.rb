require "yaml"
require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
other_packages = %w(cmake libpng12-dev libgd2-xpm-dev groff tcc)


apts = apts.flatten.map {|apt| apt }.compact
apts.delete("ruby2.1")

yaml = {}
yaml["sudo"] = "required"
yaml["services"] = ["docker"]
yaml["language"] = "ruby"
yaml["before_install"] = ["sudo docker build -t quine-relay ."]
yaml["script"] = ["sudo docker run --privileged -t quine-relay"]


File.write("../.travis.yml", YAML.dump(yaml))
