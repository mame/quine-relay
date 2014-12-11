require "yaml"
require_relative "code-gen"

apts = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.apt } }
other_packages = %w(cmake libpng12-dev libgd2-xpm-dev groff tcc)


utopic_to_precise = {
  "clojure1.4" => "clojure",
}

apts = apts.flatten.map {|apt| utopic_to_precise[apt] || apt }.compact
apts.delete("ruby2.0")

yaml = {}
yaml["language"] = "ruby"
yaml["rvm"] = ["2.1.0"]
yaml["env"] = ["PATH=/usr/games:$PATH"]
yaml["before_install"] = [
  "sudo service postgresql stop",
  "sudo service mysql stop",
  "sudo apt-get update",
  'sudo apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade',
  "sudo add-apt-repository ppa:ecere-team/ppa -y",
  "sudo add-apt-repository ppa:directhex/ppa -y",
  "sudo apt-get update",
  'sudo apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade',
]
[*apts.flatten.compact.uniq, *other_packages].sort.each do |apt|
  yaml["before_install"] << "sudo apt-get install #{ apt }"
end
yaml["before_script"] = [
  "make -C vendor/",
]
yaml["script"] = ["make CC=tcc"]


File.write("../.travis.yml", YAML.dump(yaml))
