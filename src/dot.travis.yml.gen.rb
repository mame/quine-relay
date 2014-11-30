require "yaml"
require_relative "code-gen"

apts = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.apt } }

utopic_to_precise = {
  "clojure1.4" => "clojure",
}

apts = apts.flatten.map {|apt| utopic_to_precise[apt] || apt }.compact
apts.delete("ruby2.0")

yaml = {}
yaml["language"] = "ruby"
yaml["rvm"] = ["2.1.0"]
yaml["env"] = "PATH=/usr/games:$PATH"
yaml["before_install"] = [
  "sudo apt-get update -qq",
  'sudo apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade -qq',
  "sudo apt-get install -qq #{ apts.join(" ") }",
]
yaml["before_script"] = [
  "cd vendor/",
  "make",
  "cd ../",
]
yaml["script"] = ["make"]


File.write("../.travis.yml", YAML.dump(yaml))
