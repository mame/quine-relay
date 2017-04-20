require "yaml"
require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
apts = apts.flatten.map {|apt| apt }.compact
apts.delete("ruby2.1")

srcs = RunSteps.flat_map {|s| s.src }

yaml = {}
yaml["sudo"] = "required"
yaml["services"] = ["docker"]
yaml["language"] = "ruby"
yaml["before_install"] = ["sudo docker build -t quine-relay ."]
yaml["script"] = ["sudo docker run --privileged --name qr -t quine-relay"]
yaml["after_success"] = [
  'test $TRAVIS_PULL_REQUEST == "false" && test $TRAVIS_BRANCH == "master" && sh .travis.yml'
]
yaml["env"] = {
  "global" => {
    "secure" => "NGDakAqRZgGJwEJTlXenhoXcq9ulf0X0fjnC+oF+ktTXCRpdbQd8+faxIW5DR26qF5OMWPqsLtUv8HtQyv5P5gVNs41hXygmNU1R9TOMpw64FjXtkD1HNf0D4jE2STuUU2xB+sCifeb9z6SvMpcy6ZswBlhAVnV+5dboNZL0Ww0="
  }
}

s = [
  'echo : dummy"',
  # XXX: find a method to dump yaml without document marker
  *Psych.dump(yaml).lines.map {|l| l.chomp }.drop(1),
  'dummy: |',
  '  dummy" > /dev/null',
]
File.write("../.travis.yml", s.join("\n") + "\n")
