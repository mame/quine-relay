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
yaml["script"] = ["sudo docker run --privileged --name qr -e CI=$TRAVIS -t quine-relay"]
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
  -  "  mkdir spoiler",
  *srcs.map do |s|
    "  sudo docker cp qr:/usr/local/share/quine-relay/#{ s } spoiler/"
  end,
  "  cd spoiler",
  "  git init --quiet",
  "  git config user.name 'Yusuke Endoh'",
  "  git config user.email 'mame@ruby-lang.org'",
  "  git add .",
  "  git commit -m spoiler --quiet",
  "  git push --force --quiet \"https://${GH_TOKEN}@github.com/mame/quine-relay\" master:spoiler",
  "  echo The intermediate sources are available: https://github.com/mame/quine-relay/tree/spoiler"
]
File.write("../.travis.yml", s.join("\n") + "\n")
