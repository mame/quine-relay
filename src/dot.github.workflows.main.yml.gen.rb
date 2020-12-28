require "yaml"
require_relative "code-gen"

apts = RunSteps.flat_map {|s| s.apt }
apts = apts.flatten.map {|apt| apt }.compact
apts.delete("ruby2.1")

srcs = RunSteps.flat_map {|s| s.src }

cp_cmds = srcs.map do |s|
  "          sudo docker cp qr:/usr/local/share/quine-relay/#{ s } spoiler/"
end.join("\n")

File.write(File.join(__dir__, "../.github/workflows/main.yml"), <<END)
name: CI
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v1
      - name: docker build
        run: |
          sudo docker build -t quine-relay .
      - name: docker run
        run: |
          sudo docker run --privileged --name qr -e CI=true -t quine-relay
      - name: push spoiler
        run: |
          mkdir spoiler
#{ cp_cmds }
          cd spoiler
          git init --quiet
          git config user.name 'Yusuke Endoh'
          git config user.email 'mame@ruby-lang.org'
          git add .
          git commit -m spoiler --quiet
          git push --force --quiet https://${GITHUB_ACTOR}:${GITHUB_TOKEN}@github.com/mame/quine-relay.git master:spoiler
          echo The intermediate sources are available: https://github.com/mame/quine-relay/tree/spoiler
        if: github.event_name == 'push' && github.ref == 'refs/heads/master'
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
END
