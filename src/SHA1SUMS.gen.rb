require_relative "code-gen"

Dir.chdir("..") do
  system("sha1sum", "-b", *RunSteps.map {|s| s.src }, out: "SHA1SUMS")
end
