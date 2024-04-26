require_relative "code-gen"

Dir.chdir("..") do
  system("sha256sum", "-b", *RunSteps.map {|s| s.src }, out: "SHA256SUMS")
end
