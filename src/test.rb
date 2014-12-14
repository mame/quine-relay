# usage:
#   ruby test.rb      # test all CodeGens
#   ruby test.rb Perl # test only Perl CodeGen

require_relative "code-gen"

ENV["PATH"] = "vendor/local/bin:#{ ENV["PATH"] }"

CodeGen.setup_dir("tmp")

gens = ARGV[0] ? [eval(ARGV[0])] : CodeGen::List[0..-2]
text = ARGV[1] || "Hello"

all_check = true

gens.each do |gen|
  puts "test: %p" % gen

  code = Object.new.instance_eval(CodeGen::PROLOGUE + gen.gen_code("#{ text.dump }")) + "\n"
  code.sub!("%%", "%") if gen == Octave_Ook

  steps = gen.steps + [CodeGen::Step[nil, "QR.txt", nil, nil]]

  File.write(steps.first.src, code)

  steps.each_cons(2) do |src, dst|
    cmd = src.cmd.gsub("OUTFILE", dst.src)
    cmd = cmd.gsub(/mv QR\.c QR\.c\.bak &&|&& mv QR\.c\.bak QR\.c/, "")
    cmd = cmd.gsub(/mv QR\.bc QR\.bc\.bak &&|&& mv QR\.bc\.bak QR\.bc/, "")
    cmd = cmd.gsub("$(SCHEME)", "gosh")
    cmd = cmd.gsub("$(JAVASCRIPT)", "rhino")
    cmd = cmd.gsub("$(BF)", "bf")
    cmd = cmd.gsub("$(CC)", "gcc")
    cmd = cmd.gsub("$(CXX)", "g++")
    cmd = cmd.gsub("$(GBS)", "gbs3")
    puts "cmd: " + cmd
    system(cmd) || raise("failed")
  end

  check = File.read("QR.txt").strip == text
  all_check &&= check
  puts "result: #{ check ? "OK" : "NG" }"
  puts
end

puts all_check ? "all ok" : "something wrong"
