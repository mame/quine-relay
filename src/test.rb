# usage:
#   ruby test.rb      # test all CodeGens
#   ruby test.rb Perl # test only Perl CodeGen

require_relative "code-gen"

CodeGen.setup_dir("tmp")

gens = ARGV[0] ? [eval(ARGV[0])] : CodeGen::List[0..-2]
text = ARGV[1] || "Hello"

all_check = true

gens.each do |gen|
  puts "test: %p" % gen

  code = Class.new.class_eval(CodeGen::PROLOGUE + gen.gen_code("#{ text.dump }")) + "\n"

  steps = gen.steps + [CodeGen::Step[nil, "QR.txt", nil, nil]]

  File.write(steps.first.src, code)

  steps.each_cons(2) do |src, dst|
    cmd = src.cmd.gsub("OUTFILE", dst.src)
    cmd = cmd.gsub(/mv QR\.c(\.bak)? QR\.c(\.bak)? &&/, "")
    puts "cmd: " + cmd
    system(cmd) || raise("failed")
  end

  check = File.read("QR.txt").strip == text
  all_check &&= check
  puts "result: #{ check ? "OK" : "NG" }"
  puts
end

puts all_check ? "all ok" : "something wrong"
