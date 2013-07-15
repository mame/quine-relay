require_relative "code-gen"

CodeGen.setup_dir("tmp")

gens = ARGV[1] ? [eval(ARGV[1])] : CodeGen::List[0..-2]
text = ARGV[2] || "Hello"

all_check = true

gens.each do |gen|
  puts "test: %p" % gen

  code = Class.new.class_eval(CodeGen::PROLOGUE + gen.gen_code("%(#{ text })")) + "\n"

  steps = gen.steps + [CodeGen::Step[nil, ".txt", nil, nil]]

  File.write("QR" + steps.first.ext, code)

  steps.each_cons(2) do |src, dst|
    cmd = src.cmd.gsub("OUTFILE", "QR" + dst.ext)
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
