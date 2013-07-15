require_relative "code-gen"

s = '%(eval$s=%q(#$s))'
CodeGen::List[0..-2].each {|c| s = c.gen_code(s).tr(" \\","X`") }

code = <<-END.split.join
  eval$s=%q(eval(%w(

  #{CodeGen::PROLOGUE}
  puts(eval(%q(#{
    s.gsub("print","H").gsub("tring","J").gsub("main","K").gsub("``","^")
    # 'H' % 9 => 0
    # 'J' % 9 => 2
    # 'K' % 9 => 3
    # '^' % 9 => 4
    # '`' % 9 => 6
    # 'X' % 9 => 7
  }).gsub(/[HJK^`X]/){[:print,0,:tring,:main,B*2,0,B,32.chr][$&.ord%9]}))

  
  )*""))
END

code[-6, 0] = " ##  Quine Relay -- Copyriht (c) 2013 Yusuke Endoh, @hirekoke  ##"

$stderr.puts "size: #{ code.size }"

Tmpl = File.read("uroboros.txt")

code[-6, 0] = "#" * (Tmpl.count("#") - code.size)
code = Tmpl.gsub(/#+/) { code.slice!(0, $&.size) }
File.write("../QR.rb", code)
