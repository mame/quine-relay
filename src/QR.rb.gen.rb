require_relative "code-gen"

s =
CodeGen::List[0..-2].inject('%(eval$s=%q(#$s))') {|c, lang|
  lang.gen_code(c).tr(" \\","X`")
}

s = s.gsub("print","H").gsub("tring","J").gsub("main","K").gsub("``","^")

# lookup of escapes and common names
# 'H' % 9 => 0 (print)
# 'J' % 9 => 2 (tring)
# 'K' % 9 => 3 (main)
# '^' % 9 => 4 (2 * backslash)
# '`' % 9 => 6 (backslash)
# 'X' % 9 => 7 (space)
code = <<-END.split.join
  eval$s=%q(eval(%w(

    #{CodeGen::PROLOGUE}
    puts(eval(
      %q(#{ s }).gsub(/[HJK^`X]/){[:print,0,:tring,:main,B*2,0,B,?\\s][$&.ord%9]}
    ))

  )*""))
END

$stderr.puts "size: #{ code.b.size }"

TEMPLATE = File.read("uroboros.txt")
width = TEMPLATE[/.*/].size
PADDING = "".ljust(width, "#_buffer_for_future_bug_fixes_")
COPYRIGHT =
  "  Quine Relay -- Copyright (c) 2013 Yusuke Endoh (@mametter), @hirekoke  ".
  center(width, "#")[0..-2]

code.chop!
code = TEMPLATE.gsub(/#+/) { w = $&.size; code.slice!(0, w).ljust(w, PADDING) }.chomp
code[-1] = ")"

code[-1 - COPYRIGHT.size, COPYRIGHT.size] = COPYRIGHT

File.write("../QR.rb", code + "\n")
