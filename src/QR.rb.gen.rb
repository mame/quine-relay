require_relative "code-gen"

gen_prologue_1 = GenPrologue.split(?;)[0, 2].join(?;)
gen_prologue_2 = GenPrologue.split(?;)[2..-1].join(?;)

s =
  gen_prologue_2 + ?; +
  GenSteps[0..-2].inject('"eval$s=%q(#$s)"') do |code, gen_step|
    gen_step.code.sub("PREV") { "check[#{ gen_step.name.dump },#{ code },:check_end]" }.chomp
  end

def run_check(src)
  steps = GenSteps.each
  check = ->(name, prev, _check_end) do
    step = steps.next
    raise if step.name != name
    step.check.call(prev)
    prev
  end
  Object.new.instance_eval(src)
end

hooked = s
s = s.gsub(/check\["[^"]*",|,:check_end\]/, "")

if false
  # search characters rarely used
  h = {}
  32.upto(126) {|c| h[c.chr] = 0 }
  s.chars.group_by {|c| c }.map do |c, a|
    h[c] = a.size
  end
  %w(( ) [ ] { } \  \\ \").each {|c| h.delete(c) }
  p *h.sort_by {|k, v| v }
  exit
end

if false
  # search sequences that often appear
  h = Hash.new(0)
  2.upto(10) do |n|
    s.chars.each_cons(n) do |a|
      h[a.join] += n-1
    end
  end
  p *h.sort_by {|k, v| v }.reverse
  exit
end

# a table of short-hand character for sequences that often appear
# key => [sequence, expression that restores it]
# the sequences are matched in this order, so a later one may refer to an earlier key
# a key must not appear outside a double-quoted string literal
ABBREV = {
  ?~ => [" ",       "g"],
  ?` => ["\\",      "B"],
  ?K => ["print",   ":print"],
  ?J => ['#{',      %q('#{')],
  ?Y => ["``",      "B*2"],
  ?H => ["in",      ":in"],
  ?^ => ["~maH()",  %q(g+'main()')],
  ?X => ["]}",      %q(']}')],
  ?Z => [".gsub",   %q('.gsub')],
  ?G => ["write",   ":write"],
  ?U => ["JE[",     %q('#{E[')],
  ?! => ["for",     ":for"],
}

s = s.gsub(/[#{ ABBREV.keys.join }]/){"\\x%02x" % $&.ord}

# search perfect and simplest hash
a = ABBREV.keys.join.bytes
best = nil
a.size.upto(255) do |n|
  a.size.upto(255) do |m|
    b = a.map {|c| c%n%m }
    next unless b.uniq.size == a.size
    cand = [b.max, n.to_s.size + m.to_s.size, n, m]
    best = [cand, b] if !best || (cand <=> best[0]) < 0
  end
end
$N, $M, $B = best[0][2], best[0][3], best[1]

ABBREV.each do |k, (v, _)|
  s = s.gsub(v, k)
end

a = [0] * ($B.max + 1)
ABBREV.each do |k, (_, r)|
  a[k.ord % $N % $M] = r
end
a = a.join(",")

code = <<-END.split.join
  eval$s=%q(eval(%w(

    #{gen_prologue_1};
    puts(eval(
      %q(#{ s }).gsub(/[#{ ABBREV.keys.sort.join }]/){[#{ a }][$&.ord%#{ $N }%#{ $M }]}
    ))

  )*""))
END

$stderr.puts "size: #{ code.b.size }"
code.chop!

TEMPLATE = File.read("uroboros.txt")
width = TEMPLATE[/.*/].size
while TEMPLATE.count("#") - width < code.size
  s = TEMPLATE.count("#")
  line = TEMPLATE[/^#*$/]
  TEMPLATE.replace((line + "\n" + TEMPLATE + line).gsub!(/^|$/, "######") + "\n")
  width = TEMPLATE[/.*/].size
  warn "overflow!: #{ s - width }->#{ TEMPLATE.count("#") - width }"
end
# the art cuts the code at every run of "#", and a cut inside the trailing `)*""` makes it a splat
code[-5, 0] = ?; while TEMPLATE.scan(/#+/).inject([0]) {|a, r| a << a[-1] + r.size }.any? {|i| code.size - 5 < i && i < code.size }
PAYLOAD = code[/%w\((.*)\)\*""\)\z/m, 1]
PADDING = "".ljust(width, "#_buffer_for_future_bug_fixes_")
COPYRIGHT =
  "  Quine Relay -- Copyright (c) 2013, 2014 Yusuke Endoh (@mametter), @hirekoke  ".
  center(width, "#")[0..-2]

code = TEMPLATE.gsub(/#+/) { w = $&.size; code.slice!(0, w).ljust(w, PADDING) }.chomp
code[-1] = ")"

code[-1 - COPYRIGHT.size, COPYRIGHT.size] = COPYRIGHT

File.write("../QR.rb", code + "\n")

# parse check
RubyVM::AbstractSyntaxTree.parse(code)
RubyVM::AbstractSyntaxTree.parse($s = code[/\Aeval\$s=%q\((.*)\)\z/m, 1])
raise unless eval(code[/%w\(.*\)\*""/m]) == PAYLOAD

# $s is what the innermost step prints, so check only once QR.rb is written
run_check((gen_prologue_1 + ?; + hooked).gsub(/[^\S ]/, ""))
