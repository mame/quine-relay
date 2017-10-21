require_relative "code-gen"

gen_prologue_1 = GenPrologue.split(?;)[0, 2].join(?;)
gen_prologue_2 = GenPrologue.split(?;)[2..-1].join(?;)

s =
  gen_prologue_2 + ?; +
  GenSteps[0..-2].inject('"eval$s=%q(#$s)"') {|code, s| s.code.sub("PREV"){ code }.chomp }

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

# a table of short-hand character for sequences that often apper
ABBREV = {
  ?~ => " ",
  ?` => "\\",
  ?^ => "``",
  ?Z => "print",
  ?X => "ain()",
  ?J => "tring",
  ?H => "write",
  ?K => "gsub",
  ?! => "in",
  ?Y => "^^",
}

s = s.gsub(/[#{ ABBREV.keys.join }]/){"\\x%02x" % $&.ord}

# search perfect and simplest hash
a = ABBREV.keys.join.bytes
max = 1000
a.size.upto(90) do |n|
  a.size.upto(90) do |m|
    b = a.map {|c| c%n%m }
    if b.uniq.size >= a.size&&b.max<max
      $N, $M, $B = n, m, b
      max = b.max
    end
  end
end

ABBREV.each do |k, v|
  s = s.gsub(v, k)
end

a = [0] * ($B.max + 1)
ABBREV.each do |k, v|
  v = case v
      when "\\" then "B"
      when "``" then "B*2"
      when "^^" then "B*4"
      when " " then "g"
      when "ain()" then %("ain()")
      else ":#{ v }"
      end
  a[k.ord % $N % $M] = v
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
PADDING = "".ljust(width, "#_buffer_for_future_bug_fixes_")
COPYRIGHT =
  "  Quine Relay -- Copyright (c) 2013, 2014 Yusuke Endoh (@mametter), @hirekoke  ".
  center(width, "#")[0..-2]

code = TEMPLATE.gsub(/#+/) { w = $&.size; code.slice!(0, w).ljust(w, PADDING) }.chomp
code[-1] = ")"

code[-1 - COPYRIGHT.size, COPYRIGHT.size] = COPYRIGHT

File.write("../QR.rb", code + "\n")
