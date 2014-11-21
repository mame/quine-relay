RE = /^\s*::=.*\n(?<body>(?m:.*))|^(?<lhs>.+?)::=(?<rhs>.*)$/

rules, body = {}, ""
File.binread($*[0]).scan(RE) do
  $~[:lhs] ? rules[$~[:lhs]] = $~[:rhs] : body = $~[:body].delete("\n")
end

loop do
  #puts body
  targets = []
  rules.each do |lhs, rhs|
    pos = 0
    while npos = body.index(lhs, pos)
      targets << [npos, lhs, rhs]
      pos = npos + 1
    end
  end
  break if targets.empty?
  pos, lhs, rhs = targets.sample
  case rhs
  when ":::" then rhs = gets
  when /^~/ then print $'; rhs = ""
  end
  body[pos, lhs.size] = rhs
end