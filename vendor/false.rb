code = File.read($*[0])
stack = []

pos = 0

parse = ->(re) do
  m = code.match(re, pos)
  pos = m.end(0) - 1
  m[1]
end

while code[pos]
  case code[pos]
  when /\d/ then s = parse[/(\d+)/]; stack << s.to_i
  when /[a-z]/ then stack << code[pos].to_sym
  when ?+ then a, b = stack.pop, stack.pop; stack << b + a
  when ?- then a, b = stack.pop, stack.pop; stack << b - a
  when ?* then a, b = stack.pop, stack.pop; stack << b * a
  when ?/ then a, b = stack.pop, stack.pop; stack << b / a
  when ?& then a, b = stack.pop, stack.pop; stack << b & a
  when ?| then a, b = stack.pop, stack.pop; stack << b | a
  when ?_ then stack << -stack.pop
  when ?~ then stack << ~stack.pop
  when ?= then a, b = stack.pop, stack.pop; stack << (b == a ? -1 : 0)
  when ?> then a, b = stack.pop, stack.pop; stack << (b >  a ? -1 : 0)
  when ?% then stack.pop
  when ?$ then stack << stack.last
  when ?\\ then a, b = stack.pop, stack.pop; stack << a << b
  when ?@ then a, b, c = stack.pop, stack.pop, stack.pop; stack << b << a << c
  when ?O then a = stack.pop; stack << stack[-1 - a]
  when ?: then a, b = stack.pop, stack.pop; vars[a] = b
  when ?; then a = stack.pop; stack << vars[a]
  when ?. then print "%d" % stack.pop
  when ?, then print stack.pop.chr
  when ?^ then stack << $stdin.getc
  when ?B then $stdin.flush; $stdout.flush
  when ?" then s = parse[/"(.*?)"/]; print s
  when ?{ then s = parse[/\}/]
  when ?' then stack << code[pos += 1].ord
  when ?` then raise
  when ?[
    stack << [:func, pos]
    m = code.match(/(?<b>\[(\g<b>|\{.*?\}|\".*?\"|[^\{\}\"\[\]])*\])/)
    pos = m.end(0) - 1
  when ?]
    case npos = ret.pop
    when :det
      if stack.pop == 0
        pos = ret.pop
        ret.pop; ret.pop
      else
        ret << :body
        pos = ret[-3]
      end
    when :body
      stack << :det
      pos = ret[-4]
    else
      pos = npos
    end
  when ?! then ret << pos; pos = stack.pop[1]
  when ??
    tpos = stack.pop[1]; b = stack.pop; (ret << pos; pos = tpos) if b
  when ?#
    body = stack.pop[1]
    det = stack.pop[1]
    ret << det << body << pos << :det
    pos = det
  when /\A\s\z/
  else
    raise "unknown symbol: %p" % code[pos]
  end
  pos += 1
end
