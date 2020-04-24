RE = /\G(?<num>[01]+){0}(?<label>[01]*){0}
  ( 00  (?<push> )\g<num>2
  | 010 (?<copy> )\g<num>2
  | 012 (?<slide>)\g<num>2
  | 020 (?<dup>)
  | 021 (?<swap>)
  | 022 (?<pop>)
  | 1000(?<add>)
  | 1001(?<sub>)
  | 1002(?<mul>)
  | 1010(?<div>)
  | 1011(?<mod>)
  | 110 (?<set>)
  | 111 (?<get>)
  | 1200(?<outc>)
  | 1201(?<outn>)
  | 1210(?<readc>)
  | 1211(?<readn>)
  | 200 (?<mark>)\g<label>2
  | 201 (?<call>)\g<label>2
  | 202 (?<jump>)\g<label>2
  | 210 (?<jz>)\g<label>2
  | 211 (?<jn>)\g<label>2
  | 212 (?<ret>)
  | 222 (?<end>)
  |     (?<eof>)\z
  |     (?<error>)
  )/x

names = RE.names.map {|n| n.to_sym } - [:num, :label]
code, labels = [], {}
File.read($*[0]).gsub(/[^ \t\n]/m, "").tr(" \t\n", "012").scan(RE) do
  insn = names.find {|n| $~[n] }
  arg = $~[:num]
  arg = arg[1..-1].to_i(2) * (arg[0] == ?0 ? 1 : -1) if arg
  arg = $~[:label] if $~[:label]
  raise "Unrecognised input" if insn == :error
  insn == :mark ? labels[arg] = code.size : code << [insn, arg]
end

def read_int
  s = ""
  while true
    ch = $stdin.getc
    case ch
    when /\d+/ then s << ch
    when /\s+/ then break
    else raise "Integer expected"
    end
  end
  s.to_i
end

pc, call, stack, heap = 0, [], [], Hash.new(0)
loop do
  insn, arg = code[pc]
  pc += 1
  case insn
  when :push  then stack << arg
  when :dup   then stack << stack.last
  when :copy  then stack << stack[-arg - 1]
  when :slide then n = stack.pop; stack.pop(arg); stack << n
  when :swap  then n = stack.pop; m = stack.pop; stack << n << m
  when :pop   then stack.pop
  when :add   then n = stack.pop; stack << stack.pop + n
  when :sub   then n = stack.pop; stack << stack.pop - n
  when :mul   then n = stack.pop; stack << stack.pop * n
  when :div   then n = stack.pop; stack << stack.pop / n
  when :mod   then n = stack.pop; stack << stack.pop % n
  when :set   then n = stack.pop; heap[stack.pop] = n
  when :get   then stack << heap[stack.pop]
  when :outc  then putc stack.pop
  when :readc then heap[stack.pop] = $stdin.getc.ord
  when :outn  then print stack.pop
  when :readn then heap[stack.pop] = read_int
  when :call  then call << pc; pc = labels[arg]
  when :jump  then pc = labels[arg]
  when :jz    then pc = labels[arg] if stack.pop == 0
  when :jn    then pc = labels[arg] if stack.pop <  0
  when :ret   then pc = call.pop
  when :end   then break
  when :eof   then raise("Reached EOF")
  end
end
