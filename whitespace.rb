require "scanf"

RE = /\G(?<n>[01]+){0}
  ( 00  (?<push> )\g<n>2
  | 010 (?<copy> )\g<n>2
  | 012 (?<slide>)\g<n>2
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
  | 200 (?<mark>)\g<n>2
  | 201 (?<call>)\g<n>2
  | 202 (?<jump>)\g<n>2
  | 210 (?<jz>)\g<n>2
  | 211 (?<jn>)\g<n>2
  | 212 (?<ret>)
  | 222 (?<end>)
  |     (?<eof>)\z
  |     (?<error>)
  )/x

code, labels = [], {}
File.read($*[0]).gsub(/[^ \t\n]/m, "").tr(" \t\n", "012").scan(RE) do
  insn = ($~.names - ["n"]).find {|n| $~[n] }
  num = $~[:n][1..-1].to_i(2) * ($~[:n][0] == ?0 ? 1 : -1) if $~[:n]
  raise "Unrecognised input" if insn == "error"
  insn == "mark" ? labels[num] = code.size : code << [insn, num]
end

pc, call, stack, heap = 0, [], [], {}
loop do
  insn, num = code[pc]
  pc += 1
  case insn
  when "push"  then stack << num
  when "dup"   then stack << stack.last
  when "copy"  then stack << stack[-num - 1]
  when "slide" then n = stack.pop; stack.pop(num); stack << n
  when "swap"  then n = stack.pop; m = stack.pop; stack << n << m
  when "pop"   then stack.pop
  when "add"   then n = stack.pop; stack << stack.pop + n
  when "sub"   then n = stack.pop; stack << stack.pop - n
  when "mul"   then n = stack.pop; stack << stack.pop * n
  when "div"   then n = stack.pop; stack << stack.pop / n
  when "mod"   then n = stack.pop; stack << stack.pop % n
  when "set"   then n = stack.pop; heap[stack.pop] = n
  when "get"   then stack << heap[stack.pop]
  when "outc"  then putc stack.pop
  when "readc" then heap[stack.pop] = $stdin.getc.ord
  when "outn"  then print stack.pop
  when "readn" then heap[stack.pop] = $stdin.scanf("%d")[0] || raise("Integer expected")
  when "call"  then call << pc; pc = labels[num]
  when "jump"  then pc = labels[num]
  when "jz"    then pc = labels[num] if stack.pop == 0
  when "jn"    then pc = labels[num] if stack.pop <  0
  when "ret"   then pc = call.pop
  when "end"   then break
  when "eof"   then raise("Reached EOF")
  end
end
