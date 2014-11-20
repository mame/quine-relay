IN_ADDR = -1
OUT_ADDR = -1

mem = File.read(ARGV[0]).split.map do |v|
  if v.start_with?(?#)
    case v
    when "#IN"  then IN_ADDR
    when "#OUT" then OUT_ADDR
    else raise "Unresolved register: #{ v }"
    end
  else
    v.to_i
  end
end

ip = 0
while 0 <= ip && ip < mem.size
  a, b, c = mem[ip], mem[ip + 1], mem[ip + 2]
  ip += 3

  if a == IN_ADDR
    ma = -($stdin.getc || -1).ord
  else
    ma = mem[a]
  end

  if b == OUT_ADDR
    putc ma
  else
    mb = mem[b] -= ma
    ip = c if mb <= 0
  end
end
