# Binary lambda calculus interpreter
#
# (C) Copyright 2014, 2015, Yusuke Endoh
# License: MIT
#
# See in detail: http://tromp.github.io/cl/cl.html

# BLC8 or not
binary_mode = ARGV.empty?
$stdout.sync = true

# abstract syntax tree
Var = -> v { [:var, v] }
Abs = -> e { [:abs, e] }
App = -> e1, e2 { [:app, e1, e2] }

# bit stream
current_byte, bit_offset = nil, 0
get_byte = -> do
  bit_offset = binary_mode ? 8 : 1
  current_byte = $stdin.getbyte
end
get_bit = -> do
  get_byte[] if bit_offset == 0
  bit_offset -= 1
  current_byte ? current_byte[bit_offset] == 0 : false
end

# parse
stack, acc = [:exp], nil
until stack.empty?
  v = stack.pop
  acc = case v
  when :exp
    if get_bit[]
      stack << (get_bit[] ? :abs : :app1) << :exp
      nil
    else
      v = 0
      v += 1 until get_bit[]
      Var[v]
    end
  when :abs then Abs[acc]
  when :app1 then stack << acc << :exp; nil
  else App[v, acc] # parsing two exps of app finished
  end
end
bit_offset = 0 # force to align bit offset

# I/O
cons = -> a, b { Abs[App[App[Var[0], a], b]] }
bool = -> b { Abs[Abs[Var[b ? 1 : 0]]] }
if binary_mode
  read = -> i { i > 0 ? cons[bool[get_bit[]], read[i - 1]] : bool[false] }
  in_ = [:in, -> { get_byte[] ? cons[read[8], in_] : bool[false] }]
  outc = 0
  out0, out1 = [0, 1].map {|n| [:out, -> { outc = (2 * outc + n) & 255 }] }
  out0[2] = out1[2] = App[Var[0], cons[out0, out1]]
  outn = [:out, -> { putc(outc) }]
  out_ = outn[2] = App[Var[0], cons[cons[out0, out1], outn]]
else
  in_ = [:in, -> { get_byte[] ? cons[bool[get_bit[]], in_] : bool[false] }]
  out0, out1 = [0, 1].map {|n| [:out, -> { print(n) }] }
  out_ = out0[2] = out1[2] = App[Var[0], cons[out0, out1]]
end

# eval-loop
exp = App[Abs[out_], App[acc, in_]]
env, stack = [], [[nil, nil]]
while true
  case exp[0]
  when :var
    exp[1].times { env = env[2] }
    if env.frozen?
      valexp, valenv = env # already forced
    else # not forced yet
      stack.last << env
      exp, env = env[0], env[1]
      next
    end
  when :app
    stack << [exp[2], env]
    next exp = exp[1]
  when :abs
    valexp, valenv = exp[1], env
  when :in  then next exp = exp[1][]
  when :out then exp[1][]; valexp, valenv = exp[2], env
  end
  env = stack.pop
  until env.size == 2
    th = env.pop
    th[0] = valexp # force
    th[1] = valenv
    th.freeze
  end
  break unless env[0]
  exp = valexp
  env[2] = valenv
end
