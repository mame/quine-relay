# Binary lambda calculus interpreter
#
# (C) Copyright 2014, Yusuke Endoh
# License: MIT
#
# See in detail: http://homepages.cwi.nl/~tromp/cl/cl.html

# BLC8 or not
binary_mode = ARGV.empty?
$stdout.sync = true

# abstract syntax tree
Var = Struct.new(:num)
Abs = Struct.new(:exp) # Abs[nil] represents input list
App = Struct.new(:exp1, :exp2)
Out = Struct.new(:type)

cons = ->(a, b) { Abs[App[App[Var[0], a], b]] }
bool = ->(b) { Abs[Abs[Var[b ? 1 : 0]]] }
read = ->(i) { i > 0 ? cons[bool[get_bit[]], read[i - 1]] : bool[false] }

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

# deconstructor of list of boolean lists (for BLC8 I/O)
E1 = Abs[App[Var[0], cons[cons[Out[0], Out[1]], Out[nil]]]]
# deconstructor of boolean list
E2 = Abs[App[Var[0], cons[Out[0], Out[1]]]]

# parse
stack = [:exp]
acc = nil
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
term = App[binary_mode ? E1 : E2, App[acc, Abs[nil]]]
bit_offset = 0 # force to align bit offset

# evaluate
env, out, stack = nil, 0, []
Closure = Struct.new(:term, :env, :next)
while true
  case term
  when Var
    term.num.times { env = env.next }
    term, env = env.term, env.env
  when App
    stack << Closure[term.exp2, env]
    term = term.exp1
  when Abs
    if term.exp
      exit if stack.empty?
      stack.last.next = env
      term, env = term.exp, stack.pop
    else
      # input
      term.exp = (get_byte[] ?
                  cons[binary_mode ? read[8] : bool[get_bit[]], Abs[nil]] :
                  bool[false]).exp
    end
  when Out
    if binary_mode
      if term.type
        out = (2 * out | term.type) & 255
        term = E2
      else
        putc(out)
        term = E1
      end
    else
      print(term.type)
      term = E2
    end
  end
end
