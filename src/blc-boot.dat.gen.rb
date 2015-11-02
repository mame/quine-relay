def dump(e, env)
  if e.is_a?(Symbol)
    ?1 * (env.index(e) + 1) + ?0
  elsif e[0] == :abs!
    vs = [*e[1]].reverse
    "00" * vs.size + dump(e[2], vs + env)
  else
    "01" * (e.size - 1) + e.map {|x| dump(x, env) } * ""
  end
end

L = -> x, y { [:abs!, x, y] }
Let = -> v, x, y { [L[v, y], x] }
Cons = -> x, y { L[:f, [:f, x, y]] }
False = L[[:x, :y], :y]
True  = L[[:x, :y], :x]

Num = -> n {
  [n[7] == 1 ? :ConsFalse : :ConsTrue,
  [n[6] == 1 ? :ConsFalse : :ConsTrue,
  [n[5] == 1 ? :ConsFalse : :ConsTrue,
  [n[4] == 1 ? :ConsFalse : :ConsTrue,
  [n[3] == 1 ? :ConsFalse : :ConsTrue,
  [n[2] == 1 ? :ConsFalse : :ConsTrue,
  [n[1] == 1 ? :ConsFalse : :ConsTrue,
  [n[0] == 1 ? :ConsFalse : :ConsTrue, False]]]]]]]]
}

OutBF = -> tl { [:Cons, Num[?..ord], [:ConsGt, tl]] }
DblBF = # [->++<]>
  L[:tl,
    Let[:ConsTrue, :ConsTrue,
    Let[:ConsFalse, :ConsFalse,
        [:Cons, Num[?[.ord],
        [:Cons, Num[?-.ord],
        [:ConsGt,
        [:ConsPlus,
        [:ConsPlus,
        [:Cons, Num[?<.ord],
        [:Cons, Num[?].ord],
        [:ConsGt, :tl]]]]]]]]]]]

# each_bit bit tl =
#   ("[->++<]>" concat (tl each_bit))
EachBit =
  [L[:x, [:x, :x]], # recursive
   L[[:self, :bit, :tl, :next],
     [:tl, [:self, :self], [:DblBF, [:bit, [:ConsPlus, :next], :next]]]]]

# each_byte byte tl =
#   (byte each_bit ".>") concat (tl each_byte)
EachByte =
  [L[:x, [:x, :x]], # recursive
   L[[:self, :byte, :tl],
     Let[:Cons, L[[:x, :y, :f], [:f, :x, :y]],
     Let[:ConsTrue, [:Cons, True],
     Let[:ConsFalse, [:Cons, False],
     Let[:ConsPlus, [:Cons, Num[?+.ord]],
     Let[:ConsGt, [:Cons, Num[?>.ord]],
     Let[:DblBF, DblBF,
         [:byte, EachBit, OutBF[[:tl, [:self, :self]]]]]]]]]]]]

tree = EachByte
boot = "10" + dump(tree, [])

# 20 x ? boot = 00010100 x ? boot = \a (\b x) ? boot = \a x boot
# 22 x y = 00010110 x y = \a a x y = cons x y
# 4 222  = 00000100 11011110 = \a \b (\c b) (????) = \a \b b = false
# 4 226  = 00000100 11100010 = \a \b (\c a) (\c c) = \a \b a = true
if ARGV[0]
  $stderr.puts(boot.size, (boot.size + 7) / 8)
  a = [20]
  ("foo" * 100 + "\n").each_byte do |b|
    a << 22
    0.upto(7) do |i|
      a << 22 << 4 << 222 + b[i] * 4
    end
    a << 4 << 222
  end
  a << 4 << 222
  print a.pack("C*") + [boot].pack("B*")
else
  File.write("blc-boot.dat", [boot].pack("B*"))
end
