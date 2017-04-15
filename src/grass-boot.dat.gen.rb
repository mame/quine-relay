class Sym
  def initialize(name = "<gen:%016x>" % self.object_id)
    @name = name.to_s
  end

  def inspect
    @name
  end

  def [](*others)
    prev = self
    others.each do |other|
      sym = Sym.new
      $prog << [:app, sym, prev, other]
      prev = sym
    end
    prev
  end
end

w = Sym.new(:W)
succ = Sym.new(:Succ)
out = Sym.new(:Out)

def fn(name, &blk)
  params = blk.parameters.map {|_, param_name| Sym.new(param_name) }
  sym = Sym.new(name)
  $prog << [:abs, sym, params]
  blk[*params]
  $prog << [:ret]
  sym
end

$result = []
[10, 32].each do |smallest_char|
  $prog = []

  succ2 = fn(:succ2) {|n| succ[succ[n]] }
  succ4 = fn(:succ4) {|n| succ2[succ2[n]] }
  succ8 = fn(:succ8) {|n| succ4[succ4[n]] }
  succ16 = fn(:succ16) {|n| succ8[succ8[n]] }
  succ32 = fn(:succ32) {|n| succ16[succ16[n]] }
  succ64 = fn(:succ64) {|n| succ32[succ32[n]] }
  succ128 = fn(:succ128) {|n| succ64[succ64[n]] }

  add_num = ->(n, v) do
    ary = [succ128, succ64, succ32, succ16, succ8, succ4, succ2, succ]
    until v == 0
      f = ary.pop
      n = f[n] if v % 2 == 1
      v /= 2
    end
    n
  end

  next_char = fn(:next_char) do |n|
    sc = add_num[n, 129 + smallest_char]
    # n == 127 ? c10 : succ[n]
    add_num[w, 8][n, sc, add_num[n, 1]]
  end

  sub = fn(:sub) do |magic, n, _, m|
    out[n]
    magic[magic, next_char[m]]
  end

  # http://d.hatena.ne.jp/kikx/20080914
  app23 = fn(:app23) {|x, y| y }
  magic = fn(:magic) {|magic_, n, g| g[app23, sub[magic_], n] }

  #main = fn(:main) do |n120,n121,n122,n123,n124,n125,n126,n127,n10,n11,n12,n13,n14,n15,n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,n78,n79,n80,n81,n82,n83,n84,n85,n86,n87,n88,n89,n90,n91,n92,n93,n94,n95,n96,n97,n98,n99,n100,n101,n102,n103,n104,n105,n106,n107,n108,n109,n110,n111,n112,n113,n114,n115,n116,n117,n118,n119|
  #  $prog << [:raw, "!"]
  #  n120 = n119[n33]
  #  n121 = n120[n33]
  #  n122 = n121[n33]
  #  n123 = n122[n33]
  #  n124 = n123[n33]
  #  n125 = n124[n33]
  #  n126 = n125[n33]
  #end
  main = Sym.new(:main)
  $prog << [:abs, main, (smallest_char..127).map { Sym.new }]
  $prog << [:raw, "!"]
  $prog << [:ret]

  start = fn(:start) do |start_, main_, n|
    m = main_[magic[magic, n]]
    w[n, start_, start_[start_, m]][next_char[n]]
  end

  fn(:kick) {|_kick| start[start, main, next_char[w]] }

  stacks = []
  stack = [out, succ, w]
  a = []
  $prog[0..-2].each do |type, *args|
    case type
    when :abs
      new_sym, params = args
      a << "w" * params.size
      stacks << ([new_sym] + stack)
      stack = params.reverse + stack
    when :app
      new_sym, sym0, sym1 = args
      a << "  " + "W" * (stack.index(sym0) + 1) + "w" * (stack.index(sym1) + 1)
      stack.unshift(new_sym)
    when :ret
      stack = stacks.pop
      a << "v"
    when :raw
      a << args.first
    end
  end

  prologue, epilogue = a.join.split.join.split("!")
  def dump(s)
    aa = [*"0".."Z"]
    a = ""
    s.scan(/(v|W*)(w{1,#{aa.size-1}})/) do |s1, s2|
      a << (s1 == "v" ? "/" : aa[s1.size]) << aa[s2.size]
    end
    "#{ a.dump }"
  end
  epilogue = dump(epilogue)
  prologue = dump(prologue)
  $result << 128 - smallest_char
  $result << prologue
  $result << epilogue
end

open("grass-boot.dat", "w") do |f|
  $result.each do |e|
    f.puts e
  end
end
