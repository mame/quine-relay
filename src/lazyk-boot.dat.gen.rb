Var = Struct.new(:name)
Abs = Struct.new(:var, :exp)
App = Struct.new(:exp1, :exp2)
Comb = Struct.new(:type)
S, K, I = Comb[:S], Comb[:K], Comb[:I]

class Var
  def fvs
    { name => 1 }
  end
  def remove_abs
    self
  end
  def remove_var(n)
    if n == name
      I
    else
      App[K, self]
    end
  end
  def inspect(need_paren = true)
    name.to_s
  end
end

class Abs
  def fvs
    @fvs ||= (
      s = exp.fvs.dup
      s.delete(var)
      s
    )
  end
  def remove_abs
    exp.remove_abs.remove_var(var)
  end
  def remove_var(v)
    if exp.fvs[v]
      exp.remove_abs.remove_var(v)
    else
      App[K, self]
    end
  end
  def inspect(need_paren = true)
    vs, e = [var], exp
    while e.is_a?(Abs)
      vs << e.var
      e = e.exp
    end
    "(\\#{ vs * " " }. #{ e.inspect(false) })"
  end
end
class App
  def fvs
    @fvs ||= (
      s = exp1.fvs.dup
      exp2.fvs.each do |v, n|
        s[v] ||= 0
        s[v] += n
      end
      s
    )
  end
  def remove_abs
    App[exp1.remove_abs, exp2.remove_abs]
  end
  def remove_var(v)
    if fvs[v]
      if exp2.is_a?(Var) && exp2.name == v && !exp1.fvs[v]
        exp1
      else
        App[App[S, exp1.remove_var(v)], exp2.remove_var(v)]
      end
    else
      App[K, self]
    end
  end
  def inspect(need_paren = true)
    s = "%s %s" % [exp1.inspect(false), exp2.inspect(true)]
    s = "(#{ s })" if need_paren
    s
  end
  def to_unlambda
    "`" + exp1.to_unlambda + exp2.to_unlambda
  end
  def simplify
    if exp1 == I
      exp2.simplify # I x -> x
    elsif exp1.is_a?(App)
      if exp1.exp1 == K
        exp1.simplify # K x y -> x
      elsif exp1.exp1.is_a?(App) && exp1.exp1.exp1 == S
        if exp1.exp1.exp2.is_a?(App) && exp1.exp1.exp2.exp1 == K
          App[exp1.exp1.exp2.exp2, App[exp1.exp2, exp2]].simplify # S (K x) y z -> x (y z)
        elsif exp1.exp2.is_a?(App) && exp1.exp2.exp1 == K
          App[App[exp1.exp1.exp2, exp2], exp1.exp2.exp2].simplify # S x (K y) z -> x z y
        else
          App[exp1.simplify, exp2.simplify]
        end
      else
        App[exp1.simplify, exp2.simplify]
      end
    else
      App[exp1.simplify, exp2.simplify]
    end
  end
end

class Comb
  def fvs
    {}
  end
  def remove_abs
    self
  end
  def remove_var(v)
    App[K, self]
  end
  def inspect(need_paren = true)
    type.to_s
  end
  def to_unlambda
    type.to_s.downcase
  end
  def simplify
    self
  end
end

def ary2app(exp)
  if exp.is_a?(Array)
    exp = exp.map {|e| ary2app(e) }
    e1 = App[*exp.shift(2)]
    exp.each {|e2| e1 = App[e1, e2] }
    e1
  elsif exp.is_a?(Abs)
    Abs[exp.var, ary2app(exp.exp)]
  else
    exp
  end
end

def to_unlambda(exp)
  ary2app(exp).remove_abs.simplify.to_unlambda
end

def abs(&b)
  vars = b.parameters.map {|type, name| name }
  exp = yield(*vars.map {|name| Var[name] })
  vars.reverse_each {|v| exp = Abs[v, exp] }
  exp
end

Zero = abs {|s, z| z }
One = abs {|s, z| [s, z] }
Two = abs {|s, z| [s, [s, z]] }
Three = abs {|s, z| [s, [s, [s, z]]] }
Six = abs {|s| [Two, [Three, s]] }
N256 = [abs {|n| [n, n] }, [abs {|n| [n, n] }, abs {|s, z| [s, [s, z]] }]]
Inc = abs {|n, s, z| [s, [[n, s], z]] }
Dbl = abs {|n, s| [n, abs {|x| [s, [s, x]] }] }
Cons = abs {|hd, tl| abs {|f| [f, hd, tl] } }
Pow = abs {|n, m| [m, n] }
IfLE6 = -> m, t, e { [[m, Pow, [K, t]], [Six, Pow, [K, e]]] }
Main =
  [
    abs {|x| [x, x, Zero, Zero] },
    abs {|main, count, num, code|
      IfLE6[count,
       [main, main, [Inc, count], [code, I, Inc, [Dbl, num]]],
       [Cons, num, code]
      ]
    }
  ]

if ARGV[0]
  puts "k`"
  "foobar\n".bytes do |c|
    print "``s" * 8 + "i"
    6.downto(0) {|j| print "`" + "kki"[c[j], 2] }
    puts
  end
  puts "`k`k" + to_unlambda(N256) + to_unlambda(Main)
else
  File.write("lazyk-boot.dat", "`k`k" + to_unlambda(N256) + to_unlambda(Main))
end
