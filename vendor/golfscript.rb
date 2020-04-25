#!/usr/bin/ruby
#(c) Copyright 2008 Darren Smith. All Rights Reserved.
$lb = []
class Gtype
	def go
		$stack<<self
	end
	def val
		@val
	end
	
	'+-|&^'.each_byte{|i|
		eval'def %c(rhs)
			if rhs.class != self.class
				a,b=coerce(rhs)
				a %c b
			else
				factory(@val %c rhs.val)
			end
		end'%([i]*3)
	}
	def ==(rhs)
		@val==rhs.val
	end
	def eql?(rhs)
		@val==rhs.val
	end
	def hash
		@val.hash
	end
	def <=>(rhs)
		@val<=>rhs.val
	end
end

class Gint < Gtype
	def initialize(i)
		@val = case i
			when true then 1
			when false then 0
			else;i
		end
	end
	def factory(a)
		Gint.new(a)
	end
	def to_gs
		Gstring.new(@val.to_s)
	end
	def to_int #for pack
		@val
	end
	def ginspect
		to_gs
	end
	def class_id; 0; end
	def coerce(b)
		[if b.class == Garray
			Garray.new([self])
		elsif b.class == Gstring
			to_gs
		else #Gblock
			to_gs.to_s.compile
		end,b]
	end
	
	def ~
		Gint.new(~@val)
	end
	def notop
		Gint.new(@val == 0)
	end
	'*/%<>'.each_byte{|i|
		eval'def %c(rhs)
			Gint.new(@val %c rhs.val)
		end'%[i,i]
	}
	def equalop(rhs)
		Gint.new(@val == rhs.val)
	end
	def question(b)
		Gint.new(@val**b.val)
	end
	def base(a)
		if Garray===a
			r=0
			a.val.each{|i|
				r*=@val
				r+=i.val
			}
			Gint.new(r)
		else
			i=a.val.abs
			r=[]
			while i!=0
				r.unshift Gint.new(i%@val)
				i/=@val
			end
			Garray.new(r)
		end
	end
	def leftparen
		Gint.new(@val-1)
	end
	def rightparen
		Gint.new(@val+1)
	end
end

class Garray < Gtype
	def initialize(a)
		@val = a || []
	end
	def factory(a)
		Garray.new(a)
	end
	def to_gs
		@val.inject(Gstring.new("")){|s,i|s+i.to_gs}
	end
	def flatten #maybe name to_a ?		
# 		Garray.new(@val.inject([]){|s,i|s+case i
# 			when Gstring then i.val
# 			when Gint then [i]
# 			when Garray then i.flatten.val
# 			when Gblock then i.val
# 			end
# 		})
# 	end
		#use Peter Taylor's fix to avoid quadratic flatten times
		Garray.new(flatten_append([]))
	end
	def flatten_append(prefix)
		@val.inject(prefix){|s,i|case i
			when Gint then s<<i
			when Garray then i.flatten_append(s)
			when Gstring then s.concat(i.val)
			when Gblock then s.concat(i.val)
 			end
		}
	end
	def ginspect
		Gstring.new('[')+Garray.new(@val.map{|i|i.ginspect})*Gstring.new(' ')+Gstring.new(']')
	end
	def go
		$stack<<self
	end
	def class_id; 1; end
	def coerce(b)
		if b.class == Gint
			b.coerce(self).reverse
		elsif b.class == Gstring
			[Gstring.new(self),b]
		else
			[(self*Gstring.new(' ')).to_s.compile,b]
		end
	end
	
	def leftparen
		[factory(@val[1..-1]),@val[0]]
	end
	def rightparen
		[factory(@val[0..-2]),@val[-1]]
	end
	def *(b)
		if b.class == Gint
			factory(@val*b.val)
		else
			return b*self if self.class == Gstring && b.class == Garray
			return self/Gint.new(1)*b if self.class == Gstring
			return b.factory([]) if @val.size<1
			r=@val.first
			r,x=r.coerce(b) if r.class != b.class #for size 1
			@val[1..-1].each{|i|r=r+b+i}
			r
		end
	end
	def /(b)
		if b.class == Gint
			r=[]
			a = b.val < 0 ? @val.reverse : @val
			i = -b = b.val.abs
			r << factory(a[i,b]) while (i+=b)<a.size
			Garray.new(r)
		else
			r=[]
			i=b.factory([])
			j=0
			while j<@val.size
				if @val[j,b.val.size]==b.val
					r<<i
					i=b.factory([])
					j+=b.val.size
				else
					i.val<<@val[j]
					j+=1
				end
			end
			r<<i
			Garray.new(r)
		end
	end
	def %(b)
		if b.class == Gint
			b=b.val
			factory((0..(@val.size-1)/b.abs).inject([]){|s,i|
				s<<@val[b < 0 ? i*b - 1 : i*b]
			})
		else
			self/b-Garray.new([Garray.new([])])
		end
	end
	def notop
		Gint.new(@val.empty?)
	end
	def question(b)
		Gint.new(@val.index(b)||-1)
	end
	def equalop(b)
		if b.class == Gint
			@val[b.val]
		else
			Gint.new(@val==b.val)
		end
	end
	def <(b)
		if b.class == Gint
			factory(@val[0...b.val])
		else
			Gint.new(@val<b.val)
		end
	end
	def >(b)
		if b.class == Gint
			factory(@val[[b.val,-@val.size].max..-1])
		else
			Gint.new(@val>b.val)
		end
	end
	def sort
		factory(@val.sort)
	end
	def zip
		r=[]
		@val.size.times{|x|
			@val[x].val.size.times{|y|
				(r[y]||=@val[0].factory([])).val<<@val[x].val[y]
			}
		}
		Garray.new(r)
	end
	def ~
		val
	end
end

class Gstring < Garray
	def initialize(a)
		@val=case a
			when NilClass then []
			when String then a.unpack('C*').map{|i|Gint.new(i)}
			when Array then a
			when Garray then a.flatten.val
		end
	end
	def factory(a)
		Gstring.new(a)
	end
	def to_gs
		self
	end
	def ginspect
		factory(to_s.inspect)
	end
	def to_s
		@val.pack('C*')
	end
	def class_id; 2; end
	def coerce(b)
		if b.class == Gblock
			[to_s.compile,b]
		else
			b.coerce(self).reverse
		end
	end
	def question(b)
		if b.class == Gstring
			Gint.new(to_s.index(b.to_s)||-1)
		elsif b.class == Garray
			b.question(self)
		else
			Gint.new(@val.index(b)||-1)
		end
	end
	def ~
		to_s.compile.go
		nil
	end
end

class Gblock < Garray
	def initialize(_a,_b=nil)
		@val=Gstring.new(_b).val
		@native = eval("lambda{#{_a}}")
	end
	def go
		@native.call
	end
	def factory(b)
		Gstring.new(b).to_s.compile
	end
	def class_id; 3; end
	def to_gs
		Gstring.new("{"+Gstring.new(@val).to_s+"}")
	end
	def ginspect
		to_gs
	end
	def coerce(b)
		b.coerce(self).reverse
	end
	
	def +(b)
		if b.class != self.class
			a,b=coerce(b)
			a+b
		else
			Gstring.new(@val+Gstring.new(" ").val+b.val).to_s.compile
		end
	end
	def *(b)
		if b.class == Gint
			b.val.times{go}
		else
			gpush b.val.first
			(b.val[1..-1]||[]).each{|i|$stack<<i; go}
		end
		nil
	end
	def /(b)
		if b.class==Garray||b.class==Gstring
			b.val.each{|i|gpush i; go}
			nil
		else #unfold
			r=[]
			loop{
				$stack<<$stack.last
				go
				break if gpop.notop.val!=0;
				r<<$stack.last
				b.go
			}
			gpop
			Garray.new(r)
		end
	end
	def %(b)
		r=[]
		b.val.each{|i|
			lb=$stack.size
			$stack<<i; go
			r.concat($stack.slice!(lb..$stack.size))
		}
		r=Garray.new(r)
		b.class == Gstring ? Gstring.new(r) : r
	end
	def ~
		go
		nil
	end
	def sort
		a=gpop
		a.factory(a.val.sort_by{|i|gpush i; go; gpop})
	end
	def select(a)
		a.factory(a.val.select{|i|gpush i;go; gpop.notop.val==0})
	end
	def question(b)
		b.val.find{|i|gpush i; go; gpop.notop.val==0}
	end
end

class NilClass
	def go
	end
end
class Array
	def ^(rhs)
		self-rhs|rhs-self
	end
	include Comparable
end
code=gets(nil)||''
$_=$stdin.isatty ? '' : $stdin.read
$stack = [Gstring.new($_)]
$var_lookup={}

def var(name,val=nil)
	eval"#{s="$_#{$var_lookup[name]||=$var_lookup.size}"}||=val"
	s
end

$nprocs=0

class String
	def compile(tokens=scan(/[a-zA-Z_][a-zA-Z0-9_]*|'(?:\\.|[^'])*'?|"(?:\\.|[^"])*"?|-?[0-9]+|#[^\n\r]*|./m))
	 	orig=tokens.dup
		native=""
		while t=tokens.slice!(0)
			native<<case t
				when "{" then "$stack<<"+var("{#{$nprocs+=1}",compile(tokens))
				when "}" then break
				when ":" then var(tokens.slice!(0))+"=$stack.last"
				when /^["']/ then var(t,Gstring.new(eval(t)))+".go"
				when /^-?[0-9]+/ then var(t,Gint.new(t.to_i))+".go"
				else; var(t)+".go"
				end+"\n"
		end
		source=orig[0,orig.size-tokens.size-(t=="}"?1:0)]*""
		Gblock.new(native,source)
	end
end
def gpop
	($lb.size-1).downto(0){|i|
		break if $lb[i]<$stack.size
		$lb[i]-=1
	}
	$stack.pop
end
def gpush a
	$stack.push(*a) if a
end

class String
	def cc
		Gblock.new(self)
	end
	def cc1
		('a=gpop;'+self).cc
	end
	def cc2
		('b=gpop;a=gpop;'+self).cc
	end
	def cc3
		('c=gpop;b=gpop;a=gpop;'+self).cc
	end
	def order
		('b=gpop;a=gpop;a,b=b,a if a.class_id<b.class_id;'+self).cc
	end
end

var'[','$lb<<$stack.size'.cc
var']','gpush Garray.new($stack.slice!(($lb.pop||0)..-1))'.cc
var'~','gpush ~a'.cc1
var'`','gpush a.ginspect'.cc1
var';',''.cc1
var'.','$stack<<a<<a'.cc1
var'\\','$stack<<b<<a'.cc2
var'@','$stack<<b<<c<<a'.cc3
var'+','gpush a+b'.cc2
var'-','gpush a-b'.cc2
var'|','gpush a|b'.cc2
var'&','gpush a&b'.cc2
var'^','gpush a^b'.cc2
var'*','gpush a*b'.order
var'/','gpush a/b'.order
var'%','gpush a%b'.order
var'=','gpush a.equalop(b)'.order
var'<','gpush a<b'.order
var'>','gpush a>b'.order
var'!','gpush a.notop'.cc1
var'?','gpush a.question(b)'.order
var'$','gpush (a.class==Gint ? $stack[~a.val] : a.sort)'.cc1
var',','gpush case a
	when Gint then Garray.new([*0...a.val].map{|i|Gint.new(i)})
	when Gblock then a.select(gpop)
	when Garray then Gint.new(a.val.size)
	end'.cc1
var')','gpush a.rightparen'.cc1
var'(','gpush a.leftparen'.cc1

var'rand','gpush Gint.new(rand([1,a.val].max))'.cc1
var'abs','gpush Gint.new(a.val.abs)'.cc1
var'print','print a.to_gs'.cc1
var'if',"#{var'!'}.go;(gpop.val==0?a:b).go".cc2
var'do',"loop{a.go; #{var'!'}.go; break if gpop.val!=0}".cc1
var'while',"loop{a.go; #{var'!'}.go; break if gpop.val!=0; b.go}".cc2
var'until',"loop{a.go; #{var'!'}.go; break if gpop.val==0; b.go}".cc2
var'zip','gpush a.zip'.cc1
var'base','gpush b.base(a)'.cc2

'"\n":n;
{print n print}:puts;
{`puts}:p;
{1$if}:and;
{1$\if}:or;
{\!!{!}*}:xor;
'.compile.go
code.compile.go
gpush Garray.new($stack)
'puts'.compile.go
