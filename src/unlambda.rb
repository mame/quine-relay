tokens = File.read($*[0])

stack = [nil, :S]
acc = nil
while stack
  stack, cmd = stack
  acc = if cmd == :S
    stack = [stack, acc] if acc
    case tokens.slice!(0, 1)
    when ?. then [:o, tokens.slice!(0, 1)]
    when ?r then [:o, "\n"]
    when ?` then stack = [[stack, :S], :S]; nil
    when ?i then [:i]
    when ?k then [:k]
    when ?s then [:s]
    when ?c then [:c]
    when ?d then [:d]
    when ?# then tokens.slice!(/.*/); stack = [stack, :S]; nil
    when nil then raise "EOF"
    else stack = [stack, :S]; nil
    end
  else
    [:a, cmd, acc]
  end
end

stack = [nil, :E, acc]
acc = nil
while stack
  stack, cmd, cls = stack
  acc = if cmd == :E
    if acc && acc[0] == :d
      [:d1, cls]
    else
      stack = [stack, :A, acc] if acc
      cls[0] == :a ? (stack = [[stack, :E, cls[2]], :E, cls[1]]; nil) : cls
    end
  else
    type, x, y = cls
    case type
    when :o  then print x; acc
    when :i  then acc
    when :k  then [:k1, acc]
    when :k1 then x
    when :s  then [:s1, acc]
    when :s1 then [:s2, x, acc]
    when :s2 then stack = [stack, :E, [:a, [:a, x, acc], [:a, y, acc]]]; nil
    when :c  then stack = [stack, :A, acc]; [:c1, stack[0]]
    when :c1 then stack = x; acc
    when :d  then acc
    when :d1 then stack = [stack, :E, [:a, x, acc]]; nil
    end
  end
end
