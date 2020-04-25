# https://aheui.readthedocs.io/en/latest/specs.en.html

code = $<.each_line.map do |s|
  s.chomp.unpack("U*").map do |c|
    if 0xac00 <= c && c <= 0xd7a3
      c -= 0xac00
      # Hangul initial consonant, vowel, final consonant
      [c / 21 / 28, c / 28 % 21, c % 28]
    end
  end
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

STROKE_TABLE = [0,2,4,4,2,5,5,3,5,7,9,9,7,9,9,8,4,4,6,2,4,nil,3,4,3,4,4,nil]

x, y, dx, dy, s = 0, 0, 0, 1, 0
ss = (0..27).map { [] }
def (ss[21]).pop; shift; end # queue
ss[27] = nil # extension protocol

check_size = -> n do
  if ss[s].size >= n
    true
  else
    dx, dy = -dx, -dy
    false
  end
end

while true
  ic, vo, fc = code[y][x]

  case ic
  when 11 # null
  when 18 then exit # terminate
  when  3 then check_size[2] && (a, b = ss[s].pop, ss[s].pop; ss[s] << b + a)
  when  4 then check_size[2] && (a, b = ss[s].pop, ss[s].pop; ss[s] << b * a)
  when  2 then check_size[2] && (a, b = ss[s].pop, ss[s].pop; ss[s] << b / a)
  when 16 then check_size[2] && (a, b = ss[s].pop, ss[s].pop; ss[s] << b - a)
  when  5 then check_size[2] && (a, b = ss[s].pop, ss[s].pop; ss[s] << b % a)
  when 6 # pop
    if check_size[1]
      v = ss[s].pop
      case fc
      when 21 then puts v
      when 27 then print v.chr("UTF-8")
      end
    end
  when 7 # push
    case fc
    when 21 then ss[s] << read_int
    when 27 then ss[s] << $stdin.getc.ord
    else ss[s] << STROKE_TABLE[fc]
    end
  when 8 then check_size[1] && ss[s] << ss[s][s != 21 ? -1 : 0] # duplicate
  when 17 # swap
    if check_size[2]
      r = s != 21 ? (-2..-1) : (0..1)
      ss[s][r] = ss[s][r].reverse
    end
  when 9 then s = fc # select
  when 10 then check_size[1] && ss[fc] << ss[s].pop # transfer
  when 12 # compare
    check_size[2] && ss[fc] << (ss[s].pop <= ss[s].pop ? 1 : 0)
  when 14 # fork
    dx, dy = -dx, -dy if check_size[1] && ss[fc].pop != 0
  end

  case vo
  when  0 then dx, dy =  1,  0
  when  2 then dx, dy =  2,  0
  when  4 then dx, dy = -1,  0
  when  6 then dx, dy = -2,  0
  when  8 then dx, dy =  0, -1
  when 12 then dx, dy =  0, -2
  when 13 then dx, dy =  0,  1
  when 17 then dx, dy =  0,  2
  when 18 then dy = -dy
  when 19 then dx, dy = -dx, -dy
  when 20 then dx = -dx
  end

  x, y = x + dx, y + dy
end
