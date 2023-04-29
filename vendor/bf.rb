code = File.read($*[0]).unpack("C*")
data = [0]
i = j = 0
while code[i]
  case code[i]
  when 60 # <
    j -= 1
  when 62 # >
    j += 1
    data[j] ||= 0
  when 43 # +
    data[j] += 1
  when 45 # -
    data[j] -= 1
  when 91 # [
    if data[j] == 0
      d = 0
      i += 1
      until d == 0 && code[i] == 93 # ]
        case code[i]
        when 91 # [
          d += 1
        when 93 # ]
          d -= 1
        end
        i += 1
      end
    end
  when 93 # ]
    d = 0
    i -= 1
    until d == 0 && code[i] == 91 # [
      case code[i]
      when 91 # [
        d -= 1
      when 93 # ]
        d += 1
      end
      i -= 1
    end
    next
  when 46 # .
    putc(data[j])
  when 44 # ,
    data[j] = $stdin.getc
  end
  i += 1
end
