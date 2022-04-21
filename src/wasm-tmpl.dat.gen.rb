def f(s)
  enc = -> n { [n].pack("V").unpack1("H*").gsub(/../){"\\"+$&} }
  esc = -> s { s.gsub(?\\){'\\\\'}.gsub(?"){'\\"'} }

  wasm_txt_pre = <<-'END'.lines.map {|s| s.strip }.join + enc[s.size * 16 + 3]
    (module
      (import"wasi_snapshot_preview1""fd_write"(func(param i32 i32 i32 i32)(result i32)))
      (memory(export"memory")(data "\08\00\00\00
  END

  raise unless wasm_txt_pre.size % 4 == 0

  wasm_txt_post = <<-'END'.lines.map {|s| s.strip }.join
      "))
      (func(export"_start")i32.const 1 i32.const 0 i32.const 1 i32.const 0 call 0 drop)
    )
  END

  txt = <<-END
    (module
      (import"wasi_snapshot_preview1""fd_write"(func $fd_write(param i32 i32 i32 i32)(result i32)))
      (memory 64)
      (export"memory"(memory 0))
      (data(i32.const 0)"#{
        enc[8*5]+enc[1] # " "
      }#{
        enc[8*5+4]+enc[2] # "\t"
      }#{
        enc[8*5+8]+enc[2] # "\n"
      }#{
        enc[8*5+12]+enc[wasm_txt_pre.size]
      }#{
        enc[8*5+12+wasm_txt_pre.size]+enc[wasm_txt_post.size]
      } ...\\\\t..\\\\n..#{ esc[wasm_txt_pre] }#{ esc[wasm_txt_post] }#{ s }")
      (func $out(param i32)
        i32.const 1
        local.get 0
        i32.const 8
        i32.mul
        i32.const 1
        i32.const #{ 8 * 5 + 12 }
        call $fd_write
        drop
      )
      (func(export"_start")
        (local $idx i32)
        (local $shift i32)

        i32.const 3
        call $out

        i32.const #{ 8*5+12+wasm_txt_pre.size+wasm_txt_post.size }
        local.set $idx
        (loop
          i32.const 0 call $out (; out << 32 ;)
          i32.const 0 call $out (; out << 32 ;)
          i32.const 0 call $out (; out << 32 ;)

          i32.const 8
          local.set $shift
          (loop
            local.get $idx
            i32.load8_u
            local.get $shift
            i32.const 1
            i32.sub
            local.tee $shift
            i32.shr_u
            i32.const 1
            i32.and
            call $out (; out << 32 - c[7-i] * 23 ;)

            local.get $shift
            br_if 0
          )
          i32.const 2 call $out (; out << 10 ;)
          i32.const 1 call $out (; out <<  9 ;)
          i32.const 2 call $out (; out << 10 ;)
          i32.const 0 call $out (; out << 32 ;)
          i32.const 0 call $out (; out << 32 ;)

          local.get $idx
          i32.const 1
          i32.add
          local.tee $idx
          i32.load8_u
          br_if 0
        )
        i32.const 2 call $out (; out << 10 ;)
        i32.const 2 call $out (; out << 10 ;)
        i32.const 2 call $out (; out << 10 ;)

        i32.const 4
        call $out
      )
    )
  END
end

File.write("ABCD.wat", f("abcd"))
File.write("ABCDE.wat", f("abcde"))
system("wat2wasm ABCD.wat", exception: true)
system("wat2wasm ABCDE.wat", exception: true)
abcd = File.binread("ABCD.wasm")
abcde = File.binread("ABCDE.wasm")
i = (0..).find {|i| abcd[i] != abcde[i] }
j = (abcd.size - 4).downto(0).find {|i| abcd[i] != abcde[i] }

data1 = abcd[0, i]
raise unless data1 == abcde[0, i]
data2 = abcd[i + 2, 5]
raise unless data2 == abcde[i + 2, 5]
data3 = abcd[i + 2 + 5 + 2...j-1]
raise unless data3 == abcde[i + 2 + 5 + 2...j-1]
data4 = abcd[j + 12-1...-4]

# wasm template:
#   data1 + LSB128(length+const) + data2 + LSB128(length+const) + data3 + Hexdump(length) + data4


A = [26, 34, 85, 127, 144, 153, 196]
def e(data)
  enc = "".b
  data.bytes do |n|
    case
    when n < 0x1a
      enc << [n + ?B.ord].pack("C*")
    when n < 32 || n == ?".ord || (?B.ord <= n && n <= ?Z.ord) || n >= 127
      enc << [?9.ord + A.index(n)].pack("C*")
    else
      enc << n
    end
  end
  enc
end

def d(data)
  data.gsub(/./) { "9" <= $& && $& < "@" ? [A[$&.ord - ?9.ord]].pack("C*") : "A" < $& && $& < "Z" ? [$&.ord - ?B.ord].pack("C*") : $& }
end

[data1, data2, data3, data4].each do |data|
  raise unless data == d(e(data))
end

File.open("wasm-tmpl.dat", "w") do |f|
  f.puts A.join(",")
  f.puts e(data1)
  f.puts e(data2)
  f.puts e(data3)
  f.puts e(data4)
end

out = File.read("../QR.xslt")
d=->s,t=?"{s.gsub(t){t+t}};
out = "ABCDE"

# test code
puts <<END.lines.map {|s| s.strip }.join(":")
Module QR
  Sub Main()
    Dim c,n,s As Object=System.Console.OpenStandardOutput(),t()As Short={#{A.join(",")}}
    For Each c in"#{e(data1)}}#{e(data2)}~#{e(data3)}$#{e(data4)}"
      c=Asc(c)
      If c=36
        For c=0To 11
          s.WriteByte(If(c Mod 3,Asc(#{out.size*16+3}.ToString("x8")(1Xor 7-c*2\\3)),92))
        Next
      Else
        n=(c>124)*(#{6+((287+out.size).bit_length-1)/7}*c-#{((287+out.size).bit_length-1)/7+287+out.size+125*(6+((287+out.size).bit_length-1)/7)})
        Do While n>127
          s.WriteByte(128+(127And n))
          n\\=128
        Loop
        s.WriteByte(If(c<125,If((c-1)\\7-8,c+66*(c>65And c<91),t(c-57)),n))
      End If
    Next
    For Each c in"#{d[out]}"
      s.WriteByte(Asc(c))
    Next
  End Sub
End Module
END
