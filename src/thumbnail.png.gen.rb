require "chunky_png"

# zcat /usr/share/fonts/X11/misc/4x6.pcf.gz |
# pcf2bdf |
# ruby -e '$<.each("") {|s| (n = $1.to_i; puts "  %3d => 0x%s," % [n, $2.split.map {|n| n[0] }.join] if 32 <= n && n <= 126) if s =~ /ENCODING (\d+).*BITMAP(.*)ENDCHAR/m }'

FONT = {
   32 => 0x000000,  33 => 0x444040,  34 => 0xAA0000,  35 => 0xAFAFA0,
   36 => 0x4EC2E4,  37 => 0x824820,  38 => 0x4A4A50,  39 => 0x440000,
   40 => 0x244442,  41 => 0x844448,  42 => 0xA4E4A0,  43 => 0x44E440,
   44 => 0x000048,  45 => 0x00E000,  46 => 0x000040,  47 => 0x224880,
   48 => 0x4AEA40,  49 => 0x4C44E0,  50 => 0x4A24E0,  51 => 0xE242C0,
   52 => 0xAAE220,  53 => 0xE8C2C0,  54 => 0x68CA40,  55 => 0xE24880,
   56 => 0x6A4AC0,  57 => 0x4A62C0,  58 => 0x040040,  59 => 0x040048,
   60 => 0x248420,  61 => 0x0E0E00,  62 => 0x842480,  63 => 0xC24040,
   64 => 0x6AA860,  65 => 0x4AEAA0,  66 => 0xCACAC0,  67 => 0x4A8A40,
   68 => 0xCAAAC0,  69 => 0xE8C8E0,  70 => 0xE8C880,  71 => 0x68AA60,
   72 => 0xAAEAA0,  73 => 0xE444E0,  74 => 0x222A40,  75 => 0xAACAA0,
   76 => 0x8888E0,  77 => 0xAEEAA0,  78 => 0x2AEA80,  79 => 0x4AAA40,
   80 => 0xCAC880,  81 => 0x4AAA42,  82 => 0xCACAA0,  83 => 0x6842C0,
   84 => 0xE44440,  85 => 0xAAAAE0,  86 => 0xAAAE40,  87 => 0xAAEEA0,
   88 => 0xAA4AA0,  89 => 0xAA4440,  90 => 0xE248E0,  91 => 0x644460,
   92 => 0x884220,  93 => 0xC444C0,  94 => 0x4A0000,  95 => 0x00000E,
   96 => 0x420000,  97 => 0x06AA60,  98 => 0x8CAAC0,  99 => 0x068860,
  100 => 0x26AA60, 101 => 0x04AC60, 102 => 0x24E440, 103 => 0x06A62C,
  104 => 0x8CAAA0, 105 => 0x40C4E0, 106 => 0x20222C, 107 => 0x8ACAA0,
  108 => 0xC444E0, 109 => 0x0AEAA0, 110 => 0x0CAAA0, 111 => 0x04AA40,
  112 => 0x0CAC88, 113 => 0x06AA62, 114 => 0x0AC880, 115 => 0x06C2C0,
  116 => 0x4E4420, 117 => 0x0AAA60, 118 => 0x0AAA40, 119 => 0x0AAEA0,
  120 => 0x0A44A0, 121 => 0x0AA62C, 122 => 0x0E24E0, 123 => 0x24C442,
  124 => 0x444440, 125 => 0x846448, 126 => 0x5A0000
}

src = File.foreach("../QR.rb").to_a

w = src.map {|line| line.chomp.size }.max
h = src.size

png = ChunkyPNG::Image.new(w * 4, h * 8, :black)
src.each_with_index do |line, j|
  line.chomp.chars.each_with_index do |ch, i|
    6.times do |y|
      4.times do |x|
        png[i * 4 + 3 - x, j * 8 + y + 1] =
          [:black, :white][FONT[ch.ord][x + (5 - y) * 4]]
      end
    end
  end
end
png.save('../thumbnail.png')
