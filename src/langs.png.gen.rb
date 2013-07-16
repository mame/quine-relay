require_relative "code-gen"
require "cairo"
require "rsvg2"

W = H = 512
surface = Cairo::ImageSurface.new(W, H)
ctx = Cairo::Context.new(surface)

langs = CodeGen::List.reverse.flat_map {|c| c.steps.map {|step| step.name } }

ctx.line_width = 1
ctx.set_source_rgb(255, 255, 255)
ctx.rectangle(0, 0, W, H)
ctx.fill
ctx.set_source_rgb(0, 0, 0)

ctx.translate(W / 2, H / 2)
ctx.font_size = 15
ctx.select_font_face("non-serif")

Radius = 210.0
langs.size.times do |i|
  ctx.save do
    ctx.rotate(Math::PI * (2 * i + 1) / langs.size)
    ctx.move_to(-Radius    ,  4)
    ctx.line_to(-Radius - 5, -2)
    ctx.line_to(-Radius + 5, -2)
    ctx.fill
  end

  ctx.save do
    a = langs.size / 2.0
    b = (a - 1) / 2
    ctx.rotate(-Math::PI * ((i + b) % a - b) / a)

    name = langs[i]
    e = ctx.text_extents(name)
    x = (i / a).round == 1 ? Radius : -Radius
    ctx.move_to(x - e.width / 2, e.height / 2)
    ctx.show_text(name)
  end
end

Uroboros = 240.0
ctx.save do
  svg = RSVG::Handle.new_from_file("uroboros.svg")
  ctx.translate(-Uroboros / 2, -Uroboros / 2)
  ctx.scale(Uroboros / svg.width, Uroboros / svg.height)
  ctx.render_rsvg_handle(svg)
end

surface.write_to_png("../langs.png")

# convert svg to template
#
#H = 55.0
#W = H * 2
#surface = Cairo::ImageSurface.new(W, H)
#ctx = Cairo::Context.new(surface)
#svg = RSVG::Handle.new_from_file("uroboros.svg")
#ctx.scale(W / svg.width, H / svg.height)
#ctx.render_rsvg_handle(svg)
#data = surface.data.unpack("C*")
#M = (0...H).map do |y|
#  (0...W).map do |x|
#    data[y * surface.stride + x * 4 + 3] > 48 ? " " : "#"
#  end.join
#end
#puts *M
