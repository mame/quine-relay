require_relative "code-gen"
require "cairo"
require "rsvg2"

# You need to install the two fonts: Raleway and UnifrakturCook.
# * https://www.google.com/fonts/specimen/Raleway
# * https://www.google.com/fonts/specimen/UnifrakturCook

W = H = 750
surface = Cairo::ImageSurface.new(W, H)
ctx = Cairo::Context.new(surface)

ctx.line_width = 1
ctx.set_source_rgb(1, 1, 1)
ctx.rectangle(0, 0, W, H)
ctx.fill
ctx.set_source_rgb(0, 0, 0)

ctx.translate(W / 2, H / 2)
ctx.select_font_face("Raleway")
ctx.antialias = Cairo::Antialias::GRAY

BaseFontSize = 13
SubFontSize = 10

Radius = 210.0
RunSteps.each_with_index do |s, i|
  ctx.save do
    angle = i / (RunSteps.size / 4.0)
    dir = 1 < angle && angle <= 3 ? 1 : -1
    ctx.rotate(Math::PI / 2 * (dir < 0 ? -angle : 2 - angle + 0.02))
    s.name =~ /^(.*?)(\(.*\))?$/
    name, sub_name = $1, $2 || ""
    ctx.font_size = BaseFontSize; e1 = ctx.text_extents(name)
    ctx.font_size = SubFontSize ; e2 = ctx.text_extents(sub_name)
    ctx.move_to(dir * (Radius + 10) - (dir < 0 ? e1.x_advance + e2.x_advance : 0), 0)
    ctx.font_size = BaseFontSize; ctx.show_text(name)
    ctx.font_size = SubFontSize ; ctx.show_text(sub_name)
  end
end

ArrowCount = 7
ArrowCount.times do |i|
  ctx.save do
    ctx.rotate(Math::PI * (2 * i) / ArrowCount - Math::PI + 0.1)

    ctx.line_width = 2
    ctx.line_cap = Cairo::LineCap::SQUARE
    ctx.new_path
    ctx.arc(0, 0, Radius, 0.0, Math::PI * 2 / ArrowCount - 0.1)
    ctx.stroke

    ctx.line_width = 1
    ctx.move_to(Radius    , -8)
    ctx.line_to(Radius - 6,  2)
    ctx.line_to(Radius + 6,  2)
    ctx.fill
  end
end

Uroboros = 350.0
ctx.select_font_face("UnifrakturCook")
ctx.font_size = 25
Title = "#{ RunSteps.size }-Language Uroboros Quine"
e = ctx.text_extents(Title)
height = Uroboros / 2 + e.height + 20
ctx.move_to(-e.width / 2, height / 2)
ctx.show_text(Title)
ctx.save do
  svg = RSVG::Handle.new_from_file("uroboros.svg")
  ctx.translate(-Uroboros / 2, -height / 2)
  ctx.scale(Uroboros / svg.width, Uroboros / svg.width)
  ctx.render_rsvg_handle(svg)
end

surface.write_to_png("../langs.png")
unless ENV["NO_PNG_OPT"]
  system 'optipng -fix -i0 -o7 -strip all ../langs.png'
  system 'advdef -z4 ../langs.png'
  system 'advpng -z4 ../langs.png'
end

# convert svg to template
#
#require "cairo"
#require "rsvg2"
#H = 52.0
#W = H * 4
#surface = Cairo::ImageSurface.new(W, H)
#ctx = Cairo::Context.new(surface)
#svg = RSVG::Handle.new_from_file("uroboros2.svg")
#ctx.translate(4, 2)
#ctx.rotate(-0.014)
#ctx.scale((W - 8) / svg.width, (H - 4) / svg.height)
#ctx.render_rsvg_handle(svg)
#data = surface.data.unpack("C*")
#M = (0...H).map do |y|
#  (0...W).map do |x|
#    idx = y * surface.stride + x * 4
#    data[idx] < 40 && data[idx + 3] > 48 ? " " : "#"
#  end.join
#end
#puts *M
