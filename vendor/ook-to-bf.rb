RE = /\G(?:\s*Ook(?<c1>.)\s*Ook(?<c2>.)|(?<eof>)\s*\z|(?<error>))/

T = {
  ".?" => ">",
  "?." => "<",
  ".." => "+",
  "!!" => "-",
  ".!" => ",",
  "!." => ".",
  "!?" => "[",
  "?!" => "]",
}

if $*.size != 2
  $stderr.puts "usage: #$0 in.ook out.bf"
  exit 1
end

c = ""
File.read($*[0]).scan(RE) do
  if $~[:c1]
    c << T[$~[:c1] + $~[:c2]]
  elsif $~[:error]
    raise "Ook? Ook? Ook?"
  end
end
File.write($*[1], c)
