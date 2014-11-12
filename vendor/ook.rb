RE = /\G(?:\s*Ook(?<c1>.)\s*Ook(?<c2>.)|(?<eof>)\s*\z|(?<error>))/

T = {
  ".?" => "a[i+=1]||=0;",
  "?." => "a[i-=1]||=0;",
  ".." => "a[i]+=1;",
  "!!" => "a[i]-=1;",
  ".!" => "a[i]=$stdin.getc;",
  "!." => "putc(a[i]);",
  "!?" => "while(a[i]>0);",
  "?!" => "end;",
}

c = "a=[i=0];"
File.read($*[0]).scan(RE) do
  if $~[:c1]
    c << T[$~[:c1] + $~[:c2]]
  elsif $~[:error]
    raise "Ook? Ook? Ook?"
  end
end
eval c
