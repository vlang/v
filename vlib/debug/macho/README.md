# macho

`macho` parses 64-bit Mach-O files, similar to Go's `debug/macho`.

```v
import debug.macho

data := os.read_file('app')!
f := macho.new_file(data.bytes())!
println(f.header.cpu_type)
for s in f.sections {
    println(s.name)
}
```
