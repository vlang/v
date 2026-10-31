# pe

`pe` parses Windows Portable Executable (PE) files, similar to Go's
`debug/pe`.

```v
import debug.pe

data := os.read_file('app.exe')!
f := pe.new_file(data.bytes())!
println(f.machine)
for s in f.sections {
    println(s.name)
}
```
