# wit

Pure V implementation of a WIT (WebAssembly Interface Types) parser and V binding generator.

## Usage

```v
import wit

fn main() {
    src := os.read_file('example.wit')!
    doc := wit.parse(src)!
    println(doc.to_v())
}
```

## Status

Experimental. Supports a useful subset of WIT: packages, interfaces, worlds,
records, variants, enums, flags, resources, type aliases, functions, and common
generic types (`list`, `option`, `result`, `tuple`, `own`, `borrow`).

Component model wrapping is available via `wit.wrap_component` and
`wasm.wrap_core_module` (experimental).