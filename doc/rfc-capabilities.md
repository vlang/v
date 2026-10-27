# RFC: Compile-Time Capability Declarations

Status: RFC, docs only. No existing `@[requires]`/`@[grants]`/
`@[capability]` system in tree (only unrelated `gg.Capabilities`,
MCP, Win32 uses). Reuses the `@[noalloc]` analysis pattern.

## Syntax (illustrative)

```v okfmt
import os

@[capability: 'fs.read']
fn os.read_file(path string) !string

@[requires: 'fs.read']
fn load_config(path string) !Config {
	contents := os.read_file(path)!
	return parse_config(contents)
}

@[grants: 'fs.read']
fn main() {
	config := load_config('app.toml')!
	println(config.host)
}
```

## Rules

1. Closed registry (`fs.read`, `fs.write`, `net.connect`,
   `process.spawn`, `env.read`, `unsafe.ffi`). Typos are errors.
2. Requirements propagate transitively through statically known
   calls, including generic instantiations.
3. Entry point (`main`) declares grants; missing grants fail with
   a dependency path (`main -> ... -> http.get`).
4. Unknown/indirect/interface calls and unannotated `C.*` require
   `unsafe.ffi` (fail closed).
5. Optional machine-readable manifest (`-capability-manifest out.json`).

## Security boundary

Static audit only, not a sandbox. A wrong trusted annotation on
native code is not caught. OS enforcement needs separate mechanisms.

## Scope and acceptance

Phase 1: schema, propagation, diagnostics. Phase 2: stdlib
annotations for the initial capability set. Phase 3: manifest flag.
Missing grants, unknown foreign calls, and aggregation paths are
tested; unannotated programs keep current behavior.