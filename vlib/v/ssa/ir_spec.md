# SSA IR surface and text format

This documents the `v.ssa` intermediate representation: the data model in
`ssa.v`, the text format in `ir_text.v`, and what the format covers.

## Data model (`ssa.v`)

- IDs are plain `int` aliases: `ValueID`, `TypeID`, `BlockID`.
- `OpCode` (~90 ops): terminators, integer/float arithmetic, bitwise,
  memory (`alloca`, `load`, `store`, `get_element_ptr`, atomics),
  comparisons, casts, `call` family, `phi`/`select`, `inline_asm`,
  concurrency (`go_call`, `spawn_call`), aggregates.
- `TypeStore` interns types (`get_int`, `get_ptr`, ...); `%t0` is void.
- `Module` holds values, instructions, blocks, functions, globals, plus
  V-specific side tables (C struct names, export maps) and volatile
  analysis state (use-lists, CFG edges, dominator info, layout caches).
- Operand roles are centralized: `is_block_operand`,
  `is_value_operand`, `is_definition_operand`, `is_successor_operand`.

## Text format (`ir_text.v`, version `ir_version`)

`module_to_text` prints a module; `module_from_text` parses it back.
Text IDs are scoped to the text. The printer emits:

1. `vir <version>` header and `target` line.
2. Types in array order, values (constants, funcrefs, string literals,
   detached `phi_result`s) in ID order, globals, functions with blocks
   and instructions, a `detached` section for instructions no block
   references anymore (dead code the optimizer removed from blocks but
   left in the values array), `meta` side tables, `asm_spec` blocks.
3. Every instruction carries its value ID (`%vN = <op> ...`), so the
   parser rebuilds the exact value/instruction/block arrays.

Deliberately not serialized (recomputed, not semantic):

- Source positions (`token.Pos`), layout caches, `type_layout_frozen`.
- Use-lists (rebuilt from operand roles), CFG edges (rebuilt from
  terminators), dominator info (recomputed by the optimizer).
- `const_cache`/`func_index` (rebuilt while parsing).

Round-trip guarantee: `print(parse(print(m))) == print(m)`, and the
parsed module passes the structured verifier (`optimize.verify`) when
the original does.

Notes:

- `global` lines carry the element type (`GlobalVar.typ`); the global
  SSA value holds a pointer to it, restored on parse.
- Types keep explicit identities: every entry is registered verbatim,
  and the interning cache is rebuilt first-ID-wins, so distinct
  registered types with one shape stay distinct.

## Conventions for frontends

- All instruction operands are IDs (`%vN` values, `@bN` blocks).
- Constants are text: the constant's payload is its name (`"257"`).
- V runtime layouts (str/array/map) are ordinary structs here; the
  text format has no V-specific types or ops.
- Optional extensions (`go_call`, `spawn_call`, `inline_string_init`)
  parse and print like any other op; generic consumers may reject them.
