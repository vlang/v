The `flat` module stores compact syntax tree nodes for the compiler.

`Node.typ` stores a `u32` source-spelling identity. `Node.typ_text()` resolves the spelling;
`Node.set_type_text()` indexes a new annotation. These spellings preserve unresolved generics,
nominal aliases, and parser metadata. They differ from a checker's semantic `types.TypeId`.
Resolving an unqualified spelling still requires its module and import context.

`source_type_id` and `source_type_name` cross the spelling boundary. Zero denotes empty text.
`source_type_shape` reads syntax flags recorded when a spelling is interned, allowing
type caches to inspect wrapper, container, qualified-name, and `typeof` syntax by ID.
`FlatAst.type_text_id` and `node_type_text_id` use this same `u32` namespace, independently
of the AST table used for ordinary text payloads.
The process owns immutable spellings in nonmoving chunks. Insertion is synchronized; lookups of
existing spellings and IDs observe published entries without taking the insertion lock. Index
growth publishes a new generation and retains earlier generations until process exit so readers
can finish their probes safely. Geometric growth retains less than twice the latest slot allocation.
IDs remain valid across workers and disposable arenas, so copying a node requires no type-string
promotion or ID remapping.
The table retains unique spellings until process exit; IDs are not serialized across processes.

`TypeTextTable` provides a private lookup cache over these shared identities. `TypeTextBindings`
and `NodeTypeTextBindings` store binding values as indexes. `get` resolves a present binding,
including one whose spelling is empty. `type_id` and `set_id` pass an index without formatting it.
`value_id` returns an index directly, using zero for an empty or absent binding. Use `type_id`
or `contains` when those two states need to be distinguished.
`clone` copies lexical bindings while sharing their lookup cache. `fork` copies both bindings
and the private cache for a worker; immutable IDs can be copied directly between workers.
Do not concurrently mutate the same binding map or private lookup cache.

`TypeTextList` stores an ordered sequence of IDs. `at` resolves one element without creating
an intermediate string array; `names` resolves the sequence for a consumer that accepts text.
Numeric copies preserve identities without cloning spelling storage. Generic constraints in
`NodePayload` also use this list; `Node.generic_constraints()` supplies text to syntax consumers.

`TypeTextListBindings` stores ordered lists of source IDs. `set_ids` copies a caller's list;
`type_ids` provides its numeric values for read-only use. `set` and `get` are spelling boundaries.
Cloning copies both the map and numeric lists, preserving independent mutation in workers.
`clone_owned` on either binding container also copies declaration keys into the current arena,
so compiler stages can retire a temporary arena without retaining its key storage.

`canonical_comptime_type_payload` is a reserved node payload id for a reflection source whose
type already identifies its declaring module. Generic specialization uses it to preserve that
identity through later import resolution. It requires no allocation, `Node.clone_owned` keeps it,
and `node_payload_at` returns a static empty payload for this marker. It carries no generic
parameters or constraints.
