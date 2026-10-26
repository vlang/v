The `flat` module stores compact syntax tree nodes for the compiler.

`Node.typ` stores a `u32` source-spelling identity. `Node.typ_text()` resolves the spelling;
`Node.set_type_text()` indexes a new annotation. These spellings preserve unresolved generics,
nominal aliases, and parser metadata. They differ from a checker's semantic `types.TypeId`.
Resolving an unqualified spelling still requires its module and import context.

`source_type_id` and `source_type_name` cross the spelling boundary. Zero denotes empty text.
`source_type_shape` reads exact spelling syntax flags recorded once when a type is interned.
These probes describe raw prefixes and punctuation; they do not resolve aliases or strip wrappers.
Type caches can inspect wrappers, containers, qualification, endpoint whitespace,
and `typeof` syntax by ID.
Builtin `source_type_*` constants carry scalar spellings without repeated interning;
the `builtin_alias` shape flag identifies spellings that need no alias normalization.
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
`Node.generic_constraint_ids()` borrows their ordered numeric identities for read-only consumers.
Enum backing annotations use the scalar `NodePayload.enum_backing_type` source ID.
`Node.enum_backing_type_id()` reads it and `set_enum_backing_type_id()` replaces it while preserving
other metadata. Generic name/attribute lists contain markers such as `json_as_number`, independently
of enum storage types. Payload replacement, owned cloning, and arena promotion retain the ID;
source formatters and stable cache signatures resolve its spelling only when rendering text.
Debugger child-expression source types use `NodePayload.debugger_value_types` independently of
binding names and display text. `Node.debugger_value_type_ids()` borrows the ordered immutable IDs;
`set_debugger_value_type_ids()` and `node_payload_with_debugger_types()` copy caller ID arrays.
`node_payload_with_type_metadata()` preserves both typed metadata slots in syntax clones.

`TypeTextListBindings` stores ordered lists of source IDs. `set_ids` copies a caller's list;
`append_ids` copies additional entries, preserving their order and empty spellings.
`type_ids` provides its numeric values for read-only use. `set` and `get` are spelling boundaries.
Cloning copies both the map and numeric lists, preserving independent mutation in workers.
`clone_owned` on either binding container also copies declaration keys into the current arena,
so compiler stages can retire a temporary arena without retaining its key storage.

`canonical_comptime_type_payload` is a reserved node payload id for a reflection source whose
type already identifies its declaring module. Generic specialization uses it to preserve that
identity through later import resolution. It requires no allocation, `Node.clone_owned` keeps it,
and `node_payload_at` returns a static empty payload for this marker. It carries no generic
parameters or constraints.

`scan_listed_symlinks` records which entries of one directory are symbolic links.
Pass its `ListedSymlinks` result to `record_listed_source_path_in` with that directory's
resolved path to avoid a separate metadata query for each regular entry. An unknown listing
falls back to `record_listed_source_path`; a frozen source-path table remains read-only.
`discard_replaced_storage` may discard old AST buffers only after every reader uses the replacement.
