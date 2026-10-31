# Printing recursive sum types

Automatic printing follows distinct objects in recursive sum-type trees. Repeated payload
types alone do not count as circular references. Nested directories in a tree, for example,
retain their fields even when several levels use the same directory type.

When a payload points back to an object already being printed, the cycle is shown as
`<circular>`. Shared objects reached through separate branches are printed in each branch.

Sum payloads use inline storage. Recursive trees need a reference or a dynamic collection
to break the value layout cycle, for example `type Tree = &Directory | File` when
`Directory` contains `Tree` fields. Reference variants also allow deliberate object cycles.
