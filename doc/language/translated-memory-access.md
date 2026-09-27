# Memory access in translated C

Files marked `@[translated]` retain C field mutability and pointer indexing rules.
Struct fields can be updated without V's `mut:` field annotation. A pointer into
an allocation can use a negative offset to access an earlier element in that
same allocation. Arrays still reject negative indexes.

These compatibility rules apply to expressions in the translated file. Ordinary
V files keep their usual field mutability and indexing checks, including when
using types declared by a translated file.
