# Pointer alias parameters in translated C

Translated C can use a pointer alias as a mutable parameter. For example, when
`IntPtr` aliases `&int`, a translated `mut cursor IntPtr` parameter retains the
address of the caller's pointer slot. `*cursor` reads or replaces that pointer and
`**cursor` reads its pointee. Ordinary V parameter binding keeps its value semantics.
