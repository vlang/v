The `flat` module stores compact syntax tree nodes for the compiler.

`canonical_comptime_type_payload` is a reserved node payload id for a reflection source whose
type already identifies its declaring module. Generic specialization uses it to preserve that
identity through later import resolution. It requires no allocation, `Node.clone_owned` keeps it,
and `node_payload_at` returns a static empty payload for this marker. It carries no generic
parameters or constraints.
