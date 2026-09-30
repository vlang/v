# Struct field defaults in arrays

Struct field defaults run when a struct value is initialized.
An array initializer such as `[]Box{len: 1}` initializes a `Box` element and its defaults.
Empty and capacity-only arrays, such as `[]Box{}` and `[]Box{cap: 10}`, do not initialize
elements or run their defaults.
With an explicit `init`, only that initializer determines the element's field values:
`[]Box{len: 1, init: Box{value: 1}}` does not run the default for `value`, whereas
`[]Box{len: 1, init: Box{}}` does.
An initializer such as `[][]Box{len: 1}` creates one empty `[]Box`; it does not initialize
any `Box` elements or run their field defaults.
Nested fixed arrays contain initialized elements, so their struct defaults still apply.
An explicit fixed-array `init` also supplies the elements instead of implicitly initializing them.

A type alias preserves the initialization behavior of its underlying type.
For `type Alias = Box`, `[]Alias{len: 1}` runs the same field defaults as `[]Box{len: 1}`.
An omitted struct field of type `Alias` also uses the underlying `Box` defaults.
Aliases to fixed arrays initialize their elements, including when used as an omitted struct field.
For `type Rows = []Box`, `Rows{len: 1}` also initializes a `Box`, while `Rows{}` remains empty.
Omitted fields or array elements whose aliases wrap references, options, or dynamic containers
do not initialize the underlying struct.

Omitted generic fields use their concrete type arguments.
For `struct Outer[T] { inner T }`, `Outer[Box]{}` initializes `inner` with `Box`'s defaults.
Nested generic values and fixed arrays also initialize their contained values.
For `Outer[[]Box]{}`, the omitted field is an empty array and runs no `Box` defaults.
A generic field initializer such as `outer Outer[T] = Outer[T]{}` also uses concrete arguments.
Explicit fields in either constructor continue to replace those fields' defaults.
