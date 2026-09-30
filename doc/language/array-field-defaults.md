# Struct field defaults in arrays

Struct field defaults run when a struct value is initialized.
An array initializer such as `[]Box{len: 1}` initializes a `Box` element and its defaults.
An initializer such as `[][]Box{len: 1}` creates one empty `[]Box`; it does not initialize
any `Box` elements or run their field defaults.
Nested fixed arrays contain initialized elements, so their struct defaults still apply.
