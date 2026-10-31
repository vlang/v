# Function field calls

A function stored in a struct field can be called through the struct value, for example
`holder.callback(21)`. The receiver uses the type of its local variable or parameter.
Local names such as `wait`, `read`, and `close` may also be used when a C function has the
same name. This applies to value receivers, pointer receivers, and iteration variables.

Function field calls compile with `-new-compiler` without a compatibility compiler retry.
