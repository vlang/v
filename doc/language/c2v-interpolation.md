# Postfix expressions in translated interpolation

In translated C source, postfix increment and decrement expressions can appear
inside string interpolation. The expression formats its original value and
applies its update once, including for struct fields and numeric formats.
