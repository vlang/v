# Compile-time error messages

`$compile_error` accepts a plain string literal as its message.
Concatenated strings, interpolation, and other expressions receive an explicit parser diagnostic.
In generic reflection loops, selected errors include the directive's source location and are
reported once, without treating the directive as an ordinary function call.
With `-json-errors`, the diagnostic keeps its structured file, line, column, and message fields.
