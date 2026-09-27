# Struct values passed by reference

A struct update expression such as `Settings{...original value: 7}` can be passed
directly to an immutable reference parameter, just like a struct initializer.
The compiler materializes the updated value in a temporary before passing its
address. Updating the temporary does not change the original struct.
