# Imported callback defaults

A function used as a struct field's default resolves its parameter types in the
module that declares the callback. Importing another module with the same short
type name does not change the callback's signature. This also applies when the
struct is initialized in another module.
