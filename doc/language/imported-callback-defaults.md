# Imported callback defaults

A function used as a struct field's default resolves its parameter and return
types in the file that declares the callback, including selective imports.
Importing another module with the same short type name does not change the
callback's signature. This also applies when the struct is initialized in another
module.
