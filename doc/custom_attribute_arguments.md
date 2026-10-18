# Attribute arguments

Typed attributes accept positional and named arguments. Each schema field must be supplied once;
repeating a named field or supplying it both positionally and by name is an error.

Enum values use a leading dot. A call such as `@[Route(method: .get)]` can begin with a named
enum argument, including when the schema is qualified with its module name.
