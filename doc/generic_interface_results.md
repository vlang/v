# Generic results converted to interfaces

When converting a generic function's result to an interface, V uses the concrete return type
inferred from the call's arguments. The interface conversion happens after specialization;
the target interface does not replace a type parameter already determined by an argument.

For example, `Shape(identity(Square{}))` specializes an identity function to `Square` and
stores a `Square` in the interface. Its methods and runtime type tests work as they do when
the generic result is first assigned to a local variable.

The same rule applies to optional interface fields and to generic helpers that unwrap an
optional concrete pointer before the interface conversion.
