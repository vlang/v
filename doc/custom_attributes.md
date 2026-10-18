# Attribute schemas

A struct marked with `@[attribute]` declares a metadata schema. Schemas cannot be initialized,
embedded, or supplied as generic type arguments. This restriction covers generic types,
function and method calls, and generic functions taken as values.
