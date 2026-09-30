# Enum fields in compiler-provided clones

Compiler-provided cloning copies enum values as scalars, including through chains of type aliases.
An enum's name can match a struct in another module without cloning that struct's fields.
String and collection fields alongside the enum retain their normal cloning behavior.
