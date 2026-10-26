# Attribute schemas

A struct marked with `@[attribute]` declares a metadata schema. Schemas cannot be initialized,
embedded, or supplied as generic type arguments. This restriction covers generic types,
function and method calls, and generic functions taken as values.

## Unknown attribute names

An attribute that is neither a builtin nor a registered schema warns (warn-first migration,
[vlang/rfcs#35](https://github.com/vlang/rfcs/pull/35)):

```text
unknown attribute `Rout`. Did you mean `Route`? Unvalidated attributes are deprecated;
declare a schema with `@[attribute]` or drop the attribute.
```

The suggestion appears when a same-named schema exists in another module (qualify it,
`mod.Name`), or when a schema or builtin name is close (first letter shared, small edit
distance). An attribute call `@[Name(args)]` warns once, on its name, never per argument.

Whole-string metadata such as `@['legacy']` does not warn. Independently written names,
such as `@[foo; foo_bar]`, are checked separately, even when one name prefixes another.
