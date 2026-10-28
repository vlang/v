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

## Reading attributes at compile time

An attribute whose name matches a registered schema exposes its fields when a
`$for attr in X.attributes` loop is unrolled.
Narrow with `$if attr is Schema` before touching schema fields; other names
keep the `VAttribute` view (`name`/`has_arg`/`arg`/`kind`) exactly as before:

```v
@[attribute]
struct Route {
	path   string
	method string
}

@[Route('/index', method: 'get')]
@[deprecated: 'old']
fn index() {}

fn main() {
	$for attr in index.attributes {
		$if attr is Route {
			println('route: ${attr.path} ${attr.method}')
		} $else {
			println('other: ${attr.name}')
		}
	}
}
```

Supported schema field types are `string`, integer types (including `uint` and `rune`),
`bool`, enums, and aliases of those types.
A use the checker rejects (unknown field, wrong arity) keeps the legacy pieces
so the checker diagnostic stays authoritative. Copies of the loop variable
(`r := attr`) keep the `VAttribute` view; narrowing applies to direct `attr`
uses. Inside that narrowed branch, schema fields named `name`, `arg`, `has_arg`, or
`kind` take precedence over the metadata members. Method calls such as `attr.str()`
keep the `VAttribute` method even when a schema declares a `str` field.
Outside narrowing, metadata members retain the `VAttribute` view.

## Checking schema field access

Inside an `attributes` loop, read a schema field after narrowing the loop variable with
`$if attr is Schema`. The checker validates field access in initializers, assignments,
interpolations, calls, and runtime conditions. This also applies to chained field access.
The four metadata members `name`, `arg`, `has_arg`, and `kind` remain available without
narrowing. A narrowed schema's own field takes precedence over a metadata member of the same name.

Narrowing follows `&&`, `||`, parentheses, negation, and both branches of nested `$if`
conditions. A branch that can contain multiple schemas may read a field only when every
possible schema declares it with the same type. Compile-time conditions currently accept
the four metadata members; schema field values in those conditions require further work.
