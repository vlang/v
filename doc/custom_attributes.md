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

An attribute whose name matches a registered schema yields that struct when a
`$for attr in X.attributes` loop is unrolled, instead of the flat `VAttribute`.
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

Supported schema field types are `string`, integer types, `bool`, and enums.
A use the checker rejects (unknown field, wrong arity) keeps the legacy pieces
so the checker diagnostic stays authoritative. Copies of the loop variable
(`r := attr`) keep the `VAttribute` view; narrowing applies to direct `attr`
uses. Union member resolution accepts the union of all registered schema fields
plus `VAttribute`'s four members inside an `attributes` loop; reaching a field
that the current instance may not have requires `$if attr is Schema` first,
otherwise it is a compile error.