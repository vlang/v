# WIT Conformance Matrix

Rows = WIT constructs. Columns = parse / print / codegen / validate.
✅ = supported with a test, ❌ = not supported (reason in Notes).

| Construct | Parse | Print | Codegen | Validate |
|---|---|---|---|---|
| package (namespace:name@version) | ✅ | ✅ | ✅ | ❌ |
| interface | ✅ | ✅ | ✅ | ✅ |
| world | ✅ | ✅ | ✅ | ✅ |
| record | ✅ | ✅ | ✅ | ✅ |
| variant | ✅ | ✅ | ✅ | ✅ |
| enum | ✅ | ✅ | ✅ | ✅ |
| flags | ✅ | ✅ | ✅ | ✅ |
| resource (basic + constructor + methods) | ✅ | ✅ | ✅ | ✅ |
| resource (static methods) | ✅ | ✅ | ✅ | ✅ |
| type alias | ✅ | ✅ | ✅ | ✅ |
| func | ✅ | ✅ | ✅ | ✅ |
| list<T> | ✅ | ✅ | ✅ | ✅ |
| option<T> | ✅ | ✅ | ✅ | ✅ |
| result<T, E> | ✅ | ✅ | ✅ | ✅ |
| tuple<...> | ✅ | ✅ | ✅ | ✅ |
| own<T> / borrow<T> | ✅ | ✅ | ✅ | ✅ |
| map<K, V> | ✅ | ✅ | ✅ | ✅ |
| string (param) | ✅ | ✅ | ✅ | ✅ |
| string (return) | ✅ | ✅ | ✅ | ✅ |
| async / future / stream | ✅ | ✅ | ✅ | ✅ |
| use (incl. `as`) | ✅ | ✅ | ✅ | ✅ |
| include | ✅ | ✅ | ✅ | ✅ |
| include ... with | ✅ | ✅ | ✅ | ✅ |
| @since / @deprecated / @unstable | ✅ | ✅ | ✅ | ✅ |
| @version on package | ✅ | ✅ | ✅ | ✅ |

## Component lift (core module -> component)

Function-signature shapes the component wrapper lifts today:

| Shape | Lifted |
|---|---|
| primitive params/results (i32/i64/f32/f64) | ✅ |
| single string param, `(i32, i32) -> r` core sig | ✅ |
| string returns | ❌ (depends on Item 1 follow-up) |
| lists / records / variants / resources | ❌ (depends on Item 1 follow-up) |
| options / results / tuples / maps | ❌ (depends on Item 1 follow-up) |

## Notes

- **use codegen**: single-file codegen emits all interfaces into one namespace,
  so used names resolve locally; the `// use ...` comment records the source.
- **own/borrow codegen**: handle types map to `voidptr`; `own`/`borrow` of a
  non-resource is rejected (`error_not_a_resource`).
- **async codegen**: `future<T>` / `stream<T>` surface types map to `voidptr`;
  only the surface (types, imports, exports) is generated, runtime support
  is out of scope.
- **map codegen**: emitted as V `map<K, V>`.
- **include / include ... with**: parsed into `WitInclude` nodes carrying the
  path plus `with` substitutions (aliases sorted for stable printing),
  re-emitted verbatim, and resolved by validation (unknown targets and
  unresolvable renames rejected; external package paths are allowed).
  Worlds themselves are not codegened, so includes need no V output.
- **static methods**: parsed (`is_static` on WitFunc), printed with the
  `static` keyword, codegened as namespaced functions (`Type_method`);
  a static method taking its own resource type is rejected
  (`error_static_method_takes_resource`).
- **attributes**: collected canonically (`@name` / `@name(k = v, ...)`,
  spacing normalized) and re-emitted; round-trip is AST-identical.
  World import/export-level attributes are dropped (no AST field).
- **version checks**: an interface `use` of an item whose `@since` exceeds
  the importer's `@since` is rejected (`error_version_incompatible`);
  items without their own `@since` inherit their interface's version.
  Unknown `use` targets (`error_unknown_interface`) and members
  (`error_unresolved_use`) are rejected. Duplicate constructors are
  rejected (`error_duplicate_constructor`).
- **validate**: package declarations are checked for a namespace and a
  semver `x.y.z` version (`error_invalid_package`,
  `error_invalid_package_version`).
- Resource methods have no implicit self parameter in WIT, so no
  receiver check applies; constructors are separated by the parser, so
  no constructor-as-method check applies.
