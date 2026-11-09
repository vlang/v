# WIT Conformance Matrix

Rows = WIT constructs. Columns = parse / print / codegen / validate.
✅ = supported, ⚠️ = partial, ❌ = not supported.

| Construct | Parse | Print | Codegen | Validate |
|---|---|---|---|---|
| package (namespace:name@version) | ✅ | ✅ | ✅ | ❌ |
| interface | ✅ | ✅ | ✅ | ❌ |
| world | ✅ | ✅ | ✅ | ❌ |
| record | ✅ | ✅ | ✅ | ❌ |
| variant | ✅ | ✅ | ✅ | ❌ |
| enum | ✅ | ✅ | ✅ | ❌ |
| flags | ✅ | ✅ | ✅ | ❌ |
| resource (basic) | ✅ | ⚠️ | ✅ | ❌ |
| resource (constructor) | ✅ | ⚠️ | ⚠️ | ❌ |
| type alias | ✅ | ✅ | ✅ | ❌ |
| func | ✅ | ✅ | ✅ | ❌ |
| list<T> | ✅ | ✅ | ✅ | ❌ |
| option<T> | ✅ | ✅ | ✅ | ❌ |
| result<T, E> | ✅ | ✅ | ✅ | ❌ |
| tuple<...> | ✅ | ✅ | ✅ | ❌ |
| own<T> | ✅ | ✅ | ⚠️ | ❌ |
| borrow<T> | ✅ | ✅ | ⚠️ | ❌ |
| map<K, V> | ✅ | ✅ | ✅ | ❌ |
| async/future/stream | ✅ | ✅ | ⚠️ | ❌ |
| use | ✅ | ✅ | ⚠️ | ❌ |
| include | ✅ | ⚠️ | ❌ | ❌ |
| include ... with | ✅ | ❌ | ❌ | ❌ |
| @since/@deprecated attributes | ✅ | ❌ | ❌ | ❌ |
| @version on package | ✅ | ✅ | ✅ | ❌ |

## Notes

- **own/borrow/map/async/future/stream**: parsed as named types (`own<T>`, etc.); codegen maps to `voidptr`/`int`. Full type mapping deferred to Item 3 (component ABI).
- **resource print**: printer emits only fields name/type, not full method signatures.
- **include ... with**: parsed by consuming tokens until `;`; not printed or codegened.
- **attributes**: parsed and skipped; not printed or codegened.
- **validate**: no validation implemented yet.