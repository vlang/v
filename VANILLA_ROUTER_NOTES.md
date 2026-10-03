# Comptime features for an attribute router (vanilla's `veb_like`)

Exploration notes for this branch. Goal: let a declarative router — handlers marked
`@['GET /users/:id']`, discovered with `$for method in T.methods` — be as fast and as
lean as a hand-written `match` over path segments. Measurements come from
[vanilla](https://github.com/enghitalo/vanilla) (`examples/veb_like`, `examples/router`,
`bench/router`), built with `-prod -gc none` on an AMD Ryzen 7 5800H.

## Where the router stands without compiler changes

vanilla's rewritten `veb_like` reads the attributes once at startup into a segment trie,
walks it per request (O(path depth)), and dispatches through a `$for` whose integer
compares GCC turns into a jump table with the handlers inlined. It allocates nothing per
request and runs at 1.1–1.3x the cost of the hand-written tree (54–156 ns vs 49–121 ns
in process). Getting there meant working around four compiler behaviours, two of which
this branch fixes.

## Implemented on this branch

### 1. `method.attrs` is free inside `$for`

`for attr in method.attrs` inside `$for method in T.methods` lowered to an array
literal: one heap allocation per method, on every pass. A router that reads attributes
per request paid one allocation per route scanned (a leak under `-gc none`). Now:

- the loop is unrolled into one block per attribute with `attr` bound to its literal
  (a body with `break`/`continue` keeps the array form); no attributes, no loop;
- `method.attrs.len` and `method.attrs.contains('x')` fold to constants;
- `$if method.attrs.len > 0` and `$if 'x' in method.attrs` evaluate — before, both
  were silently false. A router can now dispatch only to methods that carry
  attributes, instead of requiring every method of a given return type to be a handler.

Effect on the *old* `veb_like`, unchanged source, allocations per request:

| request | V `863ae78` | this branch |
| --- | ---: | ---: |
| static hit | 1 | 0 |
| 3 params | 18 | 9 |
| catch-all (last route) | 20 | 7 |
| 405 | 33 | 7 |
| 404 | 28 | 2 |

(What remains is what that code allocates explicitly: a params map, response buffers.)

Test: `vlib/v/tests/comptime/comptime_method_attrs_unrolled_test.v`.

### 2. Forwarding a `mut` param in a comptime call

`app.$method(req, mut out)`, with `out` itself a `mut` param, failed with
"cannot use `&[]u8` as `&&[]u8`". The check targets `mut x &T` params, but the parser
records a plain `mut x T` param as `&T` too; the comptime method cases now carry the
parser's explicit-mut-ref flag. Test:
`vlib/v/tests/comptime/comptime_call_forwarded_mut_arg_test.v`.

## Measured, not pursued: a table-free router over literal attributes

With (1), a router can match each attribute where it is declared, inside the `$for`: no
trie, no startup step, no state — the routes are constants the C compiler sees. A
prototype (vanilla routes, byte-identical answers) per request:

| request | trie `veb_like` | table-free | hand-written |
| --- | ---: | ---: | ---: |
| `GET /users` (1st route) | 86 ns | 84 ns | 76 ns |
| `GET /users/7/posts/99` | 144 ns | 229 ns | 113 ns |
| `POST /users/42` (405) | 63 ns | 265 ns | 52 ns |
| `GET /nope/x` (404) | 56 ns | 127 ns | 53 ns |

Literal patterns remove the tables but not the linear scan: every route is still tried.
A fast declarative router needs tree-shaped matching, which today means a table built at
runtime — hence proposal A.

## Proposals

**A. Compile-time evaluation of `const` initializers.** Run a pure function at compile
time (`vlib/v/eval` already interprets V) and emit its result as static C data:

```v ignore
const routes = $comptime(veb_like.compile[App]())
```

The trie would live in `.rodata` — no heap, no startup work, shared by every worker — and
a routing mistake (duplicate route, bad pattern) would be a compile error instead of a
startup failure. Matching speed stays that of today's trie. Scope: values without
pointers into the heap, or with pointers the generator can lower to static arrays.

**B. Method values: `T.method` and `T.$method`.** `f := App.one` type-checks today, as
`int`, and cgen emits `int f = App.one;` (invalid C); `T.$method` in a generic emits an
undeclared `T`. The C function (`App__one(App*, ...)`) already has the right shape, so
`App.one` should be a plain `fn (&App, ...)` pointer — no closure allocation, unlike
`app.one`. With A, a router could keep a static handler table. Not a speed win on its
own: the `$for` jump table lets GCC inline handlers, a pointer table does not.

**C. Escape analysis for fixed arrays.** A local struct containing a fixed array is
moved to the heap whenever its address reaches a call (`transform.v`: "the called
function can forward a field or view beyond this frame"), so `Params { vals [8]Slice }`
costs an allocation per request when passed as `&Params`. vanilla works around it with
eight plain fields. A callee summary (a known, non-generic callee that never slices or
stores the array), or a `@[noescape]` param attribute, would keep such structs on the
stack.

**D. `@[noalloc]` for hot paths.** `-warn-about-allocs` lists allocation sites, but it
cannot tell startup code from per-request code and misses allocations the transform
creates (the `method.attrs` array above). An attribute that turns every allocation in a
function body — syntactic or generated — into a compile error would let a server pin its
request path as allocation-free in CI.

**E. Comptime string functions.** `$if attr.starts_with('GET ')`,
`$for seg in attr.all_after(' ').split('/')`: enough to generate per-route matchers
segment by segment. The prototype above shows the limit — still linear over routes —
so this is most useful together with A.

## Validation

Rebased on V `863ae78`, compiler rebuilt with `./v self`, suites run two jobs at a time.
Every failure below was re-run with an unmodified compiler at `863ae78` (also self-built).

- `vlib/v/transform/` (33), `vlib/v/types/` (97), `vlib/v/gen/c/` (51): pass.
- `vlib/v/tests/comptime/` (180, including the two new tests): 179 pass.
  `comptime_if_in_or_block_test.v` fails the same way unmodified (its `-os cross` case).
- The 13 other files under `vlib/v/tests/` that use `.attrs`, `$method` or
  `$for ... .methods`: 12 pass. `global_shadow_diagnostic_scope_test.v` fails the same two
  cross-target cases unmodified.
- `vlib/veb/` (36, one skipped), `vlib/toml/` and `vlib/x/json5/` (43), the other users
  of `$for` over methods in vlib: pass.
- `vlib/v/compiler_errors_test.v`: 1748 pass, 3 fail.
  `checker/tests/modules/anon_struct_private_field_err` and
  `checker/tests/modules/unknown_type_named_like_main_type` give the same output unmodified
  (and did at `0137eb5`). `generic_type_inference.vv` crashed the compiler once (invalid
  memory access in a map lookup of `smartcast_type`, during cgen); it uses none of the
  changed features and passed 25 more runs with this compiler and 20 unmodified.
- Not run: `v test-all` and the whole `vlib/v/`. `vlib/v/compiler_tests/` builds a
  complete compiler per test, too heavy for the machine used.
