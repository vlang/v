# `v2.autofix`

Mechanical source-level rewrites driven by the `-autofix` flag.

## Currently implemented

- **Add missing `mut`**: A variable declared with `:=` that is later mutated
  (`x = ...`, `x += ...`, `x++`, `x--`, or passed as `mut x` to a call)
  is rewritten to `mut x := ...`. Lexical scope is honored, so a shadowed
  inner `x` does not affect the outer declaration.

  Skipped for declarations in `for-init` (`for i := 0; ... ; i++`) where V
  treats the loop variable as implicitly mutable.

## Candidates for future autofix passes

These are mechanical, low-risk rewrites that fit the same style. Listed here
so we can pick them up later, not implemented yet.

- **Remove unused `mut`**: variable declared `mut x := ...` but never mutated
  → drop the `mut` keyword.
- **Unused imports**: `import foo` with no reference to `foo.*` in the file
  → delete the import line.
- **Unused local variables**: `x := expr` where `x` is never read
  → rename to `_` (when `expr` has side effects) or delete the statement
  (when `expr` is pure).
- **Unnecessary `unsafe { ... }`**: `unsafe` block whose body contains no
  unsafe operations → unwrap.
- **Unused function parameters**: warn-only today; autofix could prefix with
  `_` to silence the diagnostic.
- **Snake_case rename**: function/variable names in camelCase
  → rename to snake_case (with cross-file references updated).
- **Trailing-comma normalization**: enforce or strip the trailing comma in
  multi-line lists according to the project style.
- **Add `?` / `!` propagation**: when a return value is an Option/Result and
  the caller ignores it via `_ := f()`, suggest `f()?` or `f()!` based on the
  enclosing function's return type.
- **Replace `if x != none { x.foo() }` with `if y := x { y.foo() }`** for
  Option chains.
- **`println('${x}')` → `println(x)`** when the interpolation has no
  formatting and the value is already a string.
- **Drop redundant `return`** at the end of a `void` function.
- **Re-order `pub` / `mut` modifiers** to match the canonical order V fmt
  uses (`pub mut:` etc.).
