# Run all ARM64 tests
```
  ./v run vlib/v2/gen/arm64/tests/run_tests.v

```
  # Run specific test with test_ssa_backends
```
  cd cmd/v2
  ./test_ssa_backends arm64 ../../vlib/v2/gen/arm64/tests/string_concat.v
```

  # Or directly with v2
```
  ./v2 -backend arm64 ../../vlib/v2/gen/arm64/tests/string_concat.v
```

# Useful cleanc flags
```
  # Disable builtin+strconv object cache for this build:
  ./v2 -backend cleanc -nocache file.v

  # Print C compiler and link command(s):
  ./v2 -backend cleanc -showcc file.v

  # Print extended parse/build stats:
  ./v2 -stats -backend cleanc file.v
  # Includes per-pass C Gen timing lines to help locate slow codegen phases.

  # Print every parsed file (full and .vh groups):
  ./v2 -print-parsed-files -backend cleanc file.v

  # Disable markused pruning (emit all functions):
  ./v2 -nomarkused -backend cleanc file.v
```

# SSA C backend (restored)
```
  # Run the SSA -> C backend directly:
  ./v2 -backend c cmd/v2/test.v

  # Compare SSA C output against reference V output:
  cd cmd/v2
  ../../v run test_ssa_backends.v c
```
