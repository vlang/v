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
