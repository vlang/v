# vbeam Runtime Library

Erlang runtime library for V programs compiled with the BEAM backend.

## Modules

| Module | Purpose |
|--------|---------|
| `vbeam_io` | I/O operations (println, print, eprintln, eprint) |
| `vbeam_conv` | Type conversions (to_binary, string_to_int, etc.) |
| `vbeam_string` | String operations (contains, split, trim, etc.) |
| `vbeam_array` | Array/list operations (push, pop, slice, etc.) |
| `vbeam_map` | Map operations (get, set, delete, etc.) |
| `vbeam_math` | Math functions (sqrt, sin, cos, etc.) |
| `vbeam_os` | OS operations (arguments, getenv, file I/O) |
| `vbeam_panic` | Error handling (panic, assert) |

## Building

```bash
# Build all modules
./scripts/build_runtime.sh

# Manual compilation
mkdir -p ebin
erlc -o ebin *.erl
```

## Usage

After building, use one of these methods:

### Method 1: Copy to project
```bash
# Copy runtime .beam files to your V project output
cp ebin/*.beam /path/to/your/project.beam/
```

### Method 2: Add to Erlang path
```bash
# When running your compiled V program
erl -pa /path/to/vbeam/runtime/ebin -noshell -s 'v.main' main -s init stop
```

### Method 3: ERL_LIBS
```bash
export ERL_LIBS=/path/to/vlang/vlib/v/gen/beam/runtime
erl -noshell -s 'v.main' main -s init stop
```

## Example

```bash
# Compile V program to Erlang
cd /path/to/vlang
./v -b beam examples/hello_world.v

# Build runtime
./vlib/v/gen/beam/runtime/scripts/build_runtime.sh

# Copy runtime to output
cp vlib/v/gen/beam/runtime/ebin/*.beam examples/hello_world.beam/

# Run
cd examples/hello_world.beam
erl -noshell -s 'v.main' main -s init stop
```

## Requirements

- Erlang/OTP 24+ (for compilation and runtime)
- The V compiler with BEAM backend enabled

## V to Erlang Type Mapping

| V Type | Erlang Type |
|--------|-------------|
| `int`, `i32`, `i64`, etc. | `integer` |
| `f32`, `f64` | `float` |
| `string` | `binary` (<<...>>) |
| `bool` | `atom` (true/false) |
| `[]T` | `list` |
| `map[K]V` | `map` |
| `struct` | `map` with `{vbeam, type}` tag |
| `enum` | `atom` |
