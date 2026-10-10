# v_printf/v_sprintf

These are v implementations of the C language `printf` and `sprintf` functions.

> **Note**
> These functions are platform dependent in C, but in V they are platform independent.

### v_sprintf

`v_sprintf` has a variable number of parameters.
The first is a format string to control the appearance of the final string.
Each format specifier (%s, %d, etc.) in the format string
is replaced by the textual version of the following parameters.

```v
import strconv

fn main() {
	a := 'World'
	s := strconv.v_sprintf('Hello %s!', a)
	println(s)
}
```

```
Hello World!
```

### v_printf

`v_printf` creates the same modified string as `v_sprintf`, using the same format specifiers,
but it will immediately print the modified string to stdout instead of returning a string.

### Syntax

The syntax for a format specifier is:

```
%[parameter][flags][width][.precision][length]type
```

#### Flags field

The Flags field may be zero or more (in any order) of:

| Character   | Description                                                                                                                                                                                      |
| ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `-` (minus) | Left-align the output of this specifier. (The default is to right-align the output.)                                                                                                             |
| `+` (plus)  | Prepends a plus for positive signed-numeric types. positive = `+`, negative = `-`. (The default doesn't prepend anything to positive numbers.)                                                   |
| `0` (zero)  | When the 'width' option is specified, prepends zeros for numeric types. (The default prepends spaces.) For example, `printf("%4X",3)` produces `   3`, while `printf("%04X",3)` produces `0003`. |
| `#` (hash)  | Alternative form of `x`, `X`, `o` and `b`: prefixes `0x`, `0X`, `0` and `0b`. |

#### Width field

The Width field specifies a _maximum_ number of characters to output,
and is typically used to pad fixed-width fields in tabulated output,
it causes truncation of oversized fields.

The width field may be omitted, or it may be a numeric integer value,
or may also be specified by a parameter when indicated by an asterisk `*`.
For example, `v_printf("%*.s", 5, my_string)` will result in `   mystring` being printed,
with a total width of 5 characters.

#### Length field

The Length field can be omitted or be any of:

| Character | Description                                                                |
| --------- | -------------------------------------------------------------------------- |
| `hh`      | For integer types, causes `printf` to expect an `i8` or `u8` argument.     |
| `h`       | For integer types, causes `printf` to expect an `int16` or `u16` argument. |
| `l`       | For integer types, causes `printf` to expect an `i64` or `u64` argument.   |
| `ll`      | For integer types, causes `printf` to expect an `i64` or `u64` argument.   |
|           |                                                                            |
|           |                                                                            |

#### Type field

The Type field can be any of:

| Character | Description                                                                                                                 |
| --------- | --------------------------------------------------------------------------------------------------------------------------- |
| `%`       | Prints a literal `%` character (this type doesn't accept any flags, width, precision, length fields).                       |
| `d`, `i`  | `int` as a signed `int` `%d` and `%i` are synonymous for output. The size of the argument is specified by the length field. |
| `u`       | `unsigned int`. The size of the argument is specified by the length field.                                                  |
| `f`, `F`  | `double` in normal notation. `f` and `F` only differs in how the strings are printed: lowercase or uppercase.               |
| `e`, `E`  | `double` in scientific notation.`e` and `E` only differs in how the strings are printed: lowercase or uppercase.            |
| `g`, `G`  | `double` in automatic notation.`g` and `G` only differs in how the strings are printed: lowercase or uppercase.             |
| `x`, `X`  | `unsigned int` as a hexadecimal number. `x` uses lower-case letters and `X` uses upper-case.                                |
| `s`       | string                                                                                                                      |
| `p`       | `void *` (pointer to void) in an implementation-defined format.                                                             |
| `c`       | `char` (character).                                                                                                         |
| `o`       | `unsigned int` as an octal number. The length field specifies its size, as for `u`. |
| `b`       | `unsigned int` as a binary number. The length field specifies its size, as for `u`. |
| `t`       | `bool`, as `true` or `false`.                                                       |
| `q`       | string, as a double-quoted literal with the escape sequences of `strconv.quote`.    |

`o` and `b` print a negative argument in two's complement, at the size of the length field
(32 bits when it is omitted): `%o` of `-1` is `37777777777`, and `%hho` of `-1` is `377`.

With the `#` flag, a zero value gets no prefix, like in C: `%#x` of `0` is `0`.
When the field is padded with zeros, they follow the prefix: `%#08x` of `255` is `0x0000ff`.

A specifier with any other type is written back unchanged, for example `%y`.
It still takes one parameter, so the specifiers after it keep theirs.

## Examples

various types

```v
import strconv

a0 := u32(10)
b0 := 200
c0 := u8(12)
s0 := 'ciAo'
ch0 := `B`
f0 := 0.312345
f1 := 200000.0
sc0 := 'ciao: [%-08u] %d %hhd [%8s] [%08X] [%-20.4f] [%-20.4f] [%c]'
temp_s := strconv.v_sprintf(sc0, a0, b0, c0, s0, b0, f0, f1, ch0)
println(temp_s)
```

```
ciao: [10      ] 200 12 [    ciAo] [000000C8] [0.3123              ] [200000.0000         ] [B]
```

integer

```v
import strconv

a := u8(12)
b := i16(13)
c := 14
d := i64(15)
sc1 := '==>%hhd %hd %d %ld'
temp_s := strconv.v_sprintf(sc1, a, b, c, d)
println(temp_s)
```

```
==>12 13 14 15
```

unsigned integer

```v
import strconv

a1 := u8(0xff)
b1 := u16(0xffff)
c1 := u32(0xffffffff)
d1 := u64(-1)
sc2 := '%hhu %hu %u %lu'
temp_s := strconv.v_sprintf(sc2, a1, b1, c1, d1)
println(temp_s)
```

```
255 65535 4294967295 18446744073709551615
```

hexadecimal

```v
import strconv

a1 := u8(0xff)
b1 := i16(u16(0xffff))
c1 := u32(0xffffffff)
d1 := u64(-1)
sc3 := '%hhx %hx %x %lx'
temp_s := strconv.v_sprintf(sc3, a1, b1, c1, d1)
println(temp_s)
```

```
ff ffff ffffffff ffffffffffffffff
```

hexadecimal

```v
import strconv

a2 := 125
sc7 := '[%9x] [%9X] [%-9x] [%-9X] [%09x] [%09X]'
temp_s := strconv.v_sprintf(sc7, a2, a2, a2, a2, a2, a2)
println(temp_s)
```

```
[       7d] [       7D] [7d       ] [7D       ] [00000007d] [00000007D]
```

octal, binary, alternative form, bool, quoted string, unknown type

```v
import strconv

a := 10
ok := true
s := 'a "b"'
sc9 := '%o %#o %b %#b %#x [%-6t] [%10q] %y'
temp_s := strconv.v_sprintf(sc9, a, a, a, a, a, ok, s, a)
println(temp_s)
```

```
12 012 1010 0b1010 0xa [true  ] [ "a \"b\""] %y
```

floating points

```v
import strconv

f0 := 0.312345
f1 := 200000.0
f2 := -1234.300e6
f3 := 1234.300e-6
sc4 := '[%-20.3e] [%20.3e] [%-020.3e] [%-020.3E] [%-020.3e] [%-020.3e]'
temp_s := strconv.v_sprintf(sc4, f0, f1, f1, f1, f2, f3)
println(temp_s)
```

```
[3.123e-01           ] [           2.000e+05] [2.000e+05           ] [2.000E+05           ] [-1.234e+09          ] [1.234e-03           ]
```

float automatic notations

```v
import strconv

mut ft := -1e-7
mut x := 0
sc8 := '[%20g][%20G]|'
for x < 12 {
	temp_s := strconv.v_sprintf(sc8, ft, ft)
	println('${temp_s}\n')
	ft = ft * 10.0
	x++
}
```

```
[              -1e-07][              -1E-07]|
[              -1e-06][              -1E-06]|
[              -1e-05][              -1E-05]|
[             -0.0001][             -0.0001]|
[              -0.001][              -0.001]|
[               -0.01][               -0.01]|
[                -0.1][                -0.1]|
[                  -1][                  -1]|
[                 -10][                 -10]|
[                -100][                -100]|
[               -1000][               -1000]|
[              -10000][              -10000]|

```

## Utility functions

The format module also has some utility functions:

```v oksyntax nofmt
// calling struct
struct BF_param {
  pad_ch       u8         = ` `     // padding char
  len0         int        = -1      // default len for whole the number or string
  len1         int        = 6       // number of decimal digits, if needed
  positive     bool       = true    // mandatory: the sign of the number passed
  sign_flag    bool       = false   // flag for print sign as prefix in padding
  align        Align_text = .right  // alignment of the string
  rm_tail_zero bool       = false   // remove the tail zeros from floats
}

// utilities
fn format_dec(d u64, p BF_param) string
fn format_fl(f f64, p BF_param) string
fn format_es(f f64, p BF_param) string
fn remove_tail_zeros(s string) string
```

`format_dec` format the integer number using the parameters in the `BF_param` struct.

`format_fl` format a float number in normal notation using the parameters in the `BF_param` struct.

`format_es format a float number in scientific notation using the parameters in the BF_param`
struct.

`remove_tail_zeros` removes the tailing zeros from a floating point number as string.
