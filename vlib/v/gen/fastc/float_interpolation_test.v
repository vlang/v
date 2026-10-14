module fastc

import os
import v.cmdexec

// `${x:.2}`, `${x:8.2}`, `${x:-8.2}` and `${x:.2f}` format a float as the C backend does:
// the precision rounds, `f` keeps trailing zeros, and the plain form trims them.
fn test_fastc_float_interpolation_formats_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_float_format_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_float_format' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn main() {
	x := 3.14159
	y := -2.5
	z := 1234567.891
	w := 0.000012345
	println('[\${x:.2}] [\${x:8.2}] [\${x:-8.2}] [\${x:.2f}] [\${y:.3}] [\${y:.1f}]')
	println('[\${z:.3}] [\${w:.4}] [\${x:.0}] [\${f32(1.5):.1}] [\${100.0:.2}]')
}
") or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '[3.14] [    3.14] [3.14    ] [3.14] [-2.5] [-2.5]\n[1.23e+06] [0] [3] [1.5] [100]'
}

// The helpers strip the third exponent digit some CRTs print (`1.23e+006`), as the C
// backend's v3_float_normalize_exponent does: the actual helper text is compiled with an
// `snprintf` that pads every two-digit exponent, on both its stack and allocated buffers.
fn test_fastc_float_helpers_normalize_padded_crt_exponents() {
	cc := os.find_abs_path_of_executable('cc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'fastc_float_exponent_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	prefix := '
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
typedef unsigned char u8;
typedef unsigned long long u64;
typedef struct { u8* str; int len; int is_lit; } string;
#define _S(s) ((string){.str = (u8*)("" s), .len = (int)(sizeof(s) - 1), .is_lit = 1})
static string builtin__f64_str(double x) {
    char* text = malloc(64);
    int n = snprintf(text, 64, "%.17g", x);
    return (string){.str = (u8*)text, .len = n, .is_lit = 0};
}
static int padded_snprintf(char* out, size_t cap, const char* format, ...) {
    char text[4096];
    va_list args;
    va_start(args, format);
    int n = vsnprintf(text, sizeof(text), format, args);
    va_end(args);
    assert(n >= 0 && n + 2 < (int)sizeof(text));
    int first = n;
    while (first > 0 && text[first - 1] >= 48 && text[first - 1] <= 57) --first;
    if (first >= 2 && (text[first - 2] == 101 || text[first - 2] == 69)
        && (text[first - 1] == 43 || text[first - 1] == 45) && n - first == 2) {
        memmove(text + first + 1, text + first, (size_t)(n - first + 1));
        text[first] = 48;
        ++n;
    }
    if (cap > 0) {
        size_t copied = (size_t)n < cap - 1 ? (size_t)n : cap - 1;
        memcpy(out, text, copied);
        out[copied] = 0;
    }
    return n;
}
#undef snprintf
#define snprintf padded_snprintf
'
	checks := '
#undef snprintf
static void expect_trimmed(double x, int precision, const char* expected) {
    string actual = v_fastc_f64_trimmed(x, precision);
    assert(actual.len == (int)strlen(expected));
    assert(strcmp((char*)actual.str, expected) == 0);
}
int main(void) {
    double values[] = {1234567.891, -1.23456789e13, 1e-9, 1e99, 1e100, 1e-100, 1e308, 1e-308};
    int precisions[] = {0, 2, 200};
    for (size_t v = 0; v < sizeof(values)/sizeof(values[0]); ++v)
        for (size_t p = 0; p < sizeof(precisions)/sizeof(precisions[0]); ++p) {
            char expected[4096];
            int n = snprintf(expected, sizeof(expected), "%.*e", precisions[p], values[v]);
            /* Rebuild the exponent with two digits at least, whatever this CRT prints. */
            char* exponent = strrchr(expected, 101);
            int power = (int)strtol(exponent + 1, NULL, 10);
            size_t offset = (size_t)(exponent - expected);
            n = (int)offset + snprintf(exponent, sizeof(expected) - offset, "e%+03d", power);
            string actual = v_fastc_f64_exp(values[v], precisions[p]);
            assert(actual.len == n);
            assert(strcmp((char*)actual.str, expected) == 0);
            assert(actual.str[actual.len] == 0);
        }
    expect_trimmed(1234567.891, 3, "1.23e+06");
    expect_trimmed(-1.5e-7, 2, "-1.5e-07");
    expect_trimmed(1e100, 2, "1e+100");
    return 0;
}
'
	source := os.join_path(dir, 'exponents.c')
	binary := os.join_path(dir, 'exponents' + $if windows { '.exe' } $else { '' })
	os.write_file(source, prefix + fastc_float_format_c + checks) or { panic(err) }
	compiled := cmdexec.run(cc, ['-std=c99', '-Wall', '-Wextra', '-Werror', '-o', binary, source,
		'-lm'])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(binary, [])
	assert executed.exit_code == 0, executed.output
}
