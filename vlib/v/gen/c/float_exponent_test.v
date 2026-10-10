module c

import os
import v.cmdexec
import v.flat
import v.types

fn test_float_precision_normalizes_padded_crt_exponents() {
	cc := os.find_abs_path_of_executable('cc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'float_exponent_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut g := FlatGen.new()
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	g.a = &ast
	g.tc = &tc
	g.has_builtins = true
	g.builtin_abi_decls()
	helpers := g.sb.str().split_into_lines().filter(it.starts_with('static inline string v3_f64_exp(')
		|| it.starts_with('static inline string v3_f64_general(')
		|| it.starts_with('static inline int v3_float_normalize_exponent(')).join('\n')
	// Inject the Windows CRT exponent padding into the actual generated helpers.
	// Precision 200 also exercises their dynamically allocated snprintf buffer.
	prefix := '
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <assert.h>
typedef unsigned char u8;
typedef struct { u8* str; int len; int is_lit; } string;
static u8* malloc_noscan(ptrdiff_t n) { return malloc((size_t)n); }
static string v3_c_lit(const char* s, int n) { return (string){(u8*)s, n, 1}; }
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
int main(void) {
    double values[] = {1.23456789e13, -1.23456789e13, 2.0, 1e-9, 1e99,
        1e100, 1e-100, 1e308, 1e-308};
    int precisions[] = {0, 2, 200};
    for (size_t v = 0; v < sizeof(values)/sizeof(values[0]); ++v)
        for (size_t p = 0; p < sizeof(precisions)/sizeof(precisions[0]); ++p)
            for (int upper = 0; upper < 2; ++upper)
                for (int general = 0; general < 2; ++general) {
                    char expected[4096];
                    const char* format = general ? (upper ? "%.*G" : "%.*g")
                        : (upper ? "%.*E" : "%.*e");
                    int n = snprintf(expected, sizeof(expected), format, precisions[p], values[v]);
                    string actual = general ? v3_f64_general(values[v], precisions[p], upper)
                        : v3_f64_exp(values[v], precisions[p], upper);
                    assert(actual.len == n);
                    assert(strcmp((char*)actual.str, expected) == 0);
                    assert(actual.str[actual.len] == 0);
                    free(actual.str);
                }
    return 0;
}
'
	source := os.join_path(dir, 'exponents.c')
	binary := os.join_path(dir, 'exponents' + $if windows { '.exe' } $else { '' })
	os.write_file(source, prefix + helpers + checks)!
	compiled := cmdexec.run(cc, ['-std=c99', '-Wall', '-Wextra', '-Werror', '-o', binary, source])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(binary, [])
	assert executed.exit_code == 0, executed.output
}
