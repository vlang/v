module modulecache

const sample_header = '#include <stdio.h>
#define call_helper(x) helper_through_macro(x)
typedef struct Thing { int a; } Thing;
int used_by_body(int x);
int unused_one(int x);
extern int g_used;
extern int g_unused;
extern char table_unused[16];
Thing* make_thing(void);
int helper_through_macro(int x);
static inline int kept_helper(int x) { return called_by_static(x); }
int called_by_static(int x);
#ifndef V3CACHE_PROGRAM_UNIT
int guarded_unused(void);
#endif
int (*fn_ptr_var)(int);
__attribute__((noreturn)) void attributed(int);
void attributed_after(int) __attribute__((noreturn));
int multi_line_proto(int a,
	int b);
struct Opaque;
'

fn test_unreferenced_prototypes_and_externs_are_left_out() {
	idx := new_declaration_index(sample_header)
	out := idx.subset('int f(void) { return used_by_body(g_used) + call_helper(1); }')
	assert out.contains('int used_by_body(int x);')
	assert out.contains('extern int g_used;')
	assert !out.contains('unused_one')
	assert !out.contains('g_unused')
	assert !out.contains('table_unused')
	assert !out.contains('guarded_unused')
	// the guard lines stay: an empty block is valid C
	assert out.contains('#ifndef V3CACHE_PROGRAM_UNIT')
}

fn test_what_a_macro_or_a_kept_function_names_stays() {
	idx := new_declaration_index(sample_header)
	out := idx.subset('int f(void) { return 0; }')
	// helper_through_macro is named by a macro, called_by_static by a static function
	assert out.contains('int helper_through_macro(int x);')
	assert out.contains('int called_by_static(int x);')
	// nothing else of the droppable kind is needed
	assert !out.contains('used_by_body')
	assert !out.contains('make_thing')
}

fn test_declarations_of_other_shapes_always_stay() {
	idx := new_declaration_index(sample_header)
	out := idx.subset('')
	for must in ['#include <stdio.h>', 'typedef struct Thing { int a; } Thing;',
		'int (*fn_ptr_var)(int);', '__attribute__((noreturn)) void attributed(int);',
		'void attributed_after(int) __attribute__((noreturn));', 'int multi_line_proto(int a,',
		'\tint b);', 'struct Opaque;', 'static inline int kept_helper'] {
		assert out.contains(must), must
	}
}

fn test_the_order_of_what_stays_is_the_headers() {
	idx := new_declaration_index(sample_header)
	out := idx.subset('int f(void) { return used_by_body(1) + make_thing()->a + g_used; }')
	i_include := out.index('#include') or { -1 }
	i_typedef := out.index('typedef struct Thing') or { -1 }
	i_used := out.index('used_by_body') or { -1 }
	i_extern := out.index('extern int g_used') or { -1 }
	i_make := out.index('make_thing') or { -1 }
	assert i_include >= 0 && i_include < i_typedef
	assert i_typedef < i_used && i_used < i_extern && i_extern < i_make
}

fn test_a_body_that_gains_a_reference_to_a_later_declaration_gets_it() {
	// the declaration comes after everything else the module uses: a reference added to the
	// code later makes the declaration appear, in the same place
	idx := new_declaration_index(sample_header)
	before := idx.subset('int f(void) { return used_by_body(1); }')
	after := idx.subset('int f(void) { return used_by_body(1) + unused_one(2); }')
	assert !before.contains('unused_one')
	assert after.contains('int unused_one(int x);')
	assert after.index('int used_by_body') or { -1 } < after.index('int unused_one') or { -1 }
}

fn test_declarations_inside_a_block_or_a_comment_are_not_candidates() {
	header := 'struct S {
int member_like(int x);
};
/* int in_comment(int x);
int still_comment(int x); */
int real(int x);
'
	idx := new_declaration_index(header)
	out := idx.subset('')
	assert out.contains('int member_like(int x);')
	assert out.contains('int in_comment(int x);')
	assert out.contains('int still_comment(int x);')
	assert !out.contains('int real(int x);')
}

fn test_a_header_with_nothing_to_drop_is_returned_whole() {
	header := '#define A 1\ntypedef int T;\n'
	idx := new_declaration_index(header)
	assert idx.subset('anything') == header
}

fn test_unreferenced_string_literal_definitions_are_left_out_with_their_continuation_lines() {
	header := 'static string _v3_lit_1 = {"one", 3};
static string _v3_lit_2 = {"a long literal "
	"continued", 20};
static string _v3_lit_3 = {"three", 5};
static inline int uses_three(void) { return _v3_lit_3.len; }
int tail(void);
'
	idx := new_declaration_index(header)
	out := idx.subset('int f(void) { return _v3_lit_1.len; }')
	assert out.contains('_v3_lit_1 =')
	assert !out.contains('_v3_lit_2')
	assert !out.contains('continued')
	// named by a unit that stays
	assert out.contains('_v3_lit_3 =')
	assert !out.contains('int tail(void);') // an unreferenced prototype goes with them
}

// naive_subset is subset written unit by unit, the way it was first written: the one the
// run-copying version must agree with.
fn naive_subset(idx DeclarationIndex, code string) string {
	mut used := map[string]bool{}
	collect_identifiers(code, mut used)
	mut out := ''
	for i, unit in idx.units {
		name := idx.declared[i]
		if name.len > 0 && !used[name] && !idx.kept[name] {
			continue
		}
		symbol := idx.literal[i]
		if symbol.len > 0 && !used[symbol] && !idx.kept[symbol] {
			continue
		}
		out += unit + '\n'
	}
	return out
}

fn test_subset_agrees_with_the_unit_by_unit_version() {
	header := sample_header + 'static string _v3_lit_9 = {"nine", 4};\nint last_unused(void);\n'
	idx := new_declaration_index(header)
	for code in ['', 'used_by_body(g_used)', 'unused_one make_thing g_unused', 'last_unused',
		'_v3_lit_9',
		'used_by_body unused_one make_thing last_unused _v3_lit_9 g_used g_unused table_unused'] {
		assert idx.subset(code) == naive_subset(idx, code), code
	}
	// the first unit and the last unit may each be a unit that goes
	first_goes := new_declaration_index('int first(void);\nint keep_me;\nint last_one(void);\n')
	assert first_goes.subset('') == naive_subset(first_goes, '')
	assert first_goes.subset('first last_one') == naive_subset(first_goes, 'first last_one')
}
