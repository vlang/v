module modulecache

// The types layer of the declaration header subset: struct, union and enum definitions,
// typedefs, forward declarations and generated constant macros go when neither the module
// code nor an item that stays names them, transitively. Each test names the way a module
// can depend on a type without the type's name in its code, and what the index must keep.

const types_header = '#include <stdint.h>
#define MAKE_POINT() ((Point){1, 2})
#define limits__max 16
#define limits__unused 99
#define SYSTEM_FEATURE 1
typedef struct Point Point;
typedef struct Line Line;
typedef struct Unused Unused;
typedef struct Holder Holder;
typedef struct Callbacks Callbacks;
typedef struct Opaque Opaque;
typedef void (*visit_fn)(Point* p, Line l);
typedef int64_t counter_t;
typedef enum {
	Color__red = 0,
	Color__green = 1,
} Color;
struct Point { int x; int y; };
struct Line { Point a; Point b; };
struct Unused { counter_t n; };
struct Holder { Opaque* data; int tag; };
struct Callbacks { visit_fn visit; };
struct Opaque { int key_values; };
typedef union Bits Bits;
union Bits { uint32_t u; float f; };
Point make_point(void);
int use_color(Color c);
int use_unused(Unused* u);
Holder* get_holder(void);
int run(Callbacks* c);
static inline int in_static(Bits b) { return b.u; }
int after_static(void);
'

fn test_a_type_that_nothing_names_goes() {
	out := new_declaration_index_at(types_header, .macros).subset('int f(void) { return 1; }')
	assert !out.contains('struct Unused')
	assert !out.contains('Unused* u')
	assert !out.contains('typedef struct Unused Unused;')
	assert !out.contains('Color__green')
	assert !out.contains('typedef int64_t counter_t;')
	assert !out.contains('limits__unused')
	// the order and the rest of the header stay
	assert out.starts_with('#include <stdint.h>\n')
	assert out.contains('#define SYSTEM_FEATURE 1')
}

fn test_a_type_named_only_through_a_macro_stays() {
	// the code says MAKE_POINT(), never Point; the macro stays, so Point and its body do
	out := new_declaration_index_at(types_header, .macros).subset('int f(void) { Point_unused_marker(); return MAKE_POINT().x; }')
	assert out.contains('typedef struct Point Point;')
	assert out.contains('struct Point { int x; int y; };')
}

fn test_a_struct_body_keeps_the_types_of_its_fields_even_through_a_function_pointer() {
	// Callbacks names visit_fn, visit_fn names Point and Line, Line names Point by value
	out := new_declaration_index_at(types_header, .macros).subset('int f(Callbacks* c) { return run(c); }')
	for want in ['struct Callbacks { visit_fn visit; };', 'typedef void (*visit_fn)(Point* p, Line l);',
		'struct Line { Point a; Point b; };', 'struct Point { int x; int y; };',
		'typedef struct Line Line;', 'int run(Callbacks* c);'] {
		assert out.contains(want), want
	}
	assert !out.contains('struct Unused')
	assert !out.contains('struct Holder')
}

fn test_an_enum_constant_used_in_code_keeps_the_enum_defined_later() {
	// the constant is in the code, the enum definition is a later unit and has a name of
	// its own that the code does not use
	out := new_declaration_index_at(types_header, .macros).subset('int f(void) { return Color__green; }')
	assert out.contains('Color__green = 1,')
	assert out.contains('} Color;')
	assert out.contains('Color__red = 0,')
}

fn test_a_type_that_code_reaches_only_through_a_pointer_keeps_its_body() {
	// c->data->key_values: the code names Holder (through get_holder) and a field, never
	// Opaque. A forward declaration of Opaque would not compile, so the body stays.
	out := new_declaration_index_at(types_header, .macros).subset('int f(void) { return get_holder()->data->key_values; }')
	assert out.contains('struct Holder { Opaque* data; int tag; };')
	assert out.contains('typedef struct Opaque Opaque;')
	assert out.contains('struct Opaque { int key_values; };')
}

fn test_a_prototype_keeps_the_types_of_its_signature() {
	out := new_declaration_index_at(types_header, .macros).subset('int f(void) { return use_unused(0); }')
	assert out.contains('int use_unused(Unused* u);')
	assert out.contains('struct Unused { counter_t n; };')
	assert out.contains('typedef int64_t counter_t;')
}

fn test_generated_constant_macros_go_and_other_macros_stay() {
	out := new_declaration_index_at(types_header, .macros).subset('int f(void) { return limits__max; }')
	assert out.contains('#define limits__max 16')
	assert !out.contains('limits__unused')
	assert out.contains('#define SYSTEM_FEATURE 1')
	assert out.contains('#define MAKE_POINT()')
	assert is_generated_constant_macro('time__hour')
	assert !is_generated_constant_macro('_GNU_SOURCE')
	assert !is_generated_constant_macro('__USE_MISC')
	assert !is_generated_constant_macro('MBEDTLS_SSL_H')
	assert !is_generated_constant_macro('V_WY_LOAD8')
	assert !is_generated_constant_macro('stdout')
}

fn test_static_functions_go_only_in_the_statics_layer() {
	code := 'int f(void) { return 0; }'
	macros := new_declaration_index_at(types_header, .macros).subset(code)
	statics := new_declaration_index_at(types_header, .statics).subset(code)
	// a static function is emitted by TinyCC even when nothing calls it, so removing it
	// changes the object: the default layer keeps it, with the type it names
	assert macros.contains('static inline int in_static(Bits b)')
	assert macros.contains('union Bits { uint32_t u; float f; };')
	assert !statics.contains('in_static')
	assert !statics.contains('union Bits')
	called := new_declaration_index_at(types_header, .statics).subset('int f(void) { return in_static(0); }')
	assert called.contains('static inline int in_static(Bits b)')
	assert called.contains('union Bits')
}

fn test_the_declarations_layer_keeps_every_type() {
	out := new_declaration_index_at(types_header, .declarations).subset('int f(void) { return 1; }')
	assert out.contains('struct Unused { counter_t n; };')
	assert out.contains('limits__unused')
	assert !out.contains('int use_unused')
}

fn test_a_token_paste_anywhere_keeps_the_types() {
	header := types_header + '#define MAKE(name) name##_t\n'
	idx := new_declaration_index_at(header, .macros)
	out := idx.subset('int f(void) { return 1; }')
	// the identifiers cannot say what MAKE(x) names: only the declarations layer applies
	assert out.contains('struct Unused { counter_t n; };')
	assert out.contains('limits__unused')
	// and the same for a paste in the code
	plain := new_declaration_index_at(types_header, .macros)
	assert plain.subset('#define P(x) x##_t\nint f(void) { return 1; }').contains('struct Unused')
}

fn test_the_subset_is_a_subsequence_of_the_header() {
	for code in ['', 'int f(void) { return Color__green + MAKE_POINT().x; }',
		'int f(void) { return run(0) + use_color(0); }', 'int f(void) { return in_static(0); }'] {
		out := new_declaration_index_at(types_header, .macros).subset(code)
		header_lines := types_header.split_into_lines()
		out_lines := out.split_into_lines()
		mut j := 0
		for line in header_lines {
			if j < out_lines.len && out_lines[j] == line {
				j++
			}
		}
		assert j == out_lines.len, code
	}
}

fn test_every_shape_the_generator_writes_is_told_apart() {
	header := 'typedef struct a a;
typedef union b b;
struct c;
typedef struct { int z; } d;
typedef enum { e_one, e_two = 2, e_three } e;
enum { f_const = 5 };
typedef int g[4];
typedef void* (*h)(int);
typedef struct __v_result_i { intptr_t ok; union { int err; int value; }; } __v_result_i;
static void s(void) { }
extern int x;
int p(int);
'
	idx := new_typed_index(header, .statics)
	mut defined := map[string]ItemKind{}
	for item in idx.items {
		for name in item.defs {
			defined[name] = item.kind
		}
	}
	assert defined['a'] == .fwd
	assert defined['b'] == .fwd
	assert defined['c'] == .fwd
	assert defined['d'] == .body
	assert defined['e'] == .body
	assert defined['e_two'] == .body
	assert defined['e_three'] == .body
	assert defined['f_const'] == .body
	assert defined['g'] == .typedef
	assert defined['h'] == .typedef
	assert defined['__v_result_i'] == .body
	assert defined['s'] == .static_fn
	assert defined['x'] == .decl
	assert defined['p'] == .decl
}

fn test_a_shape_that_is_not_known_stays() {
	header := 'typedef struct a a;
struct a { int x; };
__attribute__((constructor)) static void init_it(void) { }
_Static_assert(sizeof(a) == 4, "size");
static const int table[2] = { 1, 2 };
int p(int);
'
	out := new_declaration_index_at(header, .statics).subset('')
	assert out.contains('init_it')
	assert out.contains('_Static_assert(sizeof(a) == 4, "size");')
	assert out.contains('static const int table')
	// the assert names a, so a stays
	assert out.contains('struct a { int x; };')
	assert !out.contains('int p(int);')
}
