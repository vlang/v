module driver

fn hazards_in(text string) []string {
	source := '# 1 "/tmp/gen.h"\n# 1 "/src/native.h" 1\n' + text + '\n# 2 "/tmp/gen.h" 2\n'
	return v3_parallel_c_preprocessed_hazards(source, ['/tmp/gen.h']).map(it.reason)
}

fn test_declarations_types_and_internal_functions_are_safe_to_repeat() {
	assert hazards_in('typedef struct Point { int x; int y; } Point;
struct Forward;
struct Node { struct Node *next; };
enum { A, B };
extern int shared_counter;
int add(int a, int b);
void (*optional_callback(void))(int);
static inline int twice(int x) { return x * 2; }
static int helper(int x) { if (x) { return 1; } return 0; }
inline int plain_inline(int x) { return x; }
static const int table[3] = { 1, 2, 3 };
static const char *const names[] = { "a", "b" };
_Static_assert(sizeof(int) == 4, "int");
__attribute ((weak)) void fallback_hook(volatile void *p) { (void)p; }
__attribute__((weak)) int weak_default = 1;') == []
}

fn test_mutable_static_storage_is_reported() {
	assert hazards_in('static int counter;') == [
		'mutable static `counter` would get a separate copy in every unit',
	]
	assert hazards_in('static struct State { int ready; } state = { 0 };') == [
		'mutable static `state` would get a separate copy in every unit',
	]
	assert hazards_in('static void *(*hook)(void *);') == [
		'mutable static `hook` would get a separate copy in every unit',
	]
	// A pointer to read-only data can still be assigned.
	assert hazards_in('static const char *configured_names;') == [
		'mutable static `configured_names` would get a separate copy in every unit',
	]
	assert hazards_in('static struct Lock { int word; } lock = { 0 };') == [
		'mutable static `lock` would get a separate copy in every unit',
	]
}

fn test_static_locals_of_repeated_functions_are_reported() {
	assert hazards_in('static int next_id(void) { static int id = 0; return ++id; }') == [
		'`next_id` has the static local `id`, which every unit would copy',
	]
	assert hazards_in('static const char *name(void) { static const char text[] = "x"; return text; }') == []
}

fn test_external_definitions_are_reported() {
	assert hazards_in('int global_value = 3;') == [
		'external object `global_value` would be defined in every unit',
	]
	assert hazards_in('int compute(int x) { return x + 1; }') == [
		'external function `compute` would be defined in every unit',
	]
	assert hazards_in('@interface Box : NSObject\n- (void)run;\n@end\n@implementation Box\n- (void)run { }\n@end') == [
		'Objective-C @implementation `Box` would be defined in every unit',
	]
}

fn test_system_and_generated_header_items_are_not_reported() {
	source := '# 1 "/tmp/gen.h"\nstatic int generated_state;\n# 1 "/usr/include/stdio.h" 1 3 4\nstatic int system_state;\n# 3 "/tmp/gen.h" 2\n'
	assert v3_parallel_c_preprocessed_hazards(source, ['/tmp/gen.h']) == []
}

fn test_hazards_name_the_native_header_and_line() {
	source := '# 1 "/tmp/gen.h"\n# 1 "/src/bridge.h" 1\nint ok(void);\n\nstatic int state;\n# 2 "/tmp/gen.h" 2\n'
	hazards := v3_parallel_c_preprocessed_hazards(source, ['/tmp/gen.h'])
	assert hazards.len == 1
	assert hazards[0].str() == '/src/bridge.h:3: mutable static `state` would get a separate copy in every unit'
}

fn test_attributes_and_strings_do_not_confuse_the_scanner() {
	assert hazards_in('static inline __attribute__((always_inline)) int f(void) { return "}"[0]; }
static const char brace = \'{\';
__attribute__((visibility("hidden"))) int prototype(void);') == []
}

// owner_names_between compares a first unit and the declaration header that the
// other units include, both shaped like `clang -E -dD` output, for a body that
// uses `used`.
fn owner_names_between(owner string, header string, used []string) []string {
	mut used_names := map[string]bool{}
	for name in used {
		used_names[name] = true
	}
	mut owner_scan := V3ParallelCScan{}
	owner_scan.run(owner, used_names)
	mut header_scan := V3ParallelCScan{}
	header_scan.run(header, used_names)
	mut names := v3_parallel_c_owner_names(owner_scan, header_scan).keys()
	names.sort()
	return names
}

const shared_declarations = '# 1 "/src/bridge.h" 1
typedef struct Shared { int width; } Shared;
void bridge_call(int value);
#define BRIDGE_LIMIT 4
#define BRIDGE_SCALE 2
# 2 "/tmp/unit.c" 2
'

fn test_owner_names_are_what_only_the_first_unit_declares() {
	owner := '# 1 "/tmp/unit_0.c"\n' + shared_declarations + '# 1 "/src/native.m" 1
#define v_owner_macro(x) (x)
static void native_helper(int width, int height) { }
void native_draw(float x, float y);
void native_draw(float x, float y) { native_helper(1, 2); }
void bridge_call(int value) { }
enum NativeMode { native_mode_a = 1, native_mode_b };
typedef struct NativeBox { int width; } NativeBox, *NativeBoxRef;
static int native_counter = 0, native_other;
@class NativeLayer, NativeWindow;
# 4 "/tmp/unit_0.c" 2
'
	header := '# 1 "/tmp/unit.c"\n' + shared_declarations
	assert owner_names_between(owner, header, ['native_draw', 'native_helper', 'native_mode_b',
		'NativeBoxRef', 'native_other', 'NativeWindow', 'v_owner_macro', 'bridge_call', 'Shared',
		'width', 'BRIDGE_LIMIT']) == ['NativeBoxRef', 'NativeWindow', 'native_draw', 'native_helper',
		'native_mode_b', 'native_other', 'v_owner_macro']
	// Names that the body does not use are not collected at all.
	assert owner_names_between(owner, header, ['twice']) == []
}

fn test_owner_names_include_macros_that_differ_from_the_header() {
	owner := '# 1 "/tmp/unit_0.c"\n' + shared_declarations + '#undef BRIDGE_LIMIT
#define BRIDGE_LIMIT 8
#undef BRIDGE_SCALE
#define BRIDGE_SCALE 2
'
	// The other units would see BRIDGE_LIMIT as 4; BRIDGE_SCALE ends up the same,
	// and a macro that only the other units define means something else there too.
	header := '# 1 "/tmp/unit.c"\n' + shared_declarations + '#define ONLY_SHARED 1\n'
	assert owner_names_between(owner, header, ['BRIDGE_LIMIT', 'BRIDGE_SCALE', 'ONLY_SHARED']) == [
		'BRIDGE_LIMIT',
		'ONLY_SHARED',
	]
}

fn test_owner_names_of_implementation_sections_from_shared_directives() {
	// `#define SOKOL_APP_IMPL` before a shared include selects declarations that
	// only the first unit gets, since the declaration header drops the define.
	owner := '# 1 "/tmp/unit_0.c"\n# 1 "/src/app.h" 1\nvoid app_run(void);\n# 1 "/sdk/CGColor.h" 1 3\nextern __attribute__((visibility("default"))) void *CGColorCreateGenericRGB(double red, double green,\n    double blue, double alpha) __attribute__((availability(macos,introduced=10.5)));\n# 3 "/src/app.h" 2\n# 2 "/tmp/unit_0.c" 2\n'
	header := '# 1 "/tmp/unit.c"\n# 1 "/src/app.h" 1\nvoid app_run(void);\n# 2 "/tmp/unit.c" 2\n'
	assert owner_names_between(owner, header, ['app_run', 'CGColorCreateGenericRGB']) == [
		'CGColorCreateGenericRGB',
	]
}

fn test_objective_c_blocks_after_attributes_declare_only_their_class() {
	owner := '# 1 "/tmp/unit_0.c"
# 1 "/src/view.m" 1
__attribute__((visibility("default"))) @interface NativeView : NSObject {
	AVAudioNode *_node;
}
@property (nonatomic) float scale;
@required
- (void)draw;
@end
__attribute__((objc_root_class)) @implementation NativeView
- (void)draw { static int calls; calls++; }
@end
# 2 "/tmp/unit_0.c" 2
'
	assert owner_names_between(owner, '# 1 "/tmp/unit.c"\n', ['NativeView', '_node', 'scale', 'float',
		'required', 'draw', 'calls']) == ['NativeView']
	header := '# 1 "/tmp/gen.h"\n# 1 "/src/view.h" 1\n__attribute__((visibility("default"))) @implementation NativeView\n- (void)draw { }\n@end\n# 2 "/tmp/gen.h" 2\n'
	assert v3_parallel_c_preprocessed_hazards(header, ['/tmp/gen.h']).map(it.reason) == [
		'Objective-C @implementation `NativeView` would be defined in every unit',
	]
}

fn test_used_identifiers_skip_comments_literals_and_numbers() {
	mut used := v3_c_used_identifiers([
		'void f(void) { g(0x1f, "h", \'i\'); /* j */ }\n// k\n',
		'int l = 2e5;',
	]).keys()
	used.sort()
	assert used == ['f', 'g', 'int', 'l', 'void']
}

fn test_body_parts_using_owner_names_are_detected_by_identifier() {
	names := {
		'native_draw': true
	}
	assert v3_c_text_uses_any('void f(void) { native_draw(1, 2); }', names)
	assert !v3_c_text_uses_any('void f(void) { native_draw_rect(1, 2); my_native_draw(); }', names)
	assert !v3_c_text_uses_any('void f(void) { 0native_draw; }', names)
	assert !v3_c_text_uses_any('void f(void) { puts("native_draw"); /* native_draw */ }', names)
	assert v3_c_text_uses_any('#if defined(native_draw)\nint x;\n#endif', names)
	assert !v3_c_text_uses_any('void f(void) { native_draw(); }', map[string]bool{})
}

fn test_partition_moves_only_the_functions_that_use_owner_names() {
	names := {
		'native_draw': true
	}
	part := '/* V3CACHE_MODULE main */
void main__paint(int x) {
	if (x) { native_draw(x, "}"); }
}
string main__label(void) {
	return _S("native_draw(\\"{\\")");
}
/* V3CACHE_MODULE gfx */
int gfx__count(void) {
	// native_draw(
	return 0;
}
void gfx__flush(void) { char c = \'}\'; native_draw(c, 0); }
'
	owner, rest := v3_c_partition_items(part, names)
	assert owner == '/* V3CACHE_MODULE main */
void main__paint(int x) {
	if (x) { native_draw(x, "}"); }
}

void gfx__flush(void) { char c = \'}\'; native_draw(c, 0); }
'
	// A name in a literal or a comment is not a use.
	assert rest == '
string main__label(void) {
	return _S("native_draw(\\"{\\")");
}
/* V3CACHE_MODULE gfx */
int gfx__count(void) {
	// native_draw(
	return 0;
}
'
}
