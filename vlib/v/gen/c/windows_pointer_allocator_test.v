module c

import os

fn allocator_test_c_function(generated string, signature string) string {
	assert generated.contains(signature), signature
	return signature + generated.all_after(signature).all_before('\n}') + '\n}\n'
}

fn allocator_test_c_drop_helpers(generated string, names []string) string {
	mut prototypes := []string{}
	mut bodies := []string{}
	for name in names {
		prefix := 'static void __v3_ownership_drop_${name}('
		definitions := generated.split_into_lines().filter(it.starts_with(prefix)
			&& it.ends_with(' {'))
		assert definitions.len == 1, '${name}: ${definitions}'
		signature := definitions[0]
		prototypes << signature.all_before(' {') + ';\n'
		bodies << allocator_test_c_function(generated, signature)
	}
	// Nested generic helpers can precede their dependencies in generated C.
	return prototypes.join('') + bodies.join('')
}

fn allocator_test_c_struct(generated string, suffix string) string {
	for line in generated.split_into_lines() {
		if line.starts_with('struct ') && line.ends_with('${suffix} {') {
			name := line.all_after('struct ').all_before(' {')
			tail := generated.all_after(line + '\n')
			body := tail.all_before('\n}')
			closing := tail.all_after('\n}').all_before('\n')
			return 'typedef struct ${name} ${name};\n${line}\n${body}\n}${closing}\n'
		}
	}
	assert false, 'missing generated struct ${suffix}'
	return ''
}

fn allocator_test_mock_runtime(declarations string) string {
	return '#include <assert.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define _WIN32 1
#define _aligned_malloc mock_aligned_malloc
#define _aligned_free mock_aligned_free
typedef unsigned char u8;
typedef unsigned int u32;
typedef int64_t i64;
typedef struct { void* _object; } IError;
static IError builtin__none__, builtin__error_sentinel;
${declarations}
static void* live[16];
static void* bases[16];
static int live_count, aligned_allocations, aligned_frees, crt_frees;
static void* _aligned_malloc(size_t size, size_t alignment) {
	if (alignment < sizeof(void*)) alignment = sizeof(void*);
	void* base = malloc(size + alignment - 1);
	assert(base != NULL && live_count < 16);
	void* pointer = (void*)(((uintptr_t)base + alignment - 1) & ~(alignment - 1));
	live[live_count] = pointer; bases[live_count++] = base;
	aligned_allocations++;
	return pointer;
}
static void _aligned_free(void* pointer) {
	if (pointer == NULL) return;
	int index = 0;
	while (index < live_count && live[index] != pointer) index++;
	assert(index < live_count);
	free(bases[index]);
	live[index] = live[--live_count]; bases[index] = bases[live_count];
	aligned_frees++;
}
static void crt_free(void* pointer) {
	for (int index = 0; index < live_count; index++) assert(live[index] != pointer);
	free(pointer); crt_frees++;
}
#define free crt_free
#define _memory_panic(where, size) abort()
static void* vcalloc(size_t size) { return _aligned_malloc(size, 1); }
'
}

fn test_windows_builtin_and_compiler_promoted_pointers_keep_the_v_allocator_family() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_pointer_allocator_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c.v')
	c_source := os.join_path(dir, 'main.c')
	os.write_file(source, 'fn C.malloc(size uint) voidptr
fn C.free(value voidptr)
@[aligned: 64]
struct AlignedCell { value int }
struct Holder { values [2]AlignedCell }
@[aligned: 8]
struct NaturalCell { value int }
struct NaturalHolder { values [2]NaturalCell }
fn retain(mut values []AlignedCell) []AlignedCell { return values }
fn promoted() &Holder {
	holder := Holder{}
	_ := retain(mut holder.values)
	return &holder
}
fn release_natural_v(value &NaturalHolder) { unsafe { free(value) } }
fn (value &NaturalHolder) free() { unsafe { free(value) } }
fn release_natural_method(value &NaturalHolder) { unsafe { value.free() } }
fn (value &Holder) free() { unsafe { free(value) } }
fn release_method(value &Holder) { unsafe { value.free() } }
fn release_c(value &NaturalHolder) { unsafe { C.free(value) } }
fn main() {
	ordinary := unsafe { &NaturalHolder(malloc(int(sizeof(NaturalHolder)))) }
	release_natural_v(ordinary)
	seed := NaturalHolder{}
	copied := unsafe { &NaturalHolder(memdup(&seed, int(sizeof(NaturalHolder)))) }
	release_natural_method(copied)
	compiler_owned := promoted()
	release_method(compiler_owned)
	foreign := unsafe { &NaturalHolder(C.malloc(sizeof(NaturalHolder))) }
	release_c(foreign)
}
')!
	generated_result := os.exec([@VEXE, '-new-compiler', '-nocache', '-os', 'windows', '-gc', 'none',
		'-manualfree', '-o', c_source, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(c_source)!
	allocate := allocator_test_c_function(generated, 'u8* v_malloc(i64 n) {')
	copy := allocator_test_c_function(generated, 'void* memdup(void* src, i64 sz) {')
	deallocate := allocator_test_c_function(generated, 'void v_free(void* ptr) {')
	assert allocate.contains('_aligned_malloc(n, 1)'), allocate
	assert copy.contains('v_malloc(sz)'), copy
	assert deallocate.contains('_aligned_free(ptr)'), deallocate
	promoted := allocator_test_c_function(generated, 'main__Holder* promoted(void) {')
	promoted_allocation := promoted.split_into_lines()[1].trim_space()
	assert promoted_allocation.contains('v3_aligned_memdup('), promoted_allocation
	assert promoted_allocation.contains('__alignof__(main__Holder)'), promoted_allocation
	// Run the emitted Windows allocator paths with CRT mocks that reject crossed
	// allocation families, without requiring a Windows SDK on the test host.
	mock_source := os.join_path(dir, 'allocator_mock.c')
	mock_binary := os.join_path(dir, 'allocator_mock.exe')
	os.write_file(mock_source, '${allocator_test_mock_runtime('typedef struct __attribute__((aligned(64))) { int values[32]; } main__Holder;
typedef struct __attribute__((aligned(8))) { int value; } main__NaturalCell;
typedef struct { main__NaturalCell values[2]; } main__NaturalHolder;
')}
${allocate}
${copy}
${deallocate}
${allocator_test_c_function(generated, 'static inline void* v3_aligned_memdup(void* src, ptrdiff_t sz, size_t alignment) {')}
${allocator_test_c_function(generated, 'static inline void v3_aligned_free(void* p) {')}
${allocator_test_c_function(generated, 'void release_natural_v(main__NaturalHolder* value) {')}
${allocator_test_c_function(generated, 'void NaturalHolder__free(main__NaturalHolder* value) {')}
${allocator_test_c_function(generated, 'void release_natural_method(main__NaturalHolder* value) {')}
${allocator_test_c_function(generated, 'void Holder__free(main__Holder* value) {')}
${allocator_test_c_function(generated, 'void release_method(main__Holder* value) {')}
${allocator_test_c_function(generated, 'void release_c(main__NaturalHolder* value) {')}
int main(int argc, char** argv) {
	(void)argv;
	main__NaturalHolder seed = {{{37}}};
	main__NaturalHolder* ordinary = (main__NaturalHolder*)v_malloc(sizeof(seed));
	main__NaturalHolder* copied = (main__NaturalHolder*)memdup(&seed, sizeof(seed));
	${promoted_allocation}
	assert((uintptr_t)holder % 64 == 0);
	assert(copied->values[0].value == 37);
	if (argc == 2) { release_c((main__NaturalHolder*)ordinary); return 0; }
	if (argc == 3) { release_natural_v((main__NaturalHolder*)malloc(sizeof(seed))); return 0; }
	release_natural_v(ordinary);
	release_natural_method(copied);
	release_method(holder);
	release_c((main__NaturalHolder*)malloc(sizeof(main__NaturalHolder)));
	assert(live_count == 0 && aligned_allocations == 3 && aligned_frees == 3);
	assert(crt_frees == 1);
	return 0;
}
')!
	build := os.exec([cc, '-std=gnu11', mock_source, '-o', mock_binary])
	assert build.exit_code == 0, build.output
	run := os.exec([mock_binary])
	assert run.exit_code == 0, run.output
	for arguments in [['wrong-c-free'], ['wrong-v-free', 'extra']] {
		crossed := os.exec([mock_binary, ...arguments])
		assert crossed.exit_code != 0, 'the allocator mock accepted crossed allocation families'
	}
}

fn test_windows_implicit_pointer_sum_drops_keep_the_v_allocator_family() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_sum_pointer_allocator_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	c_source := os.join_path(dir, 'main.c')
	ownership_compiler := os.join_path(dir, 'v-ownership')
	vlib := os.join_path(@VEXEROOT, 'vlib')
	build_compiler := os.exec([@VEXE, '-new-compiler', '-b', 'c', '-cc', cc, '-gc', 'none', '-d',
		'ownership', '-path', vlib, '-o', ownership_compiler, os.join_path(vlib, 'v', 'v.v')])
	assert build_compiler.exit_code == 0, build_compiler.output
	os.write_file(source, '@[aligned: 64]
struct AlignedCell { value int }
struct NaturalCell { value int }
type AlignedValue = int | &AlignedCell
type NaturalValue = int | &NaturalCell
struct AlignedWrapper { value AlignedValue }
type GenericAlignedValue[T] = int | &T
type GenericAlignedLayer[T] = GenericAlignedValue[T] | bool
struct GenericAlignedWrapper[T] { value GenericAlignedValue[T] }
struct GenericAlignedLayerWrapper[T] { value GenericAlignedLayer[T] }
fn implicit_aligned() {
 value := AlignedValue(&AlignedCell{value: 17})
 _ := value
}
fn implicit_natural() {
 value := NaturalValue(&NaturalCell{value: 23})
 _ := value
}
fn implicit_wrapped_aligned() {
 value := AlignedWrapper{value: AlignedValue(&AlignedCell{value: 29})}
 _ := value
}
fn implicit_borrowed_wrapper(seed &AlignedCell) {
 value := AlignedWrapper{value: AlignedValue(seed)}
 _ := value
}
fn implicit_generic_aligned() {
 value := GenericAlignedWrapper[AlignedCell]{value: GenericAlignedValue[AlignedCell](&AlignedCell{value: 41})}
 _ := value
}
fn implicit_generic_nested_aligned() {
 value := GenericAlignedLayerWrapper[AlignedCell]{value: GenericAlignedLayer[AlignedCell](GenericAlignedValue[AlignedCell](&AlignedCell{value: 43}))}
 _ := value
}
fn implicit_generic_borrowed_wrapper(seed &AlignedCell) {
 value := GenericAlignedWrapper[AlignedCell]{value: GenericAlignedValue[AlignedCell](seed)}
 _ := value
}
fn main() { implicit_aligned()
 implicit_natural()
 implicit_wrapped_aligned()
 seed := AlignedCell{value: 31}
 implicit_borrowed_wrapper(&seed)
 implicit_generic_aligned()
 implicit_generic_nested_aligned()
 implicit_generic_borrowed_wrapper(&seed) }
')!
	result := os.exec([ownership_compiler, '-new-compiler', '-nocache', '-os', 'windows', '-gc',
		'none', '-autofree', '-no-closures', '-o', c_source, source])
	assert result.exit_code == 0, result.output
	generated := os.read_file(c_source)!
	root_helpers := ['AlignedValue', 'NaturalValue', 'AlignedValue', 'GenericAlignedValue_AlignedCell',
		'GenericAlignedLayer_AlignedCell']
	for i, name in ['main__implicit_aligned', 'main__implicit_natural', 'main__implicit_wrapped_aligned',
		'main__implicit_generic_aligned', 'main__implicit_generic_nested_aligned'] {
		body := allocator_test_c_function(generated, 'void ${name}(void) {')
		assert body.contains('._pointer_variant_is_owned = true'), body
		assert body.count('__v3_ownership_drop_${root_helpers[i]}(') == 1, body
		assert body.count('__v3_ownership_drop_') == 1, body
		assert !body.contains('\n\tfree('), body
	}
	for name in ['AlignedValue', 'NaturalValue', 'GenericAlignedValue_AlignedCell'] {
		signature := 'static void __v3_ownership_drop_${name}(${name}* _value) {'
		body := allocator_test_c_function(generated, signature)
		assert body.contains('_pointer_variant_is_owned &&'), body
		assert body.contains('!= NULL)'), body
		assert body.count('v3_aligned_free(') == 1, body
		assert body.count('free(') == 1, body
	}
	// Run the real shared cleanup closure, including wrappers and nested sums.
	drop_helpers := allocator_test_c_drop_helpers(generated, ['AlignedCell', 'NaturalCell',
		'AlignedValue', 'NaturalValue', 'AlignedWrapper', 'GenericAlignedValue_AlignedCell',
		'GenericAlignedLayer_AlignedCell', 'GenericAlignedWrapper_AlignedCell',
		'GenericAlignedLayerWrapper_AlignedCell'])
	declarations := allocator_test_c_struct(generated, 'AlignedCell') +
		allocator_test_c_struct(generated, 'NaturalCell') +
		allocator_test_c_struct(generated, 'AlignedValue') +
		allocator_test_c_struct(generated, 'NaturalValue') +
		allocator_test_c_struct(generated, 'AlignedWrapper') +
		allocator_test_c_struct(generated, 'GenericAlignedValue_AlignedCell') +
		allocator_test_c_struct(generated, 'GenericAlignedLayer_AlignedCell') +
		allocator_test_c_struct(generated, 'GenericAlignedWrapper_AlignedCell') +
		allocator_test_c_struct(generated, 'GenericAlignedLayerWrapper_AlignedCell')
	mock_source := os.join_path(dir, 'implicit_sum_mock.c')
	mock_binary := os.join_path(dir, 'implicit_sum_mock.exe')
	os.write_file(mock_source, '${allocator_test_mock_runtime(declarations)}
${allocator_test_c_function(generated, 'u8* v_malloc(i64 n) {')}
${allocator_test_c_function(generated, 'void* memdup(void* src, i64 sz) {')}
${allocator_test_c_function(generated, 'void v_free(void* ptr) {')}
${allocator_test_c_function(generated, 'static inline void* v3_aligned_memdup(void* src, ptrdiff_t sz, size_t alignment) {')}
${allocator_test_c_function(generated, 'static inline void v3_aligned_free(void* p) {')}
${drop_helpers}
${allocator_test_c_function(generated, 'void main__implicit_aligned(void) {')}
${allocator_test_c_function(generated, 'void main__implicit_natural(void) {')}
${allocator_test_c_function(generated, 'void main__implicit_wrapped_aligned(void) {')}
${allocator_test_c_function(generated, 'void main__implicit_borrowed_wrapper(main__AlignedCell* seed) {')}
${allocator_test_c_function(generated, 'void main__implicit_generic_aligned(void) {')}
${allocator_test_c_function(generated, 'void main__implicit_generic_nested_aligned(void) {')}
${allocator_test_c_function(generated, 'void main__implicit_generic_borrowed_wrapper(main__AlignedCell* seed) {')}
int main(void) {
 main__implicit_aligned();
 assert(live_count == 0 && aligned_allocations == 1 && aligned_frees == 1);
 main__implicit_natural();
 assert(live_count == 0 && aligned_allocations == 2 && aligned_frees == 2);
 assert(crt_frees == 0);
 main__implicit_wrapped_aligned();
 assert(live_count == 0 && aligned_allocations == 3 && aligned_frees == 3);
 main__AlignedCell seed = {.value = 31};
 main__implicit_borrowed_wrapper(&seed);
 assert(seed.value == 31 && live_count == 0 && aligned_allocations == 3 && aligned_frees == 3);
 assert(crt_frees == 0);
 main__implicit_generic_aligned();
 assert(live_count == 0 && aligned_allocations == 4 && aligned_frees == 4);
 main__implicit_generic_nested_aligned();
 assert(live_count == 0 && aligned_allocations == 5 && aligned_frees == 5);
 main__implicit_generic_borrowed_wrapper(&seed);
 assert(seed.value == 31 && live_count == 0 && aligned_allocations == 5 && aligned_frees == 5);
 assert(crt_frees == 0);
 return 0;
}
')!
	build := os.exec([cc, '-std=gnu11', mock_source, '-o', mock_binary])
	assert build.exit_code == 0, build.output
	run := os.exec([mock_binary])
	assert run.exit_code == 0, run.output
}
