module c

import os

fn allocator_test_c_function(generated string, signature string) string {
	assert generated.contains(signature), signature
	return signature + generated.all_after(signature).all_before('\n}') + '\n}\n'
}

fn test_windows_builtin_and_compiler_promoted_pointers_keep_the_v_allocator_family() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_pointer_allocator_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c.v')
	c_source := os.join_path(dir, 'main.c')
	os.write_file(source, 'fn C.malloc(size usize) voidptr
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
	ordinary := unsafe { &NaturalHolder(malloc(sizeof(NaturalHolder))) }
	release_natural_v(ordinary)
	seed := NaturalHolder{}
	copied := unsafe { &NaturalHolder(memdup(&seed, sizeof(NaturalHolder))) }
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
	allocate := allocator_test_c_function(generated, 'u8* v_malloc(ptrdiff_t n) {')
	copy := allocator_test_c_function(generated, 'void* memdup(void* src, ptrdiff_t sz) {')
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
	os.write_file(mock_source, '#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define _WIN32 1
#define _aligned_malloc mock_aligned_malloc
#define _aligned_free mock_aligned_free
typedef unsigned char u8;
typedef struct { void* _object; } IError;
static IError builtin__none__, builtin__error_sentinel;
typedef struct __attribute__((aligned(64))) { int values[32]; } main__Holder;
typedef struct __attribute__((aligned(8))) { int value; } main__NaturalCell;
typedef struct { main__NaturalCell values[2]; } main__NaturalHolder;
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
