#ifndef V_CLOSURE_FREESTANDING_H
#define V_CLOSURE_FREESTANDING_H

#include <stddef.h>

#if defined(__linux__) && (defined(__x86_64__) || defined(__aarch64__))
void *v_closure_map(size_t size);
int v_closure_protect(void *base, size_t size, int protection);
size_t v_closure_page_size(void);

static inline void v_closure_flush_code(void *base, size_t size) {
	__builtin___clear_cache((char *)base, (char *)base + size);
}
#endif

#if defined(__APPLE__) && (defined(__aarch64__) || defined(__x86_64__))
#include <mach/mach.h>
#include <mach/mach_vm.h>

/* V's existing thunk ABI reads mutable metadata exactly 16 KiB behind
 * its program counter. Remap an immutable page from our signed text beside a
 * separate RW metadata page. No JIT entitlement or write/execute transition is
 * necessary. Mach remapping is adapted from libffi's Darwin tables (MIT):
 * https://github.com/libffi/libffi/blob/v3.4.6/src/closures.c
 * See LICENSE-libffi.txt. Uses 64-bit APIs and requires immutable RX protection.
 * The instructions below are V's existing closure_thunk/get_data templates.
 */
#if defined(__aarch64__)
#define V_CLOSURE_STATIC_ALIGN ".p2align 14\n"
#define V_CLOSURE_STATIC_ACCESSOR \
	".byte 0x20,0x02,0x66,0x9e,0xc0,0x03,0x5f,0xd6\n.space 8\n"
#define V_CLOSURE_STATIC_THUNK \
	".byte 0x11,0x00,0xfe,0x5c,0x30,0x00,0xfe,0x58\n" \
	".byte 0x00,0x02,0x1f,0xd6\n.space 4\n"
#else
#define V_CLOSURE_STATIC_ALIGN ".p2align 12\n"
#define V_CLOSURE_STATIC_ACCESSOR \
	".byte 0x66,0x4c,0x0f,0x7e,0xf8,0xc3\n.space 10\n"
#define V_CLOSURE_STATIC_THUNK \
	".byte 0xf3,0x44,0x0f,0x7e,0x3d,0xf7,0xbf,0xff,0xff\n" \
	".byte 0xff,0x25,0xf9,0xbf,0xff,0xff\n.space 1\n"
#endif
__asm__(
	".section __TEXT,__text,regular,pure_instructions\n"
	V_CLOSURE_STATIC_ALIGN
	".globl _v_closure_static_templates\n"
	".private_extern _v_closure_static_templates\n"
	".weak_definition _v_closure_static_templates\n"
	"_v_closure_static_templates:\n"
	V_CLOSURE_STATIC_ACCESSOR
	".rept 1023\n"
	V_CLOSURE_STATIC_THUNK
	".endr\n"
	V_CLOSURE_STATIC_ALIGN
);
#undef V_CLOSURE_STATIC_ALIGN
#undef V_CLOSURE_STATIC_ACCESSOR
#undef V_CLOSURE_STATIC_THUNK
/* Hidden weak definitions coalesce when several generated C translation units
 * include this header, while retaining a supported symbol-based Mach relocation. */
extern const unsigned char v_closure_static_templates[] __attribute__((visibility("hidden")));

static inline void *v_closure_map_static_templates(size_t page_size) {
	if (page_size != 16384) return NULL;
	mach_vm_address_t base = 0;
	kern_return_t result = mach_vm_allocate(mach_task_self(), &base,
		2 * page_size, VM_FLAGS_ANYWHERE);
	if (result != KERN_SUCCESS) return NULL;
	mach_vm_address_t code = base + page_size;
	vm_prot_t current, maximum;
	result = mach_vm_remap(mach_task_self(), &code, page_size, 0,
		VM_FLAGS_FIXED | VM_FLAGS_OVERWRITE, mach_task_self(),
		(mach_vm_address_t)v_closure_static_templates, FALSE,
		&current, &maximum, VM_INHERIT_SHARE);
	if (result != KERN_SUCCESS || (current & (VM_PROT_READ | VM_PROT_EXECUTE)) !=
		(VM_PROT_READ | VM_PROT_EXECUTE) || (current & VM_PROT_WRITE)) {
		mach_vm_deallocate(mach_task_self(), base, 2 * page_size);
		return NULL;
	}
	return (void *)base;
}
#endif
#endif
