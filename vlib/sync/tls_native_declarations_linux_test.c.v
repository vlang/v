import sync

#include <pthread.h>

// Translated C code may declare the native Linux pthread key signatures.
// Importing sync must not introduce incompatible u64 parameter declarations.
fn C.pthread_key_create(key voidptr, destructor voidptr) i32
fn C.pthread_key_delete(key u32) i32
fn C.pthread_setspecific(key u32, const_ptr voidptr) i32
fn C.pthread_getspecific(key u32) voidptr

fn test_native_pthread_declarations_coexist_with_sync_tls() ! {
	mut native_key := u32(0)
	assert C.pthread_key_create(&native_key, unsafe { nil }) == 0
	defer { assert C.pthread_key_delete(native_key) == 0 }
	mut value := 17
	assert C.pthread_setspecific(native_key, &value) == 0
	assert C.pthread_getspecific(native_key) == voidptr(&value)

	mut tls := sync.new_tls[int](23)!
	defer { tls.destroy() or { panic(err) } }
	assert tls.get()! == 23
	tls.set(42)!
	assert tls.get()! == 42
	assert C.pthread_getspecific(native_key) == voidptr(&value)
}
