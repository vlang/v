module stdatomic

// The @VEXEROOT/thirdparty/stdatomic contains compatibility headers
// for stdatomic, that supports both nix, windows and c++.

$if windows {
	#flag -I @VEXEROOT/thirdparty/stdatomic/win
	#insert "@VEXEROOT/thirdparty/stdatomic/win/atomic.h"
} $else {
	#flag -I @VEXEROOT/thirdparty/stdatomic/nix
	#insert "@VEXEROOT/thirdparty/stdatomic/nix/atomic.h"
}

$if linux {
	$if tinyc {
		$if amd64 {
			// Redhat/CentOS:
			#flag $when_first_existing('/usr/lib/gcc/x86_64-redhat-linux/6/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/7/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/8/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/9/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/10/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/11/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/12/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/13/libatomic.a','/usr/lib/gcc/x86_64-redhat-linux/14/libatomic.a')
			// Gentoo:
			#flag $when_first_existing('/usr/lib/gcc/x86_64-pc-linux-gnu/6/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/7/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/8/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/9/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/10/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/11/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/12/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/13/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-gnu/14/libatomic.a')
			// OpenSUSE:
			#flag $when_first_existing('/usr/lib64/gcc/x86_64-suse-linux/6/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/7/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/8/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/9/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/10/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/11/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/12/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/13/libatomic.a','/usr/lib64/gcc/x86_64-suse-linux/14/libatomic.a')
			// ALT Linux:
			#flag $when_first_existing('/usr/lib64/gcc/x86_64-alt-linux/6/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/7/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/8/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/9/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/10/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/11/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/12/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/13/libatomic.a','/usr/lib64/gcc/x86_64-alt-linux/14/libatomic.a')
			$if musl ? {
				// Alpine:
				#flag $when_first_existing('/usr/lib/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/6/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/7/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/8/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/9/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/10/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/11/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/12/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/13/libatomic.a','/usr/lib/gcc/x86_64-pc-linux-musl/14/libatomic.a')
			}
		} $else $if arm64 {
			// Note: Use `.so` instead of `.a`.
			// This is because `libatomic.a` atomic symbols, such as `__atomic_fetch_add_4`, are indirect(IFUNC) symbols:
			// This can not be handled correctly by `tcc` right now.

			// Symbol table '.symtab' contains 14 entries:
			//   Num:    Value          Size Type    Bind   Vis      Ndx Name
			//     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND
			//     1: 0000000000000000     0 SECTION LOCAL  DEFAULT    1
			//     2: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT    1 $x
			//     3: 0000000000000030    12 FUNC    LOCAL  DEFAULT    1 select_fetch_add_4
			//     4: 0000000000000040    12 FUNC    LOCAL  DEFAULT    1 select_add_fetch_4
			//     5: 0000000000000014     0 NOTYPE  LOCAL  DEFAULT    6 $d
			//     6: 0000000000000000     0 SECTION LOCAL  DEFAULT    3
			//     7: 0000000000000000     0 SECTION LOCAL  DEFAULT    4
			//     8: 0000000000000000     0 SECTION LOCAL  DEFAULT    5
			//     9: 0000000000000000     0 SECTION LOCAL  DEFAULT    6
			//    10: 0000000000000000    24 FUNC    GLOBAL HIDDEN     1 libat_fetch_add_4
			//    11: 0000000000000018    24 FUNC    GLOBAL HIDDEN     1 libat_add_fetch_4
			//    12: 0000000000000030    12 IFUNC   GLOBAL DEFAULT    1 __atomic_fetch_add_4
			//    13: 0000000000000040    12 IFUNC   GLOBAL DEFAULT    1 __atomic_add_fetch_4

			// Debian/Ubuntu:
			#flag $when_first_existing('/usr/lib/gcc/aarch64-linux-gnu/6/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/7/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/8/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/9/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/10/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/11/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/12/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/13/libatomic.so','/usr/lib/gcc/aarch64-linux-gnu/14/libatomic.so')
			// Redhat/CentOS:
			#flag $when_first_existing('/usr/lib/gcc/aarch64-redhat-linux/6/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/7/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/8/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/9/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/10/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/11/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/12/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/13/libatomic.so','/usr/lib/gcc/aarch64-redhat-linux/14/libatomic.so')
			// Gentoo:
			#flag $when_first_existing('/usr/lib/gcc/aarch64-pc-linux-gnu/6/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/7/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/8/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/9/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/10/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/11/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/12/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/13/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-gnu/14/libatomic.so')
			// OpenSUSE:
			#flag $when_first_existing('/usr/lib64/gcc/aarch64-suse-linux/6/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/7/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/8/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/9/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/10/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/11/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/12/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/13/libatomic.so','/usr/lib64/gcc/aarch64-suse-linux/14/libatomic.so')
			// ALT Linux:
			#flag $when_first_existing('/usr/lib64/gcc/aarch64-alt-linux/6/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/7/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/8/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/9/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/10/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/11/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/12/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/13/libatomic.so','/usr/lib64/gcc/aarch64-alt-linux/14/libatomic.so')
			$if musl ? {
				// Alpine:
				#flag $when_first_existing('/usr/lib/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/6/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/7/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/8/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/9/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/10/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/11/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/12/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/13/libatomic.so','/usr/lib/gcc/aarch64-pc-linux-musl/14/libatomic.so')
			}
		}
	}
}

// The following functions are actually generic in C
fn C.atomic_load_ptr(voidptr) voidptr
fn C.atomic_store_ptr(voidptr, voidptr)
fn C.atomic_compare_exchange_weak_ptr(voidptr, voidptr, isize) bool
fn C.atomic_compare_exchange_strong_ptr(voidptr, voidptr, isize) bool
fn C.atomic_exchange_ptr(voidptr, voidptr) voidptr
fn C.atomic_fetch_add_ptr(voidptr, voidptr) voidptr
fn C.atomic_fetch_sub_ptr(voidptr, voidptr) voidptr

fn C.atomic_load_byte(voidptr) u8
fn C.atomic_store_byte(voidptr, u8)
fn C.atomic_compare_exchange_weak_byte(voidptr, voidptr, u8) bool
fn C.atomic_compare_exchange_strong_byte(voidptr, voidptr, u8) bool
fn C.atomic_exchange_byte(voidptr, u8) u8
fn C.atomic_fetch_add_byte(voidptr, u8) u8
fn C.atomic_fetch_sub_byte(voidptr, u8) u8

fn C.atomic_load_u16(voidptr) u16
fn C.atomic_store_u16(voidptr, u16)
fn C.atomic_compare_exchange_weak_u16(voidptr, voidptr, u16) bool
fn C.atomic_compare_exchange_strong_u16(voidptr, voidptr, u16) bool
fn C.atomic_exchange_u16(voidptr, u16) u16
fn C.atomic_fetch_add_u16(voidptr, u16) u16
fn C.atomic_fetch_sub_u16(voidptr, u16) u16

fn C.atomic_load_u32(voidptr) u32
fn C.atomic_store_u32(voidptr, u32)
fn C.atomic_compare_exchange_weak_u32(voidptr, voidptr, u32) bool
fn C.atomic_compare_exchange_strong_u32(voidptr, voidptr, u32) bool
fn C.atomic_exchange_u32(voidptr, u32) u32
fn C.atomic_fetch_add_u32(voidptr, u32) u32
fn C.atomic_fetch_sub_u32(voidptr, u32) u32

fn C.atomic_load_u64(voidptr) u64
fn C.atomic_store_u64(voidptr, u64)
fn C.atomic_compare_exchange_weak_u64(voidptr, voidptr, u64) bool
fn C.atomic_compare_exchange_strong_u64(voidptr, voidptr, u64) bool
fn C.atomic_exchange_u64(voidptr, u64) u64
fn C.atomic_fetch_add_u64(voidptr, u64) u64
fn C.atomic_fetch_sub_u64(voidptr, u64) u64

fn C.atomic_thread_fence(int)
fn C.cpu_relax()

fn C.ANNOTATE_RWLOCK_CREATE(voidptr)
fn C.ANNOTATE_RWLOCK_ACQUIRED(voidptr, int)
fn C.ANNOTATE_RWLOCK_RELEASED(voidptr, int)
fn C.ANNOTATE_RWLOCK_DESTROY(voidptr)

$if valgrind ? {
	#flag -I/usr/include/valgrind
	#include <valgrind/helgrind.h>
}
