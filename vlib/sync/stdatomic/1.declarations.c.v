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
			// most Linux distributions have /usr/lib/libatomic.so,
			// but Ubuntu uses gcc version specific dir
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/6
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/7
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/8
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/9
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/10
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/11
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/12
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/13
			#flag -L/usr/lib/gcc/x86_64-linux-gnu/14
			// Redhat/CentOS paths:
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/6
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/7
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/8
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/9
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/10
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/11
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/12
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/13
			#flag -L/usr/lib/gcc/x86_64-redhat-linux/14
			// Gentoo paths:
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/6
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/7
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/8
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/9
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/10
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/11
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/12
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/13
			#flag -L/usr/lib/gcc/x86_64-pc-linux-gnu/14
			// OpenSUSE paths:
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/6
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/7
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/8
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/9
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/10
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/11
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/12
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/13
			#flag -L/usr/lib64/gcc/x86_64-suse-linux/14
			$if musl ? {
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/6
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/7
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/8
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/9
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/10
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/11
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/12
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/13
				#flag -L/usr/lib/gcc/x86_64-pc-linux-musl/14
			}
		} $else $if arm64 {
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/6
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/7
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/8
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/9
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/10
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/11
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/12
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/13
			#flag -L/usr/lib/gcc/aarch64-linux-gnu/14
			// Redhat/CentOS paths:
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/6
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/7
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/8
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/9
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/10
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/11
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/12
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/13
			#flag -L/usr/lib/gcc/aarch64-redhat-linux/14
			// Gentoo paths:
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/6
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/7
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/8
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/9
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/10
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/11
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/12
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/13
			#flag -L/usr/lib/gcc/aarch64-pc-linux-gnu/14
			// OpenSUSE paths:
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/6
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/7
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/8
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/9
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/10
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/11
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/12
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/13
			#flag -L/usr/lib64/gcc/aarch64-suse-linux/14
		}
		#flag -latomic
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
