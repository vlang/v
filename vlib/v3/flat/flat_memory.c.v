module flat

$if macos || linux {
	#include <sys/mman.h>
	#include <unistd.h>
}

fn C.madvise(addr voidptr, length usize, advice int) int

// discard_unused_capacity releases resident pages beyond the live AST after
// parallel workers have joined and their append regions have been compacted.
// The virtual reservation stays writable, so later appends retain their capacity.
// No surviving view may read or write beyond this AST's live array lengths.
@[unsafe]
pub fn (a &FlatAst) discard_unused_capacity() {
	$if prealloc {
		// Only full pages inside unused array capacity are discarded. Live nodes,
		// array headers, and neighboring arena allocations remain untouched.
		if a.nodes.cap > a.nodes.len {
			unsafe {
				discard_unused_pages(&u8(a.nodes.data) + usize(a.nodes.len) * sizeof(Node), usize(a.nodes.cap - a.nodes.len) * sizeof(Node))
			}
		}
		if a.children.cap > a.children.len {
			unsafe {
				discard_unused_pages(&u8(a.children.data) + usize(a.children.len) * sizeof(NodeId), usize(a.children.cap - a.children.len) * sizeof(NodeId))
			}
		}
	}
}

@[unsafe]
fn discard_unused_pages(start voidptr, size usize) {
	$if macos || linux {
		if size < 1024 * 1024 {
			return
		}
		page_bytes := C.sysconf(C._SC_PAGESIZE)
		if page_bytes <= 0 {
			return
		}
		page_size := usize(page_bytes)
		lo := (usize(start) + page_size - 1) / page_size * page_size
		hi := (usize(start) + size) / page_size * page_size
		if hi <= lo {
			return
		}
		$if macos {
			// Darwin's MADV_DONTNEED retains these dirty pages. Replacing only
			// the dead tail drops its physical backing while preserving addresses.
			p := C.mmap(voidptr(lo), hi - lo, C.PROT_READ | C.PROT_WRITE, C.MAP_PRIVATE | C.MAP_ANONYMOUS | C.MAP_FIXED, -1, 0)
			if p == C.MAP_FAILED {
				panic('could not release unused AST pages')
			}
		} $else {
			C.madvise(voidptr(lo), hi - lo, C.MADV_DONTNEED)
		}
	}
}
