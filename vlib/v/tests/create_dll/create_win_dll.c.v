module test

fn C._vinit(int, voidptr)
fn C.GC_INIT()

const (
	foo = 1
	bar = (foo << 5) + 9
)

[export: Tatltuae]
pub fn test_tatltuae() int {
	return test.foo + test.bar
}

[callconv: stdcall]
[export: DllMain]
fn main(hinst voidptr, fdw_reason int, lp_reserved voidptr) bool {
	match fdw_reason {
		C.DLL_PROCESS_ATTACH {
			$if static_boehm ? {
				C.GC_INIT()
			}
			C._vinit(0, 0)
		}
		C.DLL_THREAD_ATTACH {}
		C.DLL_THREAD_DETACH {}
		C.DLL_PROCESS_DETACH {}
		else {
			return false
		}
	}
	return true
}
