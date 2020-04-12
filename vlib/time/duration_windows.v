#include <windows.h>
#flag "lws2_32"

union C.LARGE_INTEGER {
	struct C.DUMMYSTRUCTNAME {
		LowPart u32
		HighPart C.LONG
	}
	struct C.u {
		LowPart u32
		HighPart C.LONG
	}
	QuadPart i64
}

fn C.QueryPerformanceFrequency(lpFrequency *C.LARGE_INTEGER) bool
fn C.QuertPerformanceCounter(lpFrequency *C.LARGE_INTEGER) bool

fn get_millsecond() {

}