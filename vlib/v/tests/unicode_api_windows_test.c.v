#include <tchar.h>
#define v_test_winapi_tchar_size() ((int)sizeof(TCHAR))
#define v_test_crt_tchar_size() ((int)sizeof(_TCHAR))

fn C.v_test_winapi_tchar_size() int
fn C.v_test_crt_tchar_size() int
fn C.lstrlen(&u16) int

fn test_windows_generic_character_types_are_wide() {
	assert C.v_test_winapi_tchar_size() == 2
	assert C.v_test_crt_tchar_size() == 2
	assert C.lstrlen('V ΩЖ'.to_wide()) == 4
}
