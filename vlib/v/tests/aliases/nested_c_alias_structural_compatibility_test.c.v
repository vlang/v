// vtest build: windows
type C.WCHAR = u16
type C.PWSTR = &C.WCHAR
type C.FILE_SHARE_MODE = u32

struct WideAliasHolder {
mut:
	ptr C.PWSTR
}

struct ShareModeHolder {
mut:
	mode C.FILE_SHARE_MODE
}

fn accept_wide_ptr(ptr C.PWSTR) string {
	return unsafe { string_from_wide(&u16(ptr)) }
}

fn accept_share_mode(mode C.FILE_SHARE_MODE) u32 {
	return u32(mode)
}

fn test_nested_c_alias_types_are_structurally_compatible() {
	wide := 'example.txt'.to_wide()
	assert accept_wide_ptr(wide) == 'example.txt'

	mut wide_holder := WideAliasHolder{
		ptr: C.PWSTR(unsafe { nil })
	}
	wide_holder.ptr = wide
	assert unsafe { string_from_wide(&u16(wide_holder.ptr)) } == 'example.txt'

	mode := u32(0x40000000)
	assert accept_share_mode(mode) == mode

	mut mode_holder := ShareModeHolder{}
	mode_holder.mode = mode
	assert u32(mode_holder.mode) == mode

	mode_holder = ShareModeHolder{
		mode: mode
	}
	assert u32(mode_holder.mode) == mode
}
