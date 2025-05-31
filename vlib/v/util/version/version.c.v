module version

// vhash() returns the build string C.V_COMMIT_HASH . See cmd/tools/gen_vc.v .
pub fn vhash() string {
	mut buf := [50]u8{}
	buf[0] = 0
	unsafe {
		bp := &buf[0]
		C.snprintf(&char(bp), 50, c'%s', C.V_COMMIT_HASH)
		return tos_clone(bp)
	}
}
