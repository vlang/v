import v.pref

fn test_comptime_at() {
	assert @VEXE == pref.vexe_path()
}
