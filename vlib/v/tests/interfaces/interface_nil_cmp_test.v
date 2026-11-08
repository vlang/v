interface Cfg {
	id string
}

struct XX {
	cfg &Cfg = unsafe { nil }
}

@[unsafe]
fn test_main() {
	xx := XX{}
	assert xx.cfg == unsafe { nil }
	assert unsafe { nil } == xx.cfg
}
