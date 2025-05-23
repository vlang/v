module sync

fn test_chan_mod_sync() {
	_ := chan bool{cap: 1}
	assert true
}
