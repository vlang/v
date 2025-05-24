module sync

struct ChanStruct {
	workers []chan bool
}

fn test_chan_mod_sync() {
	_ := chan bool{cap: 1}
	assert true
}
