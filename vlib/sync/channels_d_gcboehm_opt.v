// constructor for channels for non-reference types (that need no scan by GC)

fn new_channel_st_noscan(n u32, st u32) &Channel {
	wsem := if n > 0 { n } else { 1 }
	rsem := if n > 0 { u32(0) } else { 1 }
	rbuf := if n > 0 { unsafe { malloc_noscan(int(n * st)) } } else { &byte(0) }
	sbuf := if n > 0 { vcalloc_noscan(int(n * 2)) } else { &byte(0) }
	mut ch := &Channel{
		objsize: st
		cap: n
		write_free: n
		read_avail: 0
		ringbuf: rbuf
		statusbuf: sbuf
		write_subscriber: 0
		read_subscriber: 0
	}
	ch.writesem.init(wsem)
	ch.readsem.init(rsem)
	ch.writesem_im.init(0)
	ch.readsem_im.init(0)
	return ch
}
