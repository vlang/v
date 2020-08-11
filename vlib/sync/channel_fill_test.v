import sync

const (
	queue_len = 1000
	queue_fill = 763
)

fn do_send(mut ch sync.Channel, fin sync.Semaphore) {
	for i in 0 .. queue_fill {
		ch.push(&i)
	}
	fin.post()
}

fn test_channel_len_cap() {
	mut ch := sync.new_channel<int>(queue_len)
	sem := sync.new_semaphore()
	go do_send(mut ch, sem)
	sem.wait()
	assert ch.cap == queue_len
	assert ch.len() == queue_fill
}
