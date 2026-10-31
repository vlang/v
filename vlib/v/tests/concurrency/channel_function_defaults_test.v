@[has_globals]
module main

type ChannelTask = fn (int) int

struct TaskChannels {
	buffered   chan ChannelTask = chan ChannelTask{cap: 2}
	unbuffered chan ChannelTask
}

__global default_task_channel chan ChannelTask

fn channel_task(value int) int {
	return value + 10
}

fn test_function_channel_explicit_and_field_defaults() {
	channels := TaskChannels{}
	channels.buffered <- channel_task
	task := <-channels.buffered
	assert task(2) == 12
	assert channels.unbuffered.cap == 0
	channels.unbuffered.close()

	explicit := chan ChannelTask{cap: 1}
	explicit <- channel_task
	received := <-explicit
	assert received(3) == 13
}

fn test_function_channel_container_and_global_defaults() {
	channels := []chan ChannelTask{len: 2, init: chan ChannelTask{cap: 1}}
	for channel in channels {
		channel <- channel_task
		task := <-channel
		assert task(4) == 14
		channel.close()
	}
	assert default_task_channel.cap == 0
	default_task_channel.close()
}
