import sync

fn test_sync_channel_push_string_literal() {
	mut ch := sync.new_channel[string](1)
	ch.push('Hello')
	mut got := ''
	assert ch.pop(&got)
	assert got == 'Hello'
}
