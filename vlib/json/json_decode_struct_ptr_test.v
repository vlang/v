import json

struct Message {
mut:
	id       int
	text     string
	reply_to &Message
}

fn test_main() {
	mut json_data := '{"id": 1, "text": "Hello", "reply_to": {"id": 2, "text": "Hi"}}'
	mut message := json.decode(Message, json_data)!
	assert message.reply_to.id == 2

	json_data = '{"id": 1, "text": "Hello", "reply_to": {"id": 2, "text": "Hi", "reply_to": {}}}'
	message = json.decode(Message, json_data)!
	assert message.reply_to.reply_to.reply_to == unsafe { nil }

	json_data = '{"id": 1, "text": "Hello", "reply_to": {"id": 2, "text": "Hi", "reply_to": {"id": 5}}}'
	message = json.decode(Message, json_data)!
	assert message.reply_to.reply_to.id == 5
}
