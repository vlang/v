module producer

import consumer

fn five() int {
	return 5
}

fn seven() int {
	return 7
}

pub const f = consumer.once(seven)
pub const direct = consumer.call(five)
pub const local = consumer.call_local(five)
