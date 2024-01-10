module sync

pub struct Channel {
	arr array
}

pub fn new_channel[T](n u32) &Channel {
	return &Channel{arr, new_array()}
}

pub fn (mut ch Channel) close() {}

pub fn (mut ch Channel) push(src voidptr) {
	#array_push(ch.val.arr,src)
}

pub fn (ch Channel) len() int {
	return ch.arr.len
}

pub fn (ch Channel) closed() bool {
	return false
}

pub fn (mut ch Channel) pop(dest voidptr) {
	#dest.val = array_pop(ch.val.arr)
}
