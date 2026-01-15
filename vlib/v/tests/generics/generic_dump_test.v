struct Slice {
	pointer &u8 = unsafe { nil }
	len     int
}

pub fn decode_slice[T]() !T {
	return T{}
}

@[manualfree]
pub fn decode[T]() ! {
	dump(decode_slice[T]()!)
}

fn test_main() {
	decode[Slice]()!
	decode[[3]int]()!
}
