type TxPayload = Can | u8

enum Tx {
	empty
	data
	can
}

fn (tx Tx) frame_bytes(payload ?TxPayload) ![]u8 {
	match tx {
		.empty {
			return [u8(tx)]
		}
		.data {
			if payload != none {
				if payload is u8 {
					return [u8(tx), payload]
				}
			}
			return error('Invalid data')
		}
		.can {
			if payload != none {
				if payload is Can {
					return [u8(tx), payload.net]
				}
			}
			return error('Invalid can')
		}
	}
}

struct Can {
	net u8
}

fn test_main() {
	assert Tx.empty.frame_bytes(none)! == [u8(0)]
	assert Tx.data.frame_bytes(u8(123))! == [u8(1), 123]
}
