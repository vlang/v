module main

const c_u16_size = sizeof(u16)
const c_u32_size = sizeof(u32)

pub enum DataKind as u8 {
	u8_array
}

union U16Bytes {
	value u16
	bytes [c_u16_size]u8
}

union U32Bytes {
	value u32
	bytes [c_u32_size]u8
}

fn test_main() {
	kind := DataKind.u8_array
	match kind {
		.u8_array {
			buf_4u8 := [c_u32_size]u8{}
			w := U32Bytes{
				bytes: buf_4u8
			}
			unsafe {
				assert w.bytes.len == c_u32_size
			}
		}
	}
}
