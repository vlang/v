import encoding.binary
import rand

type DataType = int | string

struct DataBuilder {
mut:
	func fn (mut []u8)
	len  int
}

pub fn (mut d DataBuilder) add(v DataType) {
	func := d.func
	len := d.len
	d.func = fn [v, len, func] (mut b []u8) {
		if !isnil(func) {
			func(mut b)
		}
		match v {
			int {
				binary.big_endian_put_u32_at(mut b, u32(v), len)
			}
			string {
				binary.big_endian_put_u16_at(mut b, u16(v.len), len)
				for i := 0; i < v.len; i++ {
					b[len + 2 + i] = v[i]
				}
			}
		}
	}
	d.len += match v {
		int { 4 }
		string { 2 + v.len }
	}
}

pub fn (d &DataBuilder) build() []u8 {
	mut b := []u8{len: d.len}
	d.func(mut b)
	binary.big_endian_put_u16(mut b, u16(d.len - 2))
	unsafe {
		d.func = nil
		d.len = 2
	}
	return b
}

fn new_data_builder() DataBuilder {
	return DataBuilder{
		func: unsafe { nil }
		len:  2
	}
}

fn test_closure_with_sumtype_var() {
	mut data := new_data_builder()
	data.add(1)
	data.add(2)
	data.add(3)
	data.add(4)
	data.add(5)
	data.add('1')
	data.add('22')
	data.add('334455')
	data.add(rand.string(rand.int_in_range(10, 100)!))
	data.add('5')
	data.add(1)
	data.add(2)
	data.add(3)
	data.add(4)
	data.add(5)
	ret := data.build()
	dump(ret)
	assert binary.big_endian_u16_at(ret, 0) == ret[2..].len
}
