pub struct ParserResult[O] {
	i []u8
	o O
}

type TParser[O] = fn ([]u8) !ParserResult[O]

type TF[O] = fn ([]u8) O

fn take_part(count u32) TParser[[]u8] {
	return fn [count] (i []u8) !ParserResult[[]u8] {
		if i.len < count {
			return error('error len: ${i}')
		}
		return ParserResult[[]u8]{
			i: i[count..]
			o: i[..count]
		}
	}
}

fn map[O](p TParser[u8], f TF[O]) TParser[O] {
	return fn [p, f] [O](i []u8) !ParserResult[O] {
		r := p(i)!
		return ParserResult{
			i: r.i
			o: f(r.o)
		}
	}
}

fn test_main() {
	input := '10_ttt_uuuu_qqq_'.bytes()

	nnn := take_part(2)
	res := nnn(input)!
	assert res.i == [u8(95), 116, 116, 116, 95, 117, 117, 117, 117, 95, 113, 113, 113, 95]
	assert res.o == [u8(49), 48]
}
