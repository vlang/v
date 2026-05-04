struct ParserResult[O] {
	i []u8
	o O
}

type Parser[O] = fn ([]u8) !ParserResult[O]

type Mapper[O] = fn ([]u8) O

fn take_part(count int) Parser[[]u8] {
	return fn [count] (i []u8) !ParserResult[[]u8] {
		if i.len < count {
			return error('invalid input length')
		}
		return ParserResult[[]u8]{
			i: i[count..]
			o: i[..count]
		}
	}
}

fn map_parser[O](p Parser[[]u8], f Mapper[O]) Parser[O] {
	return fn [p, f] [O](i []u8) !ParserResult[O] {
		r := p(i)!
		return ParserResult[O]{
			i: r.i
			o: f[O](r.o)
		}
	}
}

fn first_byte_value(input []u8) int {
	return int(input[0])
}

fn test_return_closure_generic_struct_result() {
	parser := map_parser[int](take_part(2), first_byte_value)
	result := parser('ab_rest'.bytes()) or {
		assert false
		return
	}
	assert result.o == int(`a`)
	assert result.i == '_rest'.bytes()
}
