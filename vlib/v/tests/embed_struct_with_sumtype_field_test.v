type Prefix = [2]string | string

struct FooStruct {
	prefix Prefix
}

struct BarStruct {
	FooStruct
}

fn (b BarStruct) print_prefix() string {
	if b.prefix is [2]string {
		eprint(b.prefix[0])
		return b.prefix[0]
	} else if b.prefix is string {
		eprint(b.prefix)
		return b.prefix
	}
	return ''
}

fn test_embed_struct_with_sumtype_field() {
	b := BarStruct{
		prefix: ['abc', 'bcd']!
	}
	ret := b.print_prefix()
	assert ret == 'abc'
}
