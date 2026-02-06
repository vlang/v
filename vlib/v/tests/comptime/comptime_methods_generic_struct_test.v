interface El {
	tag() string
	content() ![]u8
}

struct Aa {
	a string
}

fn (a Aa) tag() string {
	return 'a'
}

fn (a Aa) content() ![]u8 {
	return a.a.bytes()
}

struct Bb {
	b string
}

fn (b Bb) tag() string {
	return 'b'
}

fn (b Bb) content() ![]u8 {
	return b.b.bytes()
}

struct Choi[T] implements El {
	src []T
}

fn (c Choi[T]) tag() string {
	return 'seq'
}

fn (c Choi[T]) content() ![]u8 {
	mut out := []u8{}
	for el in c.src {
		out << el.tag().bytes()
		out << el.content()!
	}
	return out
}

fn is_gelement[T]() int {
	mut val := 0
	$for meth in T.methods {
		$if meth.name == 'tag' {
			val += 1
		}
		$if meth.name == 'content' {
			val += 1
		}
	}
	return val
}

fn is_element[T]() bool {
	$if T is El {
		return true
	}
	return false
}

struct Per {
	a Aa
	b Bb
	c Choi[Aa]
	d string
}

fn test_main() {
	assert dump(is_element[Aa]()) == true
	assert dump(is_element[Bb]()) == true
	assert dump(is_gelement[Bb]()) == 2
	assert dump(is_element[Choi[Aa]]()) == true
	assert dump(is_gelement[Choi[Bb]]()) == 2
}
