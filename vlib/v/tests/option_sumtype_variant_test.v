type Foo = ?Bar | Bar | Baz

struct Baz {}

struct Bar {
	i int
}

fn main() {
	f := Foo(?Bar{
		i: 0
	})
	match f {
		Bar {
			assert false
		}
		?Bar {
			if i := f {
				assert i == Bar{
					i: 0
				}
				dump(i)
				println(i)
			} else {
				assert false
			}
		}
		else {}
	}
	assert f == Foo(?Bar{
		i: 0
	})
	dump(f)
}
