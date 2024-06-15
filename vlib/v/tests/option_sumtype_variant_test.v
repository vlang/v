type Foo = ?Bar | ?string | Bar | Baz

struct Baz {}

struct Bar {
	i int
}

fn test_main() {
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

fn test_none() {
	f := Foo(?Bar{})
	match f {
		?Bar {
			assert f == none
		}
		else {
			assert false
		}
	}
}

fn test_unwrap_smarcast() {
	a := Foo(?string('foo'))

	$for v in Foo.variants {
		if a is v {
			$if a is ?string {
				if a != none {
					assert a == 'foo'
				}
			}
		}
	}
}
