struct Aa {
	a int
}

struct Bb {
	b int
}

type Type = Aa | Bb

fn test_main() {
	t := Type(Aa{
		a: 2
	})
	match t {
		Aa {
			assert t.a == 2
			func := fn [t] () {
				assert typeof(t).name == 'Aa'
				assert t.a == 2
			}
			func()
		}
		Bb {}
	}
}
