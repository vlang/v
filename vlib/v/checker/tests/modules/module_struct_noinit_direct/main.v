import locked
import ordinary

type Alias = locked.Foo

fn main() {
	_ = locked.new_foo()
	_ = ordinary.Foo{ digit: 5 }
	_ = locked.Foo{}
	_ = locked.Foo{ digit: 5 }
	_ = &locked.Foo{ digit: 5 }
	_ = Alias{ digit: 5 }
	_ = locked.Generic[int]{ digit: 5 }
}
