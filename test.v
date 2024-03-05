struct Undefined {}

@[on_generic_duplicate: 'note: Sentinels are internal']
pub type UndefinedOr[T] = Undefined | T

fn main() {
	x := UndefinedOr[Undefined](Undefined{})
	dump(x)
}