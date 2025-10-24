struct Struct {}

fn main() {
	s := ?Struct(Struct{})
	a := s or { panic('none') }
	a == Struct{}
}
