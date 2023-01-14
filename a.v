module main

struct Aa {
	sub AliasType
}

type AliasType = Bb

struct Bb {
	a int
}

fn encode_struct[U](val U) {

	// $if U is $Alias {
	  $if U.unaliased_type is $Struct {
			dump("good")
	   }
	// }
}

fn main() {
	aa := Aa{}
	encode_struct(aa)
}