import ebnf

// Test Code from Go Source.
// https://github.com/golang/exp/blob/master/ebnf/ebnf_test.go

good_pattern := []string{
	'Program = .',
	'Program = foo .
	 foo = "foo" .',
	'Program = "a" | "b" "c" .',
	'Program = "a" … "z" .',

	'Program = Song .
	 Song = { Note } .
	 Note = Do | (Re | Mi | Fa | So | La) | Ti .
	 Do = "c" .
	 Re = "d" .
	 Mi = "e" .
	 Fa = "f" .
	 So = "g" .
	 La = "a" .
	 Ti = ti .
	 ti = "b" .',
}
 
bad_pattern := []string{
	'Program = | .',
	'Program = | b .',
	'Program = a … b .',
	'Program = "a" … .',
	'Program = … "b" .',
	'Program = () .',
	'Program = [] .',
	'Program = {} .',
}

fn check_good(src string){
	grammer
}

fn check_bad(src string){

}

fn test_grammars(){
	for src in good_pattern{
		check_good(src)
	}
	for src in bad_pattern{
		check_bad(src)
	}
}