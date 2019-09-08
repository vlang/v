import ebnf

test_patten :=
'	`Program = .`,

	`Program = foo .
	 foo = "foo" .`,

	`Program = "a" | "b" "c" .`,

	`Program = "a" … "z" .`,

	`Program = Song .
	 Song = { Note } .
	 Note = Do | (Re | Mi | Fa | So | La) | Ti .
	 Do = "c" .
	 Re = "d" .
	 Mi = "e" .
	 Fa = "f" .
	 So = "g" .
	 La = "a" .
	 Ti = ti .
	 ti = "b" .`,
}'

fn good_program(){

}

fn bad_program(){
    
}