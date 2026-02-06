interface Ennemi {
}

struct Map {
	ennemis []Ennemi
}

fn test_interface_arr_for_mut_iter_index() {
	mut maap := Map{}

	for mut ennemi in maap.ennemis {
		assert maap.ennemis.index(ennemi) == 0
	}
}
