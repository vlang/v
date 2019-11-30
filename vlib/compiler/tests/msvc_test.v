fn test_flag_parsing() {
	mut rest := '-lGlfw -f gl2,-ltest_nice_meme,-l cc,-Ldl test.o a.o ' //, whatever.o'
	result := ['-l', 'Glfw',
			   '-f', 'gl2',
			   '-l', 'test_nice_meme',
			   '-l', 'cc',
			   '-L', 'dl',
			   '', 'test.o',
			   '', 'a.o']

	mut flags := []string
	for {
		mut base := rest

		fl := if rest.starts_with('-') {
			base = rest[2..].trim_space()
			rest[..2]
		} else {
			''
		}

		// Which ever one of these is lowest we use
		// TODO: we really shouldnt support all of these cmon
		mut lowest := base.index('-') or { -1 }
		a := base.index(' ') or { -1 }
		b := base.index(',') or { -1 }
		for x in [a, b] {
			if (x < lowest && x != -1) || lowest == -1 {
				lowest = x
			}
		}
		arg := if lowest != -1 {
			rest = base[lowest..].trim_space().trim(',')
			base[..lowest].trim_space().trim(',')
		} else {
			rest = ''
			base.trim_space()
		}

		flags << fl
		flags << arg

		if rest.len == 0 {
			break
		}
	}

	for i, f in flags {
		assert f == result[i]
	}
}
