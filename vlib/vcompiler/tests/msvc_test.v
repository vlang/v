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
			base = rest.right(2).trim_space()
			rest.left(2)
		} else {
			''
		}

		// Which ever one of these is lowest we use
		// TODO: we really shouldnt support all of these cmon
		mut lowest := base.index('-')
		for x in [base.index(' '), base.index(',')] {
			if (x < lowest && x != -1) || lowest == -1 {
				lowest = x
			}
		}
		arg := if lowest != -1 {
			rest = base.right(lowest).trim_space().trim(',')
			base.left(lowest).trim_space().trim(',')
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
