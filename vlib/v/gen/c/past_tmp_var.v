module c

struct PastTmpVar {
mut:
	tmp_var        string
	s              string
	s_ends_with_ln bool
}

fn (mut g Gen) past_tmp_var_new() PastTmpVar {
	tmp_var := g.new_tmp_var()
	mut s := g.go_before_stmt(0)
	s_ends_with_ln := s.ends_with('\n')
	s = s.trim_space()
	g.empty_line = true
	return PastTmpVar{
		tmp_var: tmp_var
		s: s
		s_ends_with_ln: s_ends_with_ln
	}
}

fn (mut g Gen) past_tmp_var_done(p &PastTmpVar) {
	if p.s_ends_with_ln {
		g.writeln(p.s)
	} else {
		g.write(p.s)
	}
	g.write(p.tmp_var)
}
