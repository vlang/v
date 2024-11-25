module c

struct PastTmpVar {
mut:
	var_name       string
	tmp_var        string
	s              string
	s_ends_with_ln bool
}

fn (mut g Gen) past_tmp_var_new() PastTmpVar {
	tmp_var := g.new_tmp_var()
	mut s := g.go_before_last_stmt()
	s_ends_with_ln := s.ends_with('\n')
	s = s.trim_space()
	g.empty_line = true
	return PastTmpVar{
		tmp_var:        tmp_var
		s:              s
		s_ends_with_ln: s_ends_with_ln
	}
}

fn (mut g Gen) past_tmp_var_from_var_name(var_name string) PastTmpVar {
	mut tmp_var := g.new_tmp_var()
	mut s := ''
	if var_name != '' {
		tmp_var = var_name
	} else {
		s = g.go_before_last_stmt()
	}
	s_ends_with_ln := s.ends_with('\n')
	s = s.trim_space()
	g.empty_line = true
	return PastTmpVar{
		var_name:       var_name
		tmp_var:        tmp_var
		s:              s
		s_ends_with_ln: s_ends_with_ln
	}
}

fn (mut g Gen) past_tmp_var_done(p &PastTmpVar) {
	if p.var_name.len == 0 {
		if p.s_ends_with_ln {
			g.writeln(p.s)
		} else {
			g.write(p.s)
		}
		if g.inside_return {
			g.write(' ')
		}
		g.write(p.tmp_var)
	}
}
