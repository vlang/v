const template_path = 'tmpl/template.in'

pub struct SomeThing {
pub:
	m map[string]string
}

fn (s SomeThing) someval(what string) string {
	return s.m[what]
}

fn (s SomeThing) template_variable() string {
	path := 'tmpl/template.in'
	return $tmpl(path)
}

fn (s SomeThing) template_const() string {
	return $tmpl(template_path)
}

fn test_tmpl_with_variable_path() {
	mut sm := map[string]string{}
	sm['huh'] = 'monkey'
	sm['hah'] = 'parrot'

	s := SomeThing{sm}
	result := s.template_variable()
	println(result)
	assert result.trim_space() == 'someval: monkey

someotherval: parrot'
}

fn test_tmpl_with_const_path() {
	mut sm := map[string]string{}
	sm['huh'] = 'monkey'
	sm['hah'] = 'parrot'

	s := SomeThing{sm}
	result := s.template_const()
	println(result)
	assert result.trim_space() == 'someval: monkey

someotherval: parrot'
}
