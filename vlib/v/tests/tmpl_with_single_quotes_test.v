module main

pub struct SomeThing {
pub:
	m map[string]string
}

fn (s SomeThing) someval(what string) string {
	return s.m[what]
}

fn (s SomeThing) template() string {
	return $tmpl('tmpl/template.in')
}

fn test_tmpl_with_single_quotes() {
	mut sm := map[string]string{}
	sm['huh'] = 'monkey'
	sm['hah'] = 'parrot'

	s := SomeThing{sm}
	result := s.template()
	println(result)
	assert result.trim_space() == 'someval: monkey

someotherval: parrot'
}
