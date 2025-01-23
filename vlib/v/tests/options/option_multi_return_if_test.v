module main

fn test_main() {
	s := 'post = [id:66][title:A link 2 Ze Past][date:1737560206][dir:./push_66]'

	id, title, date, dir := parse_post_values('post', s) or {
		eprintln('Failed to parse !')
		return
	}
	assert 'Results : id=${id} title=${title} date=${date} dir=${dir}' == 'Results : id=66 title=A link 2 Ze Past date=1737560206 dir=./push_66'
}

pub fn parse_post_values(label string, s string) ?(u64, string, i64, string) {
	if s.starts_with(label) {
		id := s.find_between('[id:', ']').u64()
		title := s.find_between('[title:', ']')
		date := s.find_between('[date:', ']').i64()
		dir := s.find_between('[dir:', ']')

		return if title.len == 0 {
			none
		} else {
			println('${@FILE_LINE} ${@FN}: Success -> id=${id} title=${title} date=${date} dir=${dir}')
			return id, title, date, dir
		}
	}
	return none
}
