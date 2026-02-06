fn check(s string) {
	srunes := s.runes()
	println('')
	println('>          s: ${s}')
	println('>      s.len: ${s.len:-4}')
	println('> srunes.len: ${srunes.len:-4}')
	mut itera_ := []rune{}
	for r in s.runes_iterator() {
		itera_ << r
	}
	println('>   srunes: ${srunes}')
	println('> iterated: ${itera_}')
	assert srunes == itera_
}

fn test_ascii() {
	check('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789')
}

fn test_mixed() {
	check('abc,あいうえお,привет,❄☕❀💰')
}

fn test_emoji_and_for_i_r_in_iterator() {
	s := '❄☕❀💰'
	check(s)
	srunes := s.runes()
	for i, r in s.runes_iterator() {
		eprintln('> i: ${i} | r: ${r}')
		assert srunes[i] == r
	}
}
