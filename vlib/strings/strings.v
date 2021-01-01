module strings

// import rand
// random returns a random string with `n` characters
/*
pub fn random(n int) string {
	buf := vmalloc(n)
	for i in 0..n {
		buf[i] = rand.next()
	}
	return tos(buf)
}
*/
pub fn filter_html(html string) string {
	return html.replace('&', '&amp').replace('<', '&lt').replace('>', '&gt')
}
