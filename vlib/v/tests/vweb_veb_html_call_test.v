fn test_vweb_veb_html_call() {
	// vfmt off
	$veb.html('./tmpl/index.html')
	$vweb.html('./tmpl/index.html')
	// vfmt on
	assert true
}
