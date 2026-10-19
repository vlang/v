// vtest vflags: -check-overflow
module http

fn test_checked_header_value_keeps_eight_tabs() {
	value := '\t'.repeat(8)
	mut header := new_header()
	header.add_custom('X-Tabs', value)!
	assert header.custom_values('X-Tabs') == [value]
}

fn test_checked_header_value_rejects_eight_nuls() {
	mut header := new_header()
	header.add_custom('X-Nul', '\x00'.repeat(8)) or {
		assert err is HeaderValueError
		return
	}
	assert false, 'NUL must still be rejected in overflow-checked builds'
}
