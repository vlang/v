fn test_match() {
	match 5 {
		0..10 { '0-9' }
		else { 'other' }
	} == '4-5'
}
