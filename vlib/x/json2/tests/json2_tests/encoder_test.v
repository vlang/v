import x.json2 as json

fn test_utf8_strings_are_not_modified() {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	deresult := json.decode[json.Any](original)!
	assert deresult.str() == original

	assert json.encode('ü') == '"ü"'
	assert json.encode('Schilddrüsenerkrankungen') == '"Schilddrüsenerkrankungen"'
}
