import x.json2.decoder2 as json
import x.json2

fn test_utf8_strings_are_not_modified() {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	deresult := json.decode[json2.Any](original)!
	assert deresult.str() == original

	assert json2.encode('ü') == '"ü"'
	assert json2.encode('Schilddrüsenerkrankungen') == '"Schilddrüsenerkrankungen"'
}
