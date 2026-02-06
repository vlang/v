module main

import encoding.xml

fn test_escape() {
	assert xml.escape_text('Normal string') == 'Normal string'
	assert xml.escape_text('12 < 34') == '12 &lt; 34'
	assert xml.escape_text('12 > 34') == '12 &gt; 34'
	assert xml.escape_text('12 & 34') == '12 &amp; 34'
	assert xml.escape_text('He said, "Very well, let us proceed."') == 'He said, &quot;Very well, let us proceed.&quot;'
	assert xml.escape_text("He said, 'Very well, let us proceed.'") == 'He said, &apos;Very well, let us proceed.&apos;'

	assert xml.escape_text('Do not escape ©.') == 'Do not escape ©.'

	mut reverse_entities := xml.default_entities_reverse.clone()
	reverse_entities['©'] = 'copy'
	assert xml.escape_text('Do escape ©.', reverse_entities: reverse_entities) == 'Do escape &copy;.'
}

fn test_unescape() ! {
	assert xml.unescape_text('Normal string')! == 'Normal string'
	assert xml.unescape_text('12 &lt; 34')! == '12 < 34'
	assert xml.unescape_text('12 &gt; 34')! == '12 > 34'
	assert xml.unescape_text('12 &amp; 34')! == '12 & 34'
	assert xml.unescape_text('He said, &quot;Very well, let us proceed.&quot;')! == 'He said, "Very well, let us proceed."'
	assert xml.unescape_text('He said, &apos;Very well, let us proceed.&apos;')! == "He said, 'Very well, let us proceed.'"

	xml.unescape_text('12 &invalid; 34') or { assert err.msg() == 'Unknown entity: invalid' }

	xml.unescape_text('Do not unescape &copy;') or { assert err.msg() == 'Unknown entity: copy' }

	mut entities := xml.default_entities.clone()
	entities['copy'] = '©'
	assert xml.unescape_text('Do unescape &copy;.', entities: entities)! == 'Do unescape ©.'
}
