module main

import encoding.xml

fn test_escape() {
	assert xml.escape_text(content: 'Normal string') == 'Normal string'
	assert xml.escape_text(content: '12 < 34') == '12 &lt; 34'
	assert xml.escape_text(content: '12 > 34') == '12 &gt; 34'
	assert xml.escape_text(content: '12 & 34') == '12 &amp; 34'
	assert xml.escape_text(content: 'He said, "Very well, let us proceed."') == 'He said, &quot;Very well, let us proceed.&quot;'
	assert xml.escape_text(content: "He said, 'Very well, let us proceed.'") == 'He said, &apos;Very well, let us proceed.&apos;'

	assert xml.escape_text(content: 'Do not escape ©.') == 'Do not escape ©.'

	mut reverse_entities := xml.default_entities_reverse.clone()
	reverse_entities['©'] = 'copy'
	assert xml.escape_text(content: 'Do escape ©.', reverse_entities: reverse_entities) == 'Do escape &copy;.'
}

fn test_unescape() ! {
	assert xml.unescape_text(content: 'Normal string')! == 'Normal string'
	assert xml.unescape_text(content: '12 &lt; 34')! == '12 < 34'
	assert xml.unescape_text(content: '12 &gt; 34')! == '12 > 34'
	assert xml.unescape_text(content: '12 &amp; 34')! == '12 & 34'
	assert xml.unescape_text(content: 'He said, &quot;Very well, let us proceed.&quot;')! == 'He said, "Very well, let us proceed."'
	assert xml.unescape_text(content: 'He said, &apos;Very well, let us proceed.&apos;')! == "He said, 'Very well, let us proceed.'"

	xml.unescape_text(content: '12 &invalid; 34') or {
		assert err.msg() == 'Unknown entity: invalid'
	}

	xml.unescape_text(content: 'Do not unescape &copy;') or {
		assert err.msg() == 'Unknown entity: copy'
	}

	mut entities := xml.default_entities.clone()
	entities['copy'] = '©'
	assert xml.unescape_text(content: 'Do unescape &copy;.', entities: entities)! == 'Do unescape ©.'
}
