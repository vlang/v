// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

fn test_strip_margins_no_tabs() {
	no_tabs := ['Hello there',
	            'This is a string',
	            'With multiple lines',
	           ].join('\n')
	no_tabs_stripped := 'Hello there
	                    |This is a string
						|With multiple lines'.strip_margin()
	assert no_tabs == no_tabs_stripped
}

fn test_strip_margins_text_before() {
	text_before := ['There is text',
	                'before the delimiter',
	                'that should be removed as well',
	               ].join('\n')
	text_before_stripped := 'There is text
	f lasj  asldfj j lksjdf |before the delimiter
	Which is removed hello  |that should be removed as well'.strip_margin()
	assert text_before_stripped == text_before
}

fn test_strip_margins_white_space_after_delim() {
	tabs := ['	Tab',
	         '    spaces',
	         '	another tab',
	        ].join('\n')
	tabs_stripped := '	Tab
	                 |    spaces
					 |	another tab'.strip_margin()
	assert tabs == tabs_stripped
}

fn test_strip_margins_alternate_delim() {
	alternate_delimiter := ['This has a different delim,',
	                        'but that is ok',
	                        'because everything works',
	                       ].join('\n')
	alternate_delimiter_stripped := 'This has a different delim,
	                                #but that is ok
                                    #because everything works'.strip_margin_custom(`#`)
	assert alternate_delimiter_stripped == alternate_delimiter
}

fn test_strip_margins_multiple_delims_after_first() {
	delim_after_first_instance := ['The delimiter used',
	                               'only matters the |||| First time it is seen',
	                               'not any | other | times',
	                              ].join('\n')
	delim_after_first_instance_stripped := 'The delimiter used
	                                       |only matters the |||| First time it is seen
	                                       |not any | other | times'.strip_margin()
	assert delim_after_first_instance_stripped == delim_after_first_instance
}

fn test_strip_margins_uneven_delims() {
	uneven_delims := ['It doesn\'t matter if the delims are uneven,',
	                  'The text will still be delimited correctly.',
	                  'Maybe not everything needs 3 lines?',
	                  'Let us go for 4 then',
	                 ].join('\n')
	uneven_delims_stripped := 'It doesn\'t matter if the delims are uneven,
           |The text will still be delimited correctly.
                      |Maybe not everything needs 3 lines?
				|Let us go for 4 then'.strip_margin()
	assert uneven_delims_stripped == uneven_delims
}

fn test_strip_margins_multiple_blank_lines() {
	multi_blank_lines := ['Multiple blank lines will be removed.',
	                      '	I actually consider this a feature.',
	                     ].join('\n')
	multi_blank_lines_stripped := 'Multiple blank lines will be removed.



		|	I actually consider this a feature.'.strip_margin()
	assert multi_blank_lines == multi_blank_lines_stripped
}

fn test_strip_margins_end_newline() {
	end_with_newline := ['This line will end with a newline',
	                     'Something cool or something.',
	                     '',
	                    ].join('\n')
	end_with_newline_stripped := 'This line will end with a newline
	                             |Something cool or something.

					'.strip_margin()
	assert end_with_newline_stripped == end_with_newline
}

fn test_strip_margins_space_delimiter() {
	space_delimiter := ['Using a white-space char will',
	                    'revert back to default behavior.',
	                   ].join('\n')
	space_delimiter_stripped := 'Using a white-space char will
		|revert back to default behavior.'.strip_margin_custom(`\n`)
	assert space_delimiter == space_delimiter_stripped
}

fn test_strip_margins_crlf() {
	crlf := ['This string\'s line endings have CR as well as LFs.',
	         'This should pass',
	         'Definitely',
	        ].join('\r\n')
	crlf_stripped := 'This string\'s line endings have CR as well as LFs.\r
	                 |This should pass\r
					 |Definitely'.strip_margin()

	assert crlf == crlf_stripped
}
