module os

fn test_split_args_groups_literal_arguments() {
	assert split_args('tool "two words" \'single quoted\' empty="" trailing\\ space')! ==
		['tool', 'two words', 'single quoted', 'empty=', 'trailing space']
	assert split_args('tool "" \'\'')! == ['tool', '', '']
	assert split_args('tool ; && | > $(command) %PATH%')! ==
		['tool', ';', '&&', '|', '>', '$(command)', '%PATH%']
	assert split_args('tool "C:\\work space\\file"')! == ['tool', 'C:\\work space\\file']
	assert split_args('  \t\r\n')! == []string{}
}

fn test_split_args_rejects_unterminated_quotes() {
	for text in ['tool "unfinished', "tool 'unfinished"] {
		split_args(text) or {
			assert err.msg() == 'unterminated quote in argument list'
			continue
		}
		assert false, text
	}
}
