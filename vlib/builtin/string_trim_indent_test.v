// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

fn test_empty_string() {
	assert ''.trim_indent() == ''
}

fn test_blank_string() {
	assert '   \t'.trim_indent() == ''
}

fn test_multiline_blank_string() {
	assert '
	\t
'.trim_indent() == ''
}

fn test_zero_indentation() {
	assert 'abc
def'.trim_indent() == 'abc\ndef'
}

fn test_zero_indentation_and_blank_first_and_last_lines() {
	assert '
abc
def
'.trim_indent() == 'abc\ndef'
}

fn test_common_case_tabbed() {
	assert '
		abc
		def
	'.trim_indent() == 'abc\ndef'
}

fn test_common_case_spaced() {
	assert '
        abc
        def
	'.trim_indent() == 'abc\ndef'
}

fn test_common_case_tabbed_with_middle_blank_like() {
	assert '
		abc

		def
	'.trim_indent() == 'abc\n\ndef'
}

fn test_common_case_tabbed_with_blank_first_line() {
	assert '    \t
		abc
		def
	'.trim_indent() == 'abc\ndef'
}

fn test_common_case_tabbed_with_blank_first_and_last_line() {
	assert '    \t
		abc
		def
    \t	'.trim_indent() == 'abc\ndef'
}

fn test_html() {
	assert '
		<!doctype html>
		<html lang="en">
		<head>
		</head>
		<body>
			<p>
				Hello, World!
			</p>
		</body>
		</html>
	'.trim_indent() == '<!doctype html>
<html lang="en">
<head>
</head>
<body>
	<p>
		Hello, World!
	</p>
</body>
</html>'
}

fn test_broken_html() {
	assert '
		<!doctype html>
		<html lang="en">
		<head>
		</head>
	<body>
			<p>
				Hello, World!
			</p>
		</body>
		</html>
	'.trim_indent() == '	<!doctype html>
	<html lang="en">
	<head>
	</head>
<body>
		<p>
			Hello, World!
		</p>
	</body>
	</html>'
}

fn test_doc_example() {
	st := '
     Hello there,
     this is a string,
     all the leading indents are removed
     and also the first and the last lines if they are blank
'.trim_indent()
	assert st == 'Hello there,
this is a string,
all the leading indents are removed
and also the first and the last lines if they are blank'
}
