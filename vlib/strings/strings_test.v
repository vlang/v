import strings

fn test_repeat() {
	assert strings.repeat(`x`, 10) == 'xxxxxxxxxx'
	assert strings.repeat(`a`, 1) == 'a'
	assert strings.repeat(`a`, 0) == ''
}

fn test_repeat_string() {
	assert strings.repeat_string('abc', 3) == 'abcabcabc'
	assert strings.repeat_string('abc', 1) == 'abc'
	assert strings.repeat_string('abc', 0) == ''
	assert strings.repeat_string('', 200) == ''
}

const test_rune_and_byte = [
	'xxx[ok1]xxx',
	'xxx[[ok2]okok]',
	'xxx[ok3[[[ok]okok]]]',
	'yyy[ok4]',
	'[]',
	']',
	'[',
	'yyy[ok5][]zzz',
	'yyy[xxx',
	'xxx[xxx
	xxx]',
]

const test_strings = [
	'xxx/*ok1*/xxx',
	'xxx/*/*ok2*/okok*/',
	'xxx/*ok3/*/*/*ok*/okok*/*/*/',
	'yyy/*ok4*/',
	'/**/',
	'*/',
	'/*',
	'yyy/*ok5*//**/zzz',
	'yyy/*xxx',
	'xxx/*xxx
	xxx*/xxx',
]

const expected_rune_and_byte_outputs = [
	'ok1',
	'[ok2]okok',
	'ok3[[[ok]okok]]',
	'ok4',
	'',
	'',
	'',
	'ok5',
	'',
	'xxx
	xxx',
]

const expected_string_outputs = [
	'ok1',
	'/*ok2*/okok',
	'ok3/*/*/*ok*/okok*/*/',
	'ok4',
	'',
	'',
	'',
	'ok5',
	'',
	'xxx
	xxx',
]

fn test_find_between_pair_family() {
	assert strings.find_between_pair_rune('xx♡ok❦yy', `♡`, `❦`) == 'ok'
	assert strings.find_between_pair_u8('xx{ok}yy', `{`, `}`) == 'ok'
	assert strings.find_between_pair_string('xx/*ok*/yy', '/*', '*/') == 'ok'
	assert strings.find_between_pair_u8('xx{ok}yy', `{`, `}`) == 'ok'
	assert strings.find_between_pair_string('xxxxokyyyy', 'xxx', 'yyy') == 'xok'

	for i, tstr in test_rune_and_byte {
		e1 := strings.find_between_pair_rune(tstr, `[`, `]`)
		e2 := expected_rune_and_byte_outputs[i]
		assert '${e1}' == '${e2}'
	}

	for i, tstr in test_rune_and_byte {
		e1 := strings.find_between_pair_u8(tstr, `[`, `]`)
		e2 := expected_rune_and_byte_outputs[i]
		assert '${e1}' == '${e2}'
	}

	for i, tstr in test_strings {
		e1 := strings.find_between_pair_string(tstr, '/*', '*/')
		e2 := expected_string_outputs[i]
		assert '${e1}' == '${e2}'
	}
}

fn test_split_capital() {
	assert strings.split_capital('') == []
	assert strings.split_capital('abc') == ['abc']
	assert strings.split_capital('X') == ['X']
	assert strings.split_capital('XX') == ['X', 'X']
	assert strings.split_capital('XYZ') == ['X', 'Y', 'Z']
	assert strings.split_capital('JohnWilliams') == ['John', 'Williams']
	assert strings.split_capital('JDStar') == ['J', 'D', 'Star']
	assert strings.split_capital('cpDumpRotarySpring') == ['cp', 'Dump', 'Rotary', 'Spring']
}

struct IndentTest {
	param  strings.IndentParam
	input  string
	output string
}

// vfmt off
const indent_test_data = [
	IndentTest{
		input:  'User1 {
        name: "John"
        settings: {
            theme: "dark"
            language: "en"
        }
    }'
		output: 'User1 {
    name: "John"
    settings: {
        theme: "dark"
        language: "en"
    }
}'
	},
	IndentTest{
		input:  'User2{name:"John" settings:{theme:"dark" language:"en" hobbies:["reading","sports"]}}'
		output: 'User2{
    name:"John" settings:{
        theme:"dark" language:"en" hobbies:["reading","sports"]
    }
}'
	},
	IndentTest{
		input:  'message {text: "Hello {world}!" count: 5 nested: {data: "Test {inner}"}}'
		output: 'message {
    text: "Hello {world}!" count: 5 nested: {
        data: "Test {inner}"
    }
}'
	},
	IndentTest{
		input:  'if x > 0 {println("Positive") for i in 0..x {println(i) if i % 2 == 0 {println("even")}}} else {println("Not positive")}'
		output: 'if x > 0 {
    println("Positive") for i in 0..x {
        println(i) if i % 2 == 0 {
            println("even")
        }
    }
} else {
    println("Not positive")
}'
	},
	IndentTest{
		input:  'config{database:{host:"localhost" port:5432} server:{port:8080 routes:{api:"/api" static:"/static"}} log:{level:"info"}}'
		output: 'config{
    database:{
        host:"localhost" port:5432
    } server:{
        port:8080 routes:{
            api:"/api" static:"/static"
        }
    } log:{
        level:"info"
    }
}'
	},
	IndentTest{
		input:  "MyS{
    a: 0
    b: 0
    son: Son{
    k: ''
    m: ''
}
}
"
		output: "MyS{
    a: 0
    b: 0
    son: Son{
        k: ''
        m: ''
    }
}
"
	},
	IndentTest{
		input:  'config {message: "He said: \\"Hello World!\\"" code: "if (x) { return \\"value\\"; }"}'
		output: 'config {
    message: "He said: \\"Hello World!\\"" code: "if (x) { return \\"value\\"; }"
}'
	},
	IndentTest {
		input : 'data {text: "String with {curly braces} and \\"quotes\\"" nested: {value: "Inner \\"quoted\\" string"}}'
		output : 'data {
    text: "String with {curly braces} and \\"quotes\\"" nested: {
        value: "Inner \\"quoted\\" string"
    }
}'
	},
	IndentTest {
		input : 'escape {backslash: "Path: C:\\\\Users\\\\Test" newline: "Line1\\nLine2" tab: "Column1\\tColumn2"}'
		output : 'escape {
    backslash: "Path: C:\\\\Users\\\\Test" newline: "Line1\\nLine2" tab: "Column1\\tColumn2"
}'
	},
	IndentTest {
		input : 'nested {outer: "Outer \\"quote\\" with {braces}" inner: {value: "Inner string with \\\\backslash and \\"quotes\\""}}'
		output : 'nested {
    outer: "Outer \\"quote\\" with {braces}" inner: {
        value: "Inner string with \\\\backslash and \\"quotes\\""
    }
}'
	},
	IndentTest {
		input : 'complex {str1: "\\"Escaped quotes\\"" str2: "Backslash: \\\\" str3: "Mixed: \\"quoted\\" and \\\\backslash" regex: "Pattern: \\\\d+\\\\.\\\\d+"}'
		output : 'complex {
    str1: "\\"Escaped quotes\\"" str2: "Backslash: \\\\" str3: "Mixed: \\"quoted\\" and \\\\backslash" regex: "Pattern: \\\\d+\\\\.\\\\d+"
}'
	},
	IndentTest {
		input : 'json {data: "{\\"name\\": \\"John\\", \\"age\\": 30, \\"city\\": \\"New York\\"}"}'
		output : 'json {
    data: "{\\"name\\": \\"John\\", \\"age\\": 30, \\"city\\": \\"New York\\"}"
}'
	},
	IndentTest {
		input : 'code {function: "fn test() { if (x) { return \\\"value\\\"; } }" error: "Error: \\\"File not found\\\" at line 10"}'
		output : 'code {
    function: "fn test() { if (x) { return \\\"value\\\"; } }" error: "Error: \\\"File not found\\\" at line 10"
}'
	},
	IndentTest {
		input : 'multiline {message: "Line 1\\nLine 2\\nLine 3 with {braces}\\nLine 4 with \\\"quotes\\\""}'
		output : 'multiline {
    message: "Line 1\\nLine 2\\nLine 3 with {braces}\\nLine 4 with \\\"quotes\\\""
}'
	},
	IndentTest {
		input : 'extreme {str: "\\\\\\\\\\\\\\"Quadruple escaped\\\\\\\\\\\\\\""}'
		output : 'extreme {
    str: "\\\\\\\\\\\\\\"Quadruple escaped\\\\\\\\\\\\\\""
}'
	},
	IndentTest{
		param: IndentParam {
			block_start: `[`
			block_end: `]`
		}
		input:  'message [text: "Hello {world}!" count: 5 nested: [data: "Test {inner}"]]'
		output: 'message [
    text: "Hello {world}!" count: 5 nested: [
        data: "Test {inner}"
    ]
]'
	},
	IndentTest{
		param: IndentParam {
			block_start: `[`
			block_end: `]`
			indent_char: `#`
		}
		input:  'message [text: "Hello {world}!" count: 5 nested: [data: "Test {inner}"]]'
		output: 'message [
####text: "Hello {world}!" count: 5 nested: [
########data: "Test {inner}"
####]
]'
	},
	IndentTest{
		param: IndentParam {
			block_start: `[`
			block_end: `]`
			indent_char: `\t`
			indent_count: 2
		}
		input:  'message [text: "Hello {world}!" count: 5 nested: [data: "Test {inner}"]]'
		output: 'message [
\t\ttext: "Hello {world}!" count: 5 nested: [
\t\t\t\tdata: "Test {inner}"
\t\t]
]'
	},
	IndentTest{
		param: IndentParam {
			block_start: `[`
			block_end: `]`
			indent_char: `#`
			indent_count: 1
			starting_level: 2
		}
		input:  'message [text: "Hello {world}!" count: 5 nested: [data: "Test {inner}"]]'
		output: '##message [
###text: "Hello {world}!" count: 5 nested: [
####data: "Test {inner}"
###]
##]'
	},
]

// vfmt on

fn test_indent() {
	for t in indent_test_data {
		mut sb := strings.new_builder(256)
		assert sb.indent(t.input, t.param).str() == t.output
	}
}
