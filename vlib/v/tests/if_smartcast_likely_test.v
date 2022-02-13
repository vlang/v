// fixes https://github.com/vlang/v/issues/11485 based on code example by https://github.com/Wertzui123
interface IExample {
}

struct Example {
	value string
}

fn test_if_smartcast_likely() {
	print_value(Example{ value: 'Hello' })
}

fn print_value(example IExample) {
	if _likely_(example is Example) {
		print('Value: ' + example.value)
	}
}
