module main

@[footer: 'Hello
World']
pub struct Config {}

fn test_main() {
	$for a in Config.attributes {
		assert a.arg == 'Hello\nWorld'
	}
}
