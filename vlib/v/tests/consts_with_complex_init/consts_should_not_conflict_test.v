module main

import os

const cfg_dir = os.join_path(os.config_dir() or { panic(err) }, 'foo')

fn test_main() {
	dump('hello')
}
