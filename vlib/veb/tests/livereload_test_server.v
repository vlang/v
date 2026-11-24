module main

import os
import veb.tests.livereload_fixture

fn main() {
	livereload_fixture.run(os.args[1].int(), os.args[2])!
}
