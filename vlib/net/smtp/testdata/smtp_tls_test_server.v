module main

import net.smtp.testdata.tls_server
import os

fn main() {
	tls_server.run(os.args[1..])
}
