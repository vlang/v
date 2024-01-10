module main

import compress.zlib
import os

enum CompressionType {
	zlib
}

fn main() {
	if os.args.len != 5 {
		eprintln('v compress <type> <in> <out>')
		eprintln('supported types: zlib')
		exit(1)
	}
	compression_type := match os.args[2] {
		'zlib' {
			CompressionType.zlib
		}
		else {
			eprintln('unsupported type: ${os.args[1]}')
			exit(1)
		}
	}
	path := os.args[3]
	content := os.read_bytes(path) or {
		eprintln('unable to read "${path}": ${err}')
		exit(1)
	}
	compressed := match compression_type {
		.zlib {
			zlib.compress(content) or {
				eprintln('compression error: ${err}')
				exit(1)
			}
		}
	}
	out_path := os.args[4]

	os.write_file_array(out_path, compressed) or {
		eprintln('failed to write "${out_path}": ${err}')
		exit(1)
	}
}
