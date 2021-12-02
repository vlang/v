module main

import compress.zlib
import os


enum CompressionMethod {
	zlib
}

fn main() {
	if os.args.len != 5 {
		eprintln('v compress <method> <in> <out>')
		eprintln('supported methods: zlib')
		exit(1)
	}
	method := match os.args[2] {
		'zlib' {
			CompressionMethod.zlib
		}
		else {
			eprintln('unsupported method: ${os.args[1]}')
			exit(1)
		}
	}
	path := os.args[3]
	content := os.read_bytes(path) or {
		eprintln('unable to read "$path": $err')
		exit(1)
	}
	compressed := match method {
		.zlib {
			zlib.compress(content) or {
				eprintln('compression error: $err')
				exit(1)
			}
		}
	}
	// cache_key := md5.hexhash(os.abs_path(path))
	// out_path := os.join_path(os.vmodules_dir(), 'cache', 'embed_file', cache_key)
	out_path := os.args[4]

	os.write_file(out_path, compressed.bytestr()) or {
		eprintln('failed to write "$path": $err')
		exit(1)
	}
}
