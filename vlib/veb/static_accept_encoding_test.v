module veb

import compress.gzip
import compress.zstd
import os

fn test_static_sidecars_obey_encoding_offers_and_refusals() {
	root := os.join_path(os.vtmp_dir(), 'veb_encoding_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'content.txt')
	body := 'static content '.repeat(100)
	os.write_file(path, body)!
	os.write_file('${path}.gz', gzip.compress(body.bytes())!.bytestr())!
	os.write_file('${path}.zst', zstd.compress(body.bytes())!.bytestr())!
	offers := {
		'':                          ''
		'gzip;q=0':                  ''
		'gzip;\tq=0':                ''
		'gzip;q=0, zstd;q=0':        ''
		'*;q=0':                     ''
		'GZIP':                      'gzip'
		'ZSTD':                      'zstd'
		'*':                         'zstd'
		'gzip;q=0, *;q=1':           'zstd'
		'zstd;q=0, gzip;q=1':        'gzip'
		'*;q=1, zstd;q=0, gzip;q=0': ''
	}
	for offer, expected in offers {
		mut ctx := Context{
			enable_static_compression:   true
			static_compression_max_size: 0
		}
		ctx.req.header.set(.accept_encoding, offer)
		ctx.send_file('text/plain', path)
		assert (ctx.res.header.get(.content_encoding) or { '' }) == expected, offer
		ext := match expected {
			'gzip' { '.gz' }
			'zstd' { '.zst' }
			else { '' }
		}
		assert ctx.return_type == .file
		assert ctx.return_file == '${path}${ext}', offer
		assert ctx.already_compressed == (expected != '')
	}
}
