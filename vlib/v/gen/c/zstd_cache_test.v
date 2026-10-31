module c

import os

fn test_zstd_cache_header_keeps_public_declarations_without_implementation() ! {
	source := os.read_file(os.join_path(@VEXEROOT, 'thirdparty', 'zstd', 'zstd.c'))!
	header := c_zstd_cache_header(source)
	assert header.contains('ZSTD_compress(')
	assert header.contains('ZSTD_decompress(')
	assert header.contains('ZSTD_ErrorCode')
	assert !header.contains('start inlining common/zstd_common.c')
	assert !header.contains('ZSTD_compressBlock_internal')
	assert c_zstd_cache_header('not an amalgamation') == ''
}
