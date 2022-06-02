module compress

#flag -I @VEXEROOT/thirdparty/zip
#include "miniz.h"

pub const max_size = u64(1 << 31)

fn C.tdefl_compress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr
fn C.tinfl_decompress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr
