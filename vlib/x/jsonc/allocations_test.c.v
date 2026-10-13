// vtest vflags: -gc none -d track_heap -nocache
module jsonc

import json2
import math

#include "@VMODROOT/vlib/json2/testdata/json5_heap_tracking.h"

fn C.json5_heap_live_bytes() u64

@[manualfree]
fn test_document_rendering_releases_scratch_storage() {
	// Reuse parsed storage and release every rendered result to isolate scratch buffers.
	for input in ['{"value":1e400,"text":"Infinity"}', '"' + 'x'.repeat(8192) + '"'] {
		doc := parse_text(input)!
		first := doc.str()
		unsafe { first.free() }
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 100 {
			rendered := doc.str()
			unsafe { rendered.free() }
		}
		assert C.json5_heap_live_bytes() == before
	}
	// NaN expands from three bytes to four, exercising output buffer growth.
	doc := Doc(json2.new_doc(json2.Any([json2.Any(math.nan()), json2.Any(math.nan())])))
	before := C.json5_heap_live_bytes()
	for _ in 0 .. 100 {
		rendered := doc.str()
		assert rendered == '[null,null]'
		unsafe { rendered.free() }
	}
	assert C.json5_heap_live_bytes() == before
}
