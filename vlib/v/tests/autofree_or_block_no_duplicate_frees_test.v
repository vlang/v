import os
import compress.gzip

// Test that autofree doesn't generate duplicate free statements after return in or blocks
fn test_autofree_or_block_no_duplicate_frees() {
$if !autofree {
eprintln('This test requires -autofree flag. Skipping.')
return
}

// This test needs to actually check generated C code
// For now, we just verify it compiles and runs
data := 'test'.bytes()
_ := gzip.compress(data) or {
msg := 'error'
eprintln('Got ${msg}')
return
}
}
