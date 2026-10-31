module testing

import os

// replace_record atomically replaces dst with src on the same filesystem.
fn replace_record(src string, dst string) ! {
	os.rename(src, dst)!
}
