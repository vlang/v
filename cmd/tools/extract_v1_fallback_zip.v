import compress.szip
import os

// The V1 fallback release is a ZIP, even on Unix. Use V's ZIP reader when
// the bootstrap host has neither bsdtar nor unzip.
fn main() {
	if os.args.len != 3 {
		eprintln('usage: extract_v1_fallback_zip <archive> <destination>')
		exit(2)
	}
	szip.extract_zip_to_dir(os.args[1], os.args[2]) or {
		eprintln('Could not extract the V1 fallback ZIP: ${err}')
		exit(1)
	}
}
