// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
import encoding.hex
import x.crypto.ascon

// The material was generated from https://hashing.tools/ascon/ascon-hash
fn main() {
	msg := 'Example of hash256 message'.bytes()
	// expected output generated from the tool
	digest := hex.decode('0889515a9cfe28ab3a43882884d5933bb74aa09f3c767f8c699b5d7114811340')!

	out := ascon.sum256(msg)
	dump(out == digest) // out == digest: true
}
