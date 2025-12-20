// AVX2 (Advanced Vector Extensions 2) Instruction Set
// Added in 2008 with Intel Haswell and AMD Excavator
// AVX2 extends most integer SSE and AVX instructions to 256 bits and adds new
// instructions for integer operations, gather operations, and more.
import benchmark

const max_iterations = 100_000_000

fn avx2_path_matcher(path &u8, known_paths &u8, num_paths int, path_len int) int {
	$if amd64 && !tinyc && !msvc {
		unsafe {
			valid_mask := (1 << path_len) - 1 // e.g., path_len=7 → 0b1111111 = 127

			for i := 0; i < num_paths; i++ {
				known_path_ptr := known_paths + i * path_len // pointer arithmetic in unsafe
				mut match_found := 0

				asm volatile amd64 {
					vmovdqu ymm0, [path] // Load 32 bytes from test path (padded)
					vmovdqu ymm1, [known_path_ptr] // Load 32 bytes from known path (padded)
					vpcmpeqb ymm0, ymm0, ymm1 // Compare: ymm0 = (path == known) ? 0xFF : 0x00
					vpmovmskb eax, ymm0 // Move byte mask to eax (32-bit)
					and eax, valid_mask // Zero out bits beyond path_len
					cmp eax, valid_mask // Check if all valid bits are 1
					sete cl // cl = 1 if full match within path_len
					movzx ecx, cl // zero-extend byte to int
					mov match_found, ecx // store result
					vzeroupper // clear upper YMM state
					; +r (match_found)
					; r (path)
					  r (known_path_ptr)
					  r (valid_mask)
					; ymm0
					  ymm1
					  eax
					  ecx
					  cl
					  cc
					  memory
				}

				if match_found == 1 {
					return i
				}
			}
			return -1
		}
	}
	return -1 // AVX2 not supported
}

const path_len = 16

fn main() {
	// Known static HTTP paths (will be left-aligned and zero-padded to 16 bytes)
	paths := [
		'/'.bytes(),
		'/about'.bytes(),
		'/contact'.bytes(),
		'/api/v1'.bytes(),
		'/api/v2'.bytes(),
		'/static'.bytes(),
		'/favicon.ico'.bytes(),
		'/robots.txt'.bytes(),
	]

	num_paths := paths.len

	// Flat array: num_paths × path_len bytes, zero-initialized
	mut known_paths := []u8{len: num_paths * path_len, init: 0}
	for i, path in paths {
		bytes := path
		for j in 0 .. bytes.len {
			known_paths[i * path_len + j] = bytes[j]
		}
		// trailing bytes remain 0 → padding
	}

	// Test path
	mut test_path_bytes := '/api/v1'.bytes()

	mut b := benchmark.start()
	mut idx := -1
	for i := 0; i < max_iterations; i++ {
		idx = avx2_path_matcher(unsafe { &test_path_bytes[0] }, unsafe { &known_paths[0] },
			num_paths, path_len)
	}
	b.measure('avx2_path_matcher for ${max_iterations} iterations')

	if idx >= 0 {
		println('Matched path: ${paths[idx].bytestr()} (index ${idx})')
	} else {
		println('No match found')
	}

	assert idx == 3 // '/api/v1' is at index 3
	println('Success! AVX2 path matcher works correctly.')
}
