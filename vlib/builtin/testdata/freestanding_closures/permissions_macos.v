module main

type MetadataProbe = fn ()

fn check_macos_permissions(callback fn () int) {
	$if arm64 {
		require_closure(C.v_closure_test_permissions(unsafe { voidptr(callback) }) == 1)
	} $else $if amd64 {
		$if closure_write_code ? {
			println('closure code write probe reached')
			unsafe { &u8(voidptr(callback))[0] = 0xc3 }
			exit(93)
		}
		$if closure_execute_metadata ? {
			// The code and metadata remain separate: writing metadata is valid,
			// executing even a return instruction from that page must fault.
			metadata := unsafe { &u8(voidptr(callback)) - 16384 }
			unsafe { metadata[0] = 0xc3 }
			println('closure metadata execute probe reached')
			execute := unsafe { MetadataProbe(metadata) }
			execute()
			exit(93)
		}
	}
}
