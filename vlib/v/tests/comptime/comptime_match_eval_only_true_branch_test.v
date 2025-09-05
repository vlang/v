module main

fn func[T]() bool {
	$match T {
		u8, u16 {
			return true
		}
		$else {
			// return false
			$compile_error('fail')
		}
	}
}

fn test_comptime_match_eval_only_true_branch() {
	assert func[u8]()
}
