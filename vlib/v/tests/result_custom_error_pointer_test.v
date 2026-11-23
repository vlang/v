import semver

struct PointerResultError {
	Error
	detail string
}

fn (err PointerResultError) msg() string {
	return err.detail
}

fn pointer_result_failure() !int {
	return &PointerResultError{ detail: 'pointer result' }
}

fn test_pointer_result_errors_preserve_type_and_message() {
	value := pointer_result_failure() or {
		assert err is PointerResultError
		assert err.msg() == 'pointer result'
		17
	}
	assert value == 17
}

fn test_imported_pointer_result_errors_match_concrete_types() {
	semver.from('asd') or {
		match err {
			semver.InvalidVersionFormatError {
				assert err.msg() == 'Invalid version format for input "asd"'
			}
			else {
				assert false, 'lost invalid-version error type'
			}
		}
	}
	semver.from('') or {
		match err {
			semver.EmptyInputError {
				assert err.msg() == 'Empty input'
			}
			else {
				assert false, 'lost empty-input error type'
			}
		}
	}
}
