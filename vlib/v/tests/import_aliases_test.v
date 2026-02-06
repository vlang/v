// for issue 19899: Multiple import some_mod as _ results in error
import os as _
import io as _

fn test_multiple_import_as_blank_ident() {
	assert true
}
