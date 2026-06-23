struct AssertAsCastPayload {
	vals []string
}

struct AssertAsCastOther {}

type AssertAsCastSum = AssertAsCastOther | AssertAsCastPayload

fn assert_as_cast_sum() AssertAsCastSum {
	return AssertAsCastPayload{
		vals: ['foo', 'bar']
	}
}

fn assert_as_cast_sums() []AssertAsCastSum {
	return [
		AssertAsCastOther{},
		AssertAsCastPayload{
			vals: ['foo', 'bar']
		},
	]
}

fn test_assert_selector_after_as_cast_is_codegen_safe() {
	assert (assert_as_cast_sum() as AssertAsCastPayload).vals == ['foo', 'bar']
}

fn test_assert_selector_after_filtered_as_cast_is_codegen_safe() {
	assert (assert_as_cast_sums().filter(it is AssertAsCastPayload)[0] as AssertAsCastPayload).vals == [
		'foo',
		'bar',
	]
}
