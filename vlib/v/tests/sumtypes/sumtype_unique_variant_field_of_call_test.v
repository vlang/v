// Like V1, a field that only one variant of a sum type has can be read from a sum
// value that cannot be smartcast, such as a call result or an array element.
struct RangeAuto {}

struct RangeOmitted {}

struct RangeEntries {
mut:
	entries []int
}

type DefinitionRange = RangeAuto | RangeEntries | RangeOmitted

struct RangeDefinition {
	value DefinitionRange
}

fn (d RangeDefinition) range() DefinitionRange {
	return d.value
}

fn resolved_entries(d RangeDefinition) []int {
	return match d.range() {
		RangeOmitted { [1] }
		RangeEntries { d.range().entries }
		RangeAuto { [2] }
	}
}

fn test_unique_variant_field_of_call_result() {
	assert resolved_entries(RangeDefinition{RangeEntries{[5, 6]}}) == [5, 6]
	assert resolved_entries(RangeDefinition{RangeAuto{}}) == [2]
	assert resolved_entries(RangeDefinition{RangeOmitted{}}) == [1]
}

fn test_unique_variant_field_of_array_element() {
	ranges := [DefinitionRange(RangeAuto{}), RangeEntries{[3]}]
	if ranges[1] is RangeEntries {
		assert ranges[1].entries == [3]
	}
	assert ranges[1].entries.len == 1
}
