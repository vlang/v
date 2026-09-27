import modifiers
import flags

fn test_enum_method_homonyms() {
	assert modifiers.Modifier.none.has(.none)
	assert modifiers.matches(.none)
	assert !modifiers.matches(.shift)
	assert flags.Modifier.shift.has(.shift)
	assert !flags.Modifier.shift.has(.ctrl)
}
