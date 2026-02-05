module builtin

// FieldData holds information about a field. Fields reside on structs.
// This is used by compile-time reflection ($for field in T.fields)
pub struct FieldData {
pub:
	name          string // the name of the field f
	typ           int    // the internal TypeID of the field f,
	unaliased_typ int    // if f's type was an alias of int, this will be TypeID(int)

	attrs    []string // the attributes of the field f
	is_pub   bool     // f is in a `pub:` section
	is_mut   bool     // f is in a `mut:` section
	is_embed bool     // f is a embedded struct

	is_shared bool // `f shared Abc`
	is_atomic bool // `f atomic int`
	is_option bool // `f ?string`

	is_array  bool // `f []string`
	is_map    bool // `f map[string]int`
	is_chan   bool // `f chan int`
	is_enum   bool // `f Enum` where Enum is an enum
	is_struct bool // `f Abc` where Abc is a struct
	is_alias  bool // `f MyInt` where `type MyInt = int`

	indirections u8 // 0 for `f int`, 1 for `f &int`, 2 for `f &&int`
}
