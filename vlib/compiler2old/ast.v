module compiler2

enum Kind {
	function,
	variable,
	bin_op,
}

struct AST {

}

struct Token {

}

enum Visibility {
	public
	private
}

pub enum IntType {
    // int_size,
	int_i8,
    int_i16,
    int_i32,
    int_i64,
    int_i128,
}

pub enum UintType {
    // uint_size,
    uint_u8,
    uint_u16,
    uint_u32,
    uint_u64,
    uint_u128,
}

pub enum ItemKind {
    _const
    _fn
    _module
    type_alias
    _enum
    _struct
    _union
}


struct Item {
	kind ItemKind
	tokens []Token
	items  []Item

	bin_op BinOp

}

struct BinOp {
	// left  Item
	// right Item
	token Token
}

enum BinOpKind {
	add,     // The `+` operator (addition)
	sub,     // The `-` operator (subtraction)
	mul,     // The `*` operator (multiplication)
	div,     // The `/` operator (division)
	rem,     // The `%` operator (modulus)
	and,     // The `&&` operator (logical and)
	_or,      // The `||` operator (logical or)
	bit_xor, // The `^` operator (bitwise xor)
	bit_and, // The `&` operator (bitwise and)
	bit_or,  // The `|` operator (bitwise or)
	shl,     // The `<<` operator (shift left)
	shr,     // The `>>` operator (shift right)
	eq,      // The `==` operator (equality)
	lt,      // The `<` operator (less than)
	le,      // The `<=` operator (less than or equal to)
	ne,      // The `!=` operator (not equal to)
	ge,      // The `>=` operator (greater than or equal to)
	gt,      // The `>` operator (greater than)
}

// struct BinOp bin_op_kind


struct Function {

}

// pub struct Item {
// 	pub ident: Ident,
// 	pub attrs: Vec<Attribute>,
// 	pub id: NodeId,
// 	pub node: ItemKind,
// 	pub vis: Visibility,
// 	pub span: Span,

// 	/// Original tokens this item was parsed from. This isn't necessarily
// 	/// available for all items, although over time more and more items should
// 	/// have this be `Some`. Right now this is primarily used for procedural
// 	/// macros, notably custom attributes.
// 	///
// 	/// Note that the tokens here do not include the outer attributes, but will
// 	/// include inner attributes.
// 	pub tokens: Option<TokenStream>,
// }