module main

struct EmptyExpr {
}

struct BasicLiteral {
	value int
}

struct Ident {
	name string
}

type Type = ArrayFixedType | ArrayType

type Expr = ArrayInitExpr | BasicLiteral | EmptyExpr | Ident | Type

struct ArrayType {
	elem_type Expr
}

struct ArrayFixedType {
	len       Expr
	elem_type Expr
}

struct ArrayInitExpr {
	typ   Expr
	exprs []Expr
}

struct FieldDecl {
	name  string
	value Expr
}

fn fixed_len_from_type(typ Type) int {
	return match typ {
		ArrayFixedType {
			if typ.len is BasicLiteral {
				typ.len.value
			} else {
				-2
			}
		}
		ArrayType {
			-3
		}
	}
}

fn type_to_len(expr Expr) int {
	return match expr {
		Type {
			fixed_len_from_type(expr)
		}
		else {
			-1
		}
	}
}

fn field_len(field FieldDecl) int {
	if field.value is ArrayInitExpr {
		arr := field.value
		return type_to_len(arr.typ)
	}
	return -4
}

fn main() {
	field := FieldDecl{
		name:  'buf'
		value: Expr(ArrayInitExpr{
			typ:   Expr(Type(ArrayFixedType{
				len:       Expr(BasicLiteral{
					value: 128
				})
				elem_type: Expr(Ident{
					name: 'u8'
				})
			}))
			exprs: []Expr{}
		})
	}
	println(field_len(field))
}
