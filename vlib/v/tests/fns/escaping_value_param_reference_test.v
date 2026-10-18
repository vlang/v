// Like V1, a by-value parameter whose address escapes the call (stored in a returned
// struct literal, directly or through another call) is moved to the heap at entry.
@[heap]
struct EscExpr {
	value int
}

struct EscPlain {
	value int
}

struct EscKind {
mut:
	lhs ?&EscExpr
	rhs &EscExpr  = unsafe { nil }
	any &EscPlain = unsafe { nil }
}

struct EscStmt {
	kind EscKind
}

struct EscParser {
	start int
}

fn esc_expr(value int) EscExpr {
	return EscExpr{
		value: value
	}
}

fn (p &EscParser) statement(kind EscKind) EscStmt {
	return EscStmt{
		kind: kind
	}
}

fn (mut p EscParser) binary(lhs EscExpr, value int) (EscStmt, int) {
	rhs := esc_expr(value)
	return p.statement(EscKind{
		lhs: &lhs
		rhs: &rhs
	}), p.start
}

fn plain_holder(plain EscPlain) EscKind {
	return EscKind{
		any: &plain
	}
}

fn (plain EscPlain) holder() EscKind {
	return EscKind{
		any: &plain
	}
}

fn esc_overwrite_stack(seed int) int {
	mut values := [64]int{}
	for i in 0 .. values.len {
		values[i] = seed + i
	}
	return values[32]
}

fn test_heap_struct_param_address_in_call_argument() {
	mut p := EscParser{}
	stmt, _ := p.binary(esc_expr(3), 4)
	esc_overwrite_stack(7)
	assert stmt.kind.rhs.value == 4
	lhs := stmt.kind.lhs or { panic('missing lhs') }
	assert lhs.value == 3
}

fn test_plain_struct_param_address_in_returned_literal() {
	kind := plain_holder(EscPlain{5})
	receiver_kind := EscPlain{6}.holder()
	esc_overwrite_stack(9)
	assert kind.any.value == 5
	assert receiver_kind.any.value == 6
}
