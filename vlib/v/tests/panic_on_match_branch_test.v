module main

pub struct Operand {
pub:
	typ OperandType = .reg_state
}

pub enum OperandType {
	reg_self
	reg_state
}

struct Value {
}

fn set_value(operand Operand, val2 &Value) {
	val1 := match operand.typ {
		.reg_state {
			val2
		}
		.reg_self {
			panic('ERR')
		}
	}
	assert val1.str() == val2.str()
}

fn test_main() {
	mut val := Value{}
	set_value(Operand{}, &val)
}
