type MathOp = fn (int, int) int

interface Calculator {
	get_operation() MathOp
}

struct SimpleCalc {}

fn (s SimpleCalc) get_operation() MathOp {
	_ = s
	return fn (a int, b int) int {
		return a + b
	}
}

fn test_interface_method_fn_type_alias_return() {
	calc := Calculator(SimpleCalc{})
	operation := calc.get_operation()
	assert operation(2, 3) == 5
}
