type MathOp = fn (int, int) int

@[callconv: cdecl]
type CdeclMathOp = fn (int, int) int

interface Calculator {
	get_operation() MathOp
}

interface CdeclCalculator {
	get_operation() CdeclMathOp
}

struct SimpleCalc {}

fn (s SimpleCalc) get_operation() MathOp {
	_ = s
	return fn (a int, b int) int {
		return a + b
	}
}

struct CdeclCalc {}

fn (c CdeclCalc) get_operation() CdeclMathOp {
	_ = c
	return fn (a int, b int) int {
		return a - b
	}
}

struct PlainCalc {}

fn (c PlainCalc) get_operation() MathOp {
	_ = c
	return fn (a int, b int) int {
		return a * b
	}
}

fn test_interface_method_fn_type_alias_return() {
	calc := Calculator(SimpleCalc{})
	operation := calc.get_operation()
	assert operation(2, 3) == 5
}

fn test_interface_method_fn_type_alias_omitted_callconv_matches_cdecl() {
	cdecl_calc := Calculator(CdeclCalc{})
	cdecl_operation := cdecl_calc.get_operation()
	assert cdecl_operation(5, 3) == 2

	plain_calc := CdeclCalculator(PlainCalc{})
	plain_operation := plain_calc.get_operation()
	assert plain_operation(5, 3) == 15
}
