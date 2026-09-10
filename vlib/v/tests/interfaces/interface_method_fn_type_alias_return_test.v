type MathOp = fn (int, int) int

type EquivalentMathOp = fn (int, int) int

type MyInt = int

type AliasedMathOp = fn (MyInt, MyInt) MyInt

@[callconv: cdecl]
type CdeclMathOp = fn (int, int) int

type MathFactory = fn () MathOp

@[callconv: cdecl]
type CdeclMathFactory = fn () CdeclMathOp

interface Calculator {
	get_operation() MathOp
}

interface CdeclCalculator {
	get_operation() CdeclMathOp
}

interface AliasedCalculator {
	get_operation() AliasedMathOp
}

interface FactoryCalculator {
	get_factory() MathFactory
}

interface OptionalCalculator {
	get_operation() ?MathOp
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

struct AliasCalc {}

fn (c AliasCalc) get_operation() MathOp {
	_ = c
	return fn (a int, b int) int {
		return a - b
	}
}

struct CdeclFactoryCalc {}

fn (c CdeclFactoryCalc) get_factory() CdeclMathFactory {
	_ = c
	return fn () CdeclMathOp {
		return fn (a int, b int) int {
			return a / b
		}
	}
}

struct OptionalSimpleCalc {}

fn (c OptionalSimpleCalc) get_operation() ?EquivalentMathOp {
	_ = c
	return fn (a int, b int) int {
		return a + b
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

fn test_interface_method_fn_type_component_alias_matches_parent() {
	calc := AliasedCalculator(AliasCalc{})
	operation := calc.get_operation()
	assert operation(8, 3) == 5
}

fn test_interface_method_nested_fn_type_alias_omitted_callconv_matches_cdecl() {
	calc := FactoryCalculator(CdeclFactoryCalc{})
	factory := calc.get_factory()
	operation := factory()
	assert operation(12, 3) == 4
}

fn test_interface_method_option_fn_type_alias_return() {
	calc := OptionalCalculator(OptionalSimpleCalc{})
	operation := calc.get_operation() or { panic('missing operation') }
	assert operation(2, 3) == 5
}
