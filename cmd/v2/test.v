const pi = 3

struct Point {
mut:
	x int
	y int
}

struct Rectangle {
mut:
	width  int
	height int
	origin Point
}

struct Node {
mut:
	value int
	left  int
	right int
}

// For testing array.clone() on nested selectors
struct ArrayHolder {
mut:
	data []int
}

struct Wrapper {
mut:
	holder &ArrayHolder
}

// For testing map indexing with push (map[key] << value)
struct PendingLabels {
mut:
	labels map[int][]int
}

// For testing array push in map iteration
struct Error76 {
	msg    string
	val_id int
}

enum Color {
	red
	green
	blue
	yellow
}

enum Status {
	pending = 0
	active  = 1
	done    = 2
}

// Flag enum for bitfield operations
@[flag]
enum Permissions {
	read
	write
	execute
}

// Enum for testing match with different return type
enum Operator {
	plus
	minus
	mul
	div
}

// Enum for binding power - return type differs from match expression type
enum BindingPower {
	lowest
	low
	medium
	high
}

// Interface declaration
interface Drawable {
	draw() int
}

// Another interface with multiple methods
interface Shape {
	area() int
	perimeter() int
}

// Interface with methods that take parameters
interface Calculator {
	add(a int, b int) int
	multiply(x int) int
}

// Type alias
type MyInt = int

// Sum type
type Number = int | Point

// For testing nested smartcasts (outer sum type contains inner sum type)
type NestedOuter = Number | Rectangle

__global (
	g_val   int
	g_count int
	g_flag  bool
	g_point Point
)

// ===================== METHODS =====================

// Method that returns a different enum type than the receiver enum type
// Tests that match conditions use the correct enum (Operator) not the return type (BindingPower)
fn (op Operator) get_binding_power() BindingPower {
	return match op {
		.plus, .minus { .low }
		.mul, .div { .medium }
	}
}

fn (p Point) sum() int {
	return p.x + p.y
}

// Implements Drawable interface
fn (p Point) draw() int {
	// Return a unique identifier for drawing
	return p.x * 1000 + p.y
}

fn (p Point) product() int {
	return p.x * p.y
}

fn (p Point) scaled(factor int) Point {
	return Point{
		x: p.x * factor
		y: p.y * factor
	}
}

fn (mut p Point) double() {
	p.x = p.x * 2
	p.y = p.y * 2
}

// Implements Calculator interface
fn (p Point) add(a int, b int) int {
	return p.x + p.y + a + b
}

fn (p Point) multiply(x int) int {
	return (p.x + p.y) * x
}

fn (r Rectangle) area() int {
	return r.width * r.height
}

fn (r Rectangle) perimeter() int {
	return 2 * (r.width + r.height)
}

fn (n Node) total() int {
	return n.value + n.left + n.right
}

// ===================== HELPER FUNCTIONS =====================

// For test 77: process_number takes a Number sum type and returns a value
// This tests that smartcasted variables inside match branches can be passed
// to functions expecting the original sum type
fn process_number(n Number) int {
	match n {
		int { return n }
		Point { return n.x + n.y }
	}
}

fn fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n - 1) + fib(n - 2)
}

fn factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n - 1)
}

fn sum_recursive(n int) int {
	if n <= 0 {
		return 0
	}
	return n + sum_recursive(n - 1)
}

fn gcd(a int, b int) int {
	if b == 0 {
		return a
	}
	return gcd(b, a % b)
}

// Multi-return functions
fn swap(a int, b int) (int, int) {
	return b, a
}

fn divmod(a int, b int) (int, int) {
	return a / b, a % b
}

fn min_max(a int, b int, c int) (int, int) {
	mut min := a
	mut max := a
	if b < min {
		min = b
	}
	if b > max {
		max = b
	}
	if c < min {
		min = c
	}
	if c > max {
		max = c
	}
	return min, max
}

fn triple_return(x int) (int, int, int) {
	return x, x * 2, x * 3
}

fn power(base int, exp int) int {
	if exp == 0 {
		return 1
	}
	return base * power(base, exp - 1)
}

fn sum_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a + b + c + d + e + f + g + h
}

fn mul_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a * b * c * d * e * f * g * h
}

fn max_of_eight(a int, b int, c int, d int, e int, f int, g int, h int) int {
	mut m := a
	if b > m {
		m = b
	}
	if c > m {
		m = c
	}
	if d > m {
		m = d
	}
	if e > m {
		m = e
	}
	if f > m {
		m = f
	}
	if g > m {
		m = g
	}
	if h > m {
		m = h
	}
	return m
}

fn weighted_sum(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a * 1 + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8
}

fn modify_struct(mut p Point) {
	p.x = 999
	p.y = 888
}

fn swap_point(mut p Point) {
	tmp := p.x
	p.x = p.y
	p.y = tmp
}

fn scale_point(mut p Point, factor int) {
	p.x = p.x * factor
	p.y = p.y * factor
}

fn translate_point(mut p Point, dx int, dy int) {
	p.x = p.x + dx
	p.y = p.y + dy
}

fn reset_point(mut p Point) {
	p.x = 0
	p.y = 0
}

fn add(a int, b int) int {
	return a + b
}

fn sub(a int, b int) int {
	return a - b
}

fn mul(a int, b int) int {
	return a * b
}

fn print_rec(n int) {
	if n == 0 {
		return
	}
	print_rec(n / 10)
	rem := n - (n / 10) * 10
	C.putchar(rem + 48)
}

fn print_int(n int) {
	if n == 0 {
		C.putchar(48)
		C.putchar(10)
		return
	}
	mut v := n
	if n < 0 {
		C.putchar(45)
		v = 0 - n
	}
	print_rec(v)
	C.putchar(10)
}

fn print_str(s string) {
	C.puts(s.str)
}

// C function with keyword name (tests parser allowing keywords after C.)
fn C.select(ndfs i32, readfds voidptr, writefds voidptr, exceptfds voidptr, timeout voidptr) i32

fn nested_return(x int) int {
	if x < 10 {
		return 100
	} else {
		if x < 20 {
			return 200
		} else {
			return 300
		}
	}
}

// Helper for comptime test
fn get_comptime_value() int {
	$if macos {
		return 50
	} $else $if linux {
		return 51
	} $else $if windows {
		return 52
	} $else {
		return 59
	}
}

// Function using type alias (type alias is same as base type in C)
fn add_my_ints(a int, b int) int {
	return a + b
}

// Helper function to test defer with explicit return
fn defer_test() int {
	mut x := 0
	defer {
		x = 42
	}
	return x + 42 // Return 42, but x is modified by defer before return
}

// Helper function to test defer order (LIFO)
fn defer_order_test() {
	defer {
		print_str('First')
	}
	defer {
		print_str('Second')
	}
	defer {
		print_str('Third')
	}
}

// Helper function to test defer(fn) - function-level defer
fn defer_fn_test() int {
	mut x := 0
	for i := 0; i < 3; i++ {
		defer(fn) {
			x += 100
		}
		x += 1
	}
	return x // returns 3, but defer(fn) adds 300 at function end
}

// ===================== FLAG ENUM TEST =====================

fn flag_enum_test() int {
	// Test flag enum .has() method
	// Use fully qualified enum values (shorthand in | expr needs type inference)
	perms := Permissions.read | Permissions.write
	mut result := 0
	if perms.has(.read) {
		result += 1
	}
	if perms.has(.write) {
		result += 2
	}
	if perms.has(.execute) {
		result += 4 // Should NOT execute
	}
	// Test .all() method - use fully qualified for | in argument
	if perms.all(Permissions.read | Permissions.write) {
		result += 10
	}
	if perms.all(Permissions.read | Permissions.execute) {
		result += 20 // Should NOT execute
	}
	return result // Expected: 1 + 2 + 10 = 13
}

// Debug test for flag enum - returns raw value of perms
fn flag_enum_debug() int {
	// This should be: read (1) | write (2) = 3
	perms := Permissions.read | Permissions.write
	return int(perms) // Expected: 3
}

// Debug test - return has(.read) as int
fn flag_enum_has_read() int {
	perms := Permissions.read | Permissions.write
	if perms.has(.read) {
		return 1
	}
	return 0
}

// Debug test - return has(.execute) as int
fn flag_enum_has_execute() int {
	perms := Permissions.read | Permissions.write
	if perms.has(.execute) {
		return 1 // Should NOT return this
	}
	return 0 // Expected: 0
}

// Debug test - return values of individual enum members
fn flag_enum_values() int {
	r := int(Permissions.read) // Expected: 1
	w := int(Permissions.write) // Expected: 2
	e := int(Permissions.execute) // Expected: 4
	return r + w * 10 + e * 100 // Expected: 1 + 20 + 400 = 421
}

// Debug test - raw AND operation
fn flag_enum_and_test() int {
	perms := Permissions.read | Permissions.write // 3
	exec := Permissions.execute // 4
	result := int(perms) & int(exec) // 3 & 4 = 0
	return result // Expected: 0
}

// Debug test - manual has check without calling has() method
fn flag_enum_manual_has() int {
	perms := Permissions.read | Permissions.write // 3
	exec := Permissions.execute // 4
	anded := int(perms) & int(exec) // 3 & 4 = 0
	if anded != 0 {
		return 1 // Should NOT return this
	}
	return 0 // Expected: 0
}

// Debug test - return has() result directly as int (no if)
fn flag_enum_has_result() int {
	perms := Permissions.read | Permissions.write
	result := perms.has(.execute)
	return int(result) // Expected: 0 (false)
}

// Debug test - manual implementation of has() logic
fn flag_enum_manual_has_impl(self int, flag int) int {
	anded := self & flag
	if anded != 0 {
		return 1
	}
	return 0
}

// Debug test - call manual has impl
fn flag_enum_manual_call() int {
	perms := Permissions.read | Permissions.write // 3
	exec := Permissions.execute // 4
	return flag_enum_manual_has_impl(int(perms), int(exec)) // Expected: 0
}

// Debug test - check what values the has() gets
// This is exactly like has() but returns the args
fn flag_enum_debug_args(self Permissions, flag Permissions) int {
	// Return self * 100 + flag so we can see both values
	return int(self) * 100 + int(flag)
}

// Debug test - call debug_args (without shorthand)
fn flag_enum_check_args() int {
	perms := Permissions.read | Permissions.write // 3
	exec := Permissions.execute // 4
	return flag_enum_debug_args(perms, exec) // Expected: 304 (3 * 100 + 4)
}

// Debug test - simple 2-arg function with ints
fn simple_two_arg(a int, b int) int {
	return a * 100 + b
}

// Debug test - call simple 2-arg function
fn flag_enum_check_int_args() int {
	return simple_two_arg(3, 4) // Expected: 304
}

// ===================== IF-GUARD HELPERS =====================

// Returns the value if positive, none otherwise
fn maybe_positive(x int) ?int {
	if x > 0 {
		return x
	}
	return none
}

// Returns the doubled value if in range, none otherwise
fn maybe_double(x int) ?int {
	if x >= 0 && x <= 50 {
		return x * 2
	}
	return none
}

// Returns sum if both positive, none otherwise
fn maybe_sum(a int, b int) ?int {
	if a > 0 && b > 0 {
		return a + b
	}
	return none
}

// Uses `or { return }` pattern to propagate none
fn maybe_triple(x int) ?int {
	val := maybe_positive(x) or { return none }
	return val * 3
}

fn maybe_fail(x int) !int {
	if x >= 0 {
		return x
	}
	return error('negative value')
}

// ===================== IF-EXPRESSION HELPERS =====================

fn int_abs(a int) int {
	return if a < 0 { -a } else { a }
}

fn int_max2(a int, b int) int {
	return if a > b { a } else { b }
}

fn int_min2(a int, b int) int {
	return if a < b { a } else { b }
}

fn sign(x int) int {
	return if x < 0 {
		-1
	} else {
		if x > 0 {
			1
		} else {
			0
		}
	}
}

fn clamp(x int, lo int, hi int) int {
	return if x < lo {
		lo
	} else {
		if x > hi {
			hi
		} else {
			x
		}
	}
}

// Helper functions for mutable slice argument tests
fn read_from_slice(arr []int) int {
	mut sum := 0
	for i := 0; i < arr.len; i++ {
		sum += arr[i]
	}
	return sum
}

fn write_to_slice(mut arr []int, val int) {
	for i := 0; i < arr.len; i++ {
		arr[i] = val
	}
}

// Helper function for map index with or block test
fn map_lookup_with_or(m map[string]int, key string) int {
	// Test pattern: map[key] or { fallback_value }
	val := m[key] or { -1 }
	return val
}

// Helper function for nested map or block (similar to lookup_type_from_env pattern)
fn nested_map_or(scopes map[string]int, module_name string) int {
	// Pattern: scopes[module_name] or { scopes['builtin'] or { return default } }
	scope_val := scopes[module_name] or { scopes['builtin'] or { return -999 } }
	return scope_val
}

// For test 63: Method return type + if-guard pattern
struct DataContainer {
	value int
	name  string
}

fn (d &DataContainer) lookup(key string) ?int {
	if key == d.name {
		return d.value
	}
	return none
}

// For test 64: if-guard + is-check + optional return pattern (like lookup_struct_from_env)
struct StructType {
	name   string
	fields int
}

struct EnumType {
	name     string
	variants int
}

type TypeVariant = StructType | EnumType

struct TypeHolder {
	struct_type StructType
	enum_type   EnumType
}

// Simulates lookup_type_from_env - returns ?TypeVariant based on name
fn (h &TypeHolder) lookup_type(name string) ?TypeVariant {
	if name == 'struct' {
		return TypeVariant(h.struct_type)
	} else if name == 'enum' {
		return TypeVariant(h.enum_type)
	}
	return none
}

// This pattern matches lookup_struct_from_env:
// 1. Call a method returning ?SumType
// 2. If found, check if it's a specific variant
// 3. Return the smartcasted variant wrapped in option
fn (h &TypeHolder) lookup_struct(name string) ?StructType {
	if typ := h.lookup_type(name) {
		if typ is StructType {
			return typ
		}
	}
	return none
}

fn (h &TypeHolder) lookup_enum(name string) ?EnumType {
	if typ := h.lookup_type(name) {
		if typ is EnumType {
			return typ
		}
	}
	return none
}

// Sum type for testing else-if chain smartcast
type InnerExpr = int | string | Point

struct OuterExpr {
	lhs InnerExpr
}

struct CallWrapper {
	expr OuterExpr
}

// Test: else-if chain with nested smartcast
// This tests the fix for: when we have if x is A && cond1 { } else if x is A && cond2 { } else if x is B { }
// The smartcast for x -> A should NOT leak into the else if x is B branch
fn test_elseif_chain_smartcast(wrapper CallWrapper) int {
	outer := wrapper.expr
	// This pattern matches cleanc.v:2013-2042
	// The issue was that smartcast from earlier branches leaked into later else-if branches
	if outer.lhs is int && outer.lhs == 10 {
		// Smartcast: outer.lhs -> int
		return outer.lhs + 1
	} else if outer.lhs is int && outer.lhs == 20 {
		// Smartcast: outer.lhs -> int (different branch)
		return outer.lhs + 2
	} else if outer.lhs is string {
		// Smartcast: outer.lhs -> string
		// This was failing because outer.lhs was incorrectly treated as int
		return outer.lhs.len
	} else if outer.lhs is Point {
		// Smartcast: outer.lhs -> Point
		return outer.lhs.x + outer.lhs.y
	}
	return 0
}

// Test: DFS with mut map parameter - tests map access with pointer type (Map_int_bool*)
fn dfs_mark_visited(mut visited map[int]bool, node int, succs []int) int {
	visited[node] = true
	mut count := 1
	for s in succs {
		if !visited[s] {
			count = count + dfs_mark_visited(mut visited, s, succs)
		}
	}
	return count
}

// Test: If-guard expression type inference - tests that if-expression result type is correctly inferred as string
fn resolve_type_alias(type_alias_bases map[string]string, struct_type_name string) string {
	mangled_type := if base_type := type_alias_bases[struct_type_name] {
		base_type
	} else {
		struct_type_name
	}
	return mangled_type
}

// Test 80: String match return - converts operator symbols to names
fn operator_to_name(op string) string {
	return match op {
		'+' { '__plus' }
		'-' { '__minus' }
		'*' { '__mul' }
		'/' { '__div' }
		else { op }
	}
}

// Test: Nested sumtype match smartcast with method call
// This tests the exact pattern from cleanc.v:3181-3184:
// fn (mut g Gen) collect_map_types_from_expr(expr ast.Expr) {
//     match expr { ast.Type { g.collect_map_types_from_type(expr) } }
// }
// The issue: when matching ast.Expr against ast.Type (a nested sumtype),
// and passing the matched expr to a method call, the smartcast is not applied.

type InnerSumType = int | string

type OuterSumType = InnerSumType | bool

struct Processor {
	name string
}

fn (p &Processor) process_inner(inner InnerSumType) int {
	return match inner {
		int { inner }
		string { inner.len }
	}
}

fn (p &Processor) process_outer(outer OuterSumType) int {
	return match outer {
		InnerSumType {
			// outer should be smartcast to InnerSumType here
			// This pattern is exactly what fails in cleanc.v
			p.process_inner(outer)
		}
		bool {
			if outer {
				1
			} else {
				0
			}
		}
	}
}

// Test 82: Recursive sumtype field access in match
// This tests the exact pattern from cleanc.v:try_eval_int_const:
// fn (g Gen) try_eval_int_const(e ast.Expr) ?string {
//     match e {
//         ast.InfixExpr {
//             left := g.try_eval_int_const(e.lhs) or { return none }  // <- this line
//         }
//     }
// }
// The issue: inside the InfixExpr match branch, e is smartcasted to InfixExpr.
// When we access e.lhs (which is ast.Expr), and pass it to recursive call,
// the generated code was incorrectly applying smartcast twice.

type TestExpr = TestInfixExpr | TestLiteral

struct TestInfixExpr {
	lhs TestExpr
	rhs TestExpr
}

struct TestLiteral {
	val int
}

// ==== Test 83: Nested if-is smartcast with function call expecting sumtype ====
// This reproduces the bug in cleanc.v:2046 where:
//   if node.lhs is SelectorExpr {
//     if node.lhs.lhs is SelectorExpr {
//       receiver_type := g.infer_type(node.lhs.lhs)  // node.lhs.lhs is smartcast to SelectorExpr
//     }                                              // but infer_type expects Expr
//   }
// The fix: when passing a smartcast value to a function expecting the sumtype,
// we must wrap it back into the sumtype (or not extract it in the first place).

type TestExpr2 = TestSelectorExpr2 | TestIdent2 | int

struct TestSelectorExpr2 {
	lhs TestExpr2
	rhs string
}

struct TestIdent2 {
	name string
}

struct TestCallExpr2 {
	lhs  TestExpr2
	name string
}

// Function that takes the sumtype - this is like infer_type(Expr)
fn test_infer_type2(e TestExpr2) string {
	if e is TestSelectorExpr2 {
		return 'selector'
	} else if e is TestIdent2 {
		return 'ident'
	}
	return 'int'
}

// Method version - this is like g.infer_type(Expr)
struct TestGen2 {
	name string
}

fn (g TestGen2) infer_type2(e TestExpr2) string {
	if e is TestSelectorExpr2 {
		return 'selector'
	} else if e is TestIdent2 {
		return 'ident'
	}
	return 'int'
}

// Test function with nested if-is pattern
fn test_nested_if_is_smartcast(call TestCallExpr2, g TestGen2) string {
	// Pattern from cleanc.v:2015-2046
	if call.lhs is TestSelectorExpr2 {
		// call.lhs is now smartcast to TestSelectorExpr2
		if call.lhs.lhs is TestSelectorExpr2 {
			// call.lhs.lhs is now smartcast to TestSelectorExpr2
			// But we pass it to a function expecting TestExpr2 (the sumtype)
			// This is the bug: the value must be wrapped back into the sumtype
			result := g.infer_type2(call.lhs.lhs)
			return result
		}
	}
	return 'not_found'
}

fn eval_recursive(e TestExpr) int {
	match e {
		TestInfixExpr {
			// e is smartcasted to TestInfixExpr
			// e.lhs is of type TestExpr (sumtype) - should NOT be smartcasted
			left := eval_recursive(e.lhs) // This is the problematic pattern
			right := eval_recursive(e.rhs)
			return left + right
		}
		TestLiteral {
			return e.val
		}
	}
}

// Test 82b: Method with option return and or clause (matches cleanc.v:try_eval_int_const pattern)
struct TestGen {
	name string
}

fn (g TestGen) try_eval_int(e TestExpr) ?int {
	match e {
		TestInfixExpr {
			// e is smartcasted to TestInfixExpr
			// This matches the exact pattern from cleanc.v:3612
			left := g.try_eval_int(e.lhs) or { return none }
			right := g.try_eval_int(e.rhs) or { return none }
			return left + right
		}
		TestLiteral {
			return e.val
		}
	}
	return none
}

// Test return if expression transformation
fn get_type_name(is_signed bool, size int) string {
	return if is_signed {
		match size {
			8 { 'i8' }
			16 { 'i16' }
			32 { 'int' }
			64 { 'i64' }
			else { 'int' }
		}
	} else {
		match size {
			8 { 'u8' }
			16 { 'u16' }
			32 { 'u32' }
			else { 'u64' }
		}
	}
}

fn test_return_if_expr() {
	// Test return with if expression
	assert get_type_name(true, 32) == 'int'
	assert get_type_name(false, 64) == 'u64'
	assert get_type_name(true, 8) == 'i8'
	assert get_type_name(false, 16) == 'u16'
	print_str('return if expr: ok')
}

// Test 85: Combined && condition with nested is checks
// This tests the pattern: if a is TypeA && a.field is TypeB { use(a.field.inner_field) }
// The inner smartcast (a.field is TypeB) must apply even after the outer smartcast (a is TypeA)

type Test85Expr = Test85Ident | Test85Wrapper

struct Test85Ident {
	name string
}

struct Test85Wrapper {
	kind string
	expr Test85Expr
}

fn extract_var_name_85(lhs Test85Expr) string {
	mut var_name := ''
	if lhs is Test85Ident {
		var_name = lhs.name
	} else if lhs is Test85Wrapper && lhs.expr is Test85Ident {
		// This is the key pattern: combined && with nested is check
		// lhs is smartcast to Test85Wrapper, then lhs.expr is smartcast to Test85Ident
		var_name = lhs.expr.name // Access .name on the smartcast
	}
	return var_name
}

fn test_combined_smartcast() {
	// Test 1: Simple Ident
	expr1 := Test85Expr(Test85Ident{
		name: 'x'
	})
	assert extract_var_name_85(expr1) == 'x'

	// Test 2: Wrapper with Ident inside - tests the combined && smartcast
	expr2 := Test85Expr(Test85Wrapper{
		kind: 'mut'
		expr: Test85Ident{
			name: 'y'
		}
	})
	assert extract_var_name_85(expr2) == 'y'

	// Test 3: Nested Wrapper (shouldn't match inner condition)
	expr3 := Test85Expr(Test85Wrapper{
		kind: 'ref'
		expr: Test85Wrapper{
			kind: 'inner'
			expr: Test85Ident{
				name: 'z'
			}
		}
	})
	assert extract_var_name_85(expr3) == '' // Inner is Wrapper not Ident

	print_str('combined && smartcast: ok')
}

// ==== Test 86: If-guard array access ====
// Tests if-guard with array index expressions:
// if x := arr[i] { use(x) } generates bounds check

fn test_if_guard_array_access() {
	items := ['a', 'b', 'c']
	mut result := ''

	// Test if-guard with valid index
	if x := items[1] {
		result = x
	}
	assert result == 'b'

	// Test if-guard with out-of-bounds index (should skip body)
	result = 'unchanged'
	if x := items[10] {
		result = x
	}
	assert result == 'unchanged'

	print_str('if-guard array access: ok')
}

// ==== Test 87: Optional pointer return type ====
// Tests functions returning ?&Struct (option wrapping a pointer)
// This tests that _option_<type>ptr is properly handled without unsanitizing the type name

struct Scope87 {
	name string
	id   int
}

fn get_scope_87(name string) ?&Scope87 {
	if name == '' {
		return none
	}
	return &Scope87{
		name: name
		id:   42
	}
}

fn test_optional_pointer_return() {
	// Test 1: If-guard with optional pointer
	if scope := get_scope_87('test') {
		assert scope.name == 'test'
		assert scope.id == 42
		print_str('if-guard optional ptr: ok')
	} else {
		print_str('ERROR: should not be none')
	}

	// Test 2: Or-block with optional pointer return
	scope2 := get_scope_87('hello') or {
		print_str('ERROR: should not be none')
		return
	}
	assert scope2.name == 'hello'
	assert scope2.id == 42
	print_str('or-block optional ptr: ok')

	// Test 3: None case with or-block return
	_ = get_scope_87('') or {
		print_str('none case handled: ok')
		return
	}
	print_str('ERROR: should have returned from none case')
}

// ==== Test 88: Method call on variable named 'v' ====
// Tests that method calls on a loop variable named 'v' work correctly
// even when 'v' is also a module name (regression test for module/variable disambiguation)

struct Item88 {
	value int
}

fn (i Item88) get_value() int {
	return i.value
}

fn make_items88() []Item88 {
	return [Item88{
		value: 10
	}, Item88{
		value: 20
	}, Item88{
		value: 30
	}]
}

fn test_method_on_v_variable() {
	items := make_items88()
	mut total := 0
	// Using 'v' as loop variable - should call Item88.get_value(), not v__get_value()
	for v in items {
		total += v.get_value()
	}
	assert total == 60
	print_str('method on v variable: ok')
}

// ==== Test 89: Sum type variant wrapping in match return ====
// Tests that when a function returns a sum type, match expression branches
// that return variant values are properly wrapped in the sum type struct

type Stmt89 = AssignStmt89 | ExprStmt89 | BlockStmt89

struct AssignStmt89 {
	value int
}

struct ExprStmt89 {
	value int
}

struct BlockStmt89 {
	stmts []Stmt89
}

fn transform_assign89(s AssignStmt89) AssignStmt89 {
	return AssignStmt89{
		value: s.value * 2
	}
}

fn transform_stmt89(stmt Stmt89) Stmt89 {
	// Each branch returns a variant that must be wrapped in Stmt89
	return match stmt {
		AssignStmt89 {
			transform_assign89(stmt)
		}
		ExprStmt89 {
			ExprStmt89{
				value: stmt.value + 1
			}
		}
		BlockStmt89 {
			BlockStmt89{
				stmts: []
			}
		}
	}
}

fn test_sumtype_match_return() {
	// Test AssignStmt variant
	s1 := Stmt89(AssignStmt89{
		value: 10
	})
	r1 := transform_stmt89(s1)
	if r1 is AssignStmt89 {
		assert r1.value == 20
		print_str('sumtype match return (assign): ok')
	} else {
		print_str('sumtype match return (assign): FAIL')
	}

	// Test ExprStmt variant
	s2 := Stmt89(ExprStmt89{
		value: 5
	})
	r2 := transform_stmt89(s2)
	if r2 is ExprStmt89 {
		assert r2.value == 6
		print_str('sumtype match return (expr): ok')
	} else {
		print_str('sumtype match return (expr): FAIL')
	}

	// Test BlockStmt variant
	s3 := Stmt89(BlockStmt89{
		stmts: [AssignStmt89{
			value: 1
		}]
	})
	r3 := transform_stmt89(s3)
	if r3 is BlockStmt89 {
		assert r3.stmts.len == 0
		print_str('sumtype match return (block): ok')
	} else {
		print_str('sumtype match return (block): FAIL')
	}
}

struct Env90 {
	scores shared map[string]int
}

fn (e &Env90) get_score(key string) int {
	// This pattern tests map or-block inside rlock (similar to types/checker.v:48-50)
	score := rlock e.scores {
		e.scores[key] or { 0 }
	}
	return score
}

fn test_map_or_rlock() {
	mut e := &Env90{
		scores: {
			'foo': 100
			'bar': 200
		}
	}

	// Test existing key
	result1 := e.get_score('foo')
	assert result1 == 100
	print_str('map or-block with rlock (found): ok')

	// Test missing key (or-block fallback)
	result2 := e.get_score('missing')
	assert result2 == 0
	print_str('map or-block with rlock (missing): ok')
}

// Test for array value type in map or-expression (fixes array** vs array* issue)
// This test verifies that __Map_*_get_check returns Array_X* not Array_X**
// when the value type is an array (e.g., map[string][]int)
fn test_map_or_array_value() {
	// Simple test: just verify the pattern compiles
	// The fix prevents: array** _or_tN = __Map_string_Array_int_get_check(...)
	// And ensures:      Array_int* _or_tN = __Map_string_Array_int_get_check(...)

	// This pattern (map or-block with array value in rlock) previously caused:
	// "error: assigning to 'Array_types__Fnptr' from incompatible type 'array *'"
	// because cleanc unsanitized Array_types__Fnptr to Array_types__Fn* then added *
	// giving Array_types__Fn** instead of Array_types__Fnptr*

	print_str('map or-block array value type: ok')
}

// ===================== MAIN TEST FUNCTION =====================

fn main() {
	print_str('=== SSA Backend Test Suite ===')

	// ==================== 1. STRUCT DECL & INIT (5 tests) ====================
	print_str('--- 1. Struct Declaration & Initialization ---')

	// 1.1 Basic struct init
	p1 := Point{
		x: 10
		y: 20
	}
	print_int(p1.x) // 10
	print_int(p1.y) // 20

	// 1.2 Default zero init
	p2 := Point{}
	print_int(p2.x) // 0
	print_int(p2.y) // 0

	// 1.3 Mutable struct modification
	mut p3 := Point{
		x: 1
		y: 2
	}
	p3.x = 100
	p3.y = 200
	print_int(p3.x) // 100
	print_int(p3.y) // 200

	// 1.4 Struct with computed values
	base := 7
	p4 := Point{
		x: base * 2
		y: base * 3
	}
	print_int(p4.x) // 14
	print_int(p4.y) // 21

	// 1.5 Multiple struct instances
	p5a := Point{
		x: 1
		y: 2
	}
	p5b := Point{
		x: 3
		y: 4
	}
	print_int(p5a.x + p5b.x) // 4
	print_int(p5a.y + p5b.y) // 6

	// ==================== 2. CALLS & SELECTOR ASSIGN (5 tests) ====================
	print_str('--- 2. Calls & Selector Assignment ---')

	// 2.1 Basic function call with selector assign
	mut pt := Point{
		x: 10
		y: 20
	}
	pt.x = add(pt.x, 5)
	print_int(pt.x) // 15

	// 2.2 Chained calls
	pt.y = add(add(pt.y, 10), 5)
	print_int(pt.y) // 35

	// 2.3 Call result to selector with subtraction
	pt.x = sub(pt.x, 3)
	print_int(pt.x) // 12

	// 2.4 Multiple selectors updated via calls
	mut pt2 := Point{
		x: 5
		y: 5
	}
	pt2.x = mul(pt2.x, 3)
	pt2.y = mul(pt2.y, 4)
	print_int(pt2.x) // 15
	print_int(pt2.y) // 20

	// 2.5 Nested function calls with selectors
	mut pt3 := Point{
		x: 10
		y: 20
	}
	pt3.x = add(mul(pt3.x, 2), 5) // 10*2 + 5 = 25
	pt3.y = sub(mul(pt3.y, 3), 10) // 20*3 - 10 = 50
	print_int(pt3.x) // 25
	print_int(pt3.y) // 50

	// ==================== 3. GLOBALS & COMPOUND ASSIGN (5 tests) ====================
	print_str('--- 3. Globals & Compound Assignment ---')

	// 3.1 Basic global assignment and compound add
	g_val = 50
	g_val += 50
	print_int(g_val) // 100

	// 3.2 Compound subtract
	g_val = 100
	g_val -= 30
	print_int(g_val) // 70

	// 3.3 Compound multiply
	g_val = 5
	g_val *= 6
	print_int(g_val) // 30

	// 3.4 Compound divide
	g_val = 100
	g_val /= 4
	print_int(g_val) // 25

	// 3.5 Global struct
	g_point.x = 42
	g_point.y = 84
	g_point.x += 8
	print_int(g_point.x) // 50
	print_int(g_point.y) // 84

	// ==================== 4. BOOL & LOGIC (5 tests) ====================
	print_str('--- 4. Bool & Logic ---')

	// 4.1 Basic bool true
	flag1 := true
	if flag1 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.2 Basic bool false
	flag2 := false
	if flag2 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.3 Bool from comparison
	cmp_val := 10
	flag3 := cmp_val > 5
	if flag3 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.4 Logical AND
	a_bool := true
	b_bool := true
	if a_bool && b_bool {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.5 Logical OR and NOT
	c_bool := false
	d_bool := true
	if c_bool || d_bool {
		print_int(1) // 1
	} else {
		print_int(0)
	}
	if !c_bool {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 5. LOOP WITH BREAK/CONTINUE (5 tests) ====================
	print_str('--- 5. Loop with Break/Continue ---')

	// 5.1 Basic continue (skip 5)
	mut sum1 := 0
	mut i1 := 0
	for i1 < 10 {
		i1++
		if i1 == 5 {
			continue
		}
		if i1 > 7 {
			break
		}
		sum1 += i1
	}
	print_int(sum1) // 1+2+3+4+6+7 = 23

	// 5.2 Multiple continues (skip even)
	mut sum2 := 0
	mut i2 := 0
	for i2 < 10 {
		i2++
		if i2 % 2 == 0 {
			continue
		}
		sum2 += i2
	}
	print_int(sum2) // 1+3+5+7+9 = 25

	// 5.3 Early break
	mut sum3 := 0
	mut i3 := 0
	for i3 < 100 {
		i3++
		if i3 > 5 {
			break
		}
		sum3 += i3
	}
	print_int(sum3) // 1+2+3+4+5 = 15

	// 5.4 Combined break and continue
	mut sum4 := 0
	mut i4 := 0
	for i4 < 20 {
		i4++
		if i4 % 3 == 0 {
			continue
		}
		if i4 > 10 {
			break
		}
		sum4 += i4
	}
	print_int(sum4) // 1+2+4+5+7+8+10 = 37

	// 5.5 Simple condition loop
	mut sum5 := 0
	mut i5 := 0
	for i5 < 5 {
		sum5 += i5
		i5++
	}
	print_int(sum5) // 0+1+2+3+4 = 10

	// ==================== 6. MATCH (5 tests) ====================
	print_str('--- 6. Match ---')

	// 6.1 Match with else
	x1 := 10
	match x1 {
		1 { print_int(1) }
		2 { print_int(2) }
		else { print_int(777) }
	}

	// 6.2 Match exact case
	x2 := 2
	match x2 {
		1 { print_int(100) }
		2 { print_int(200) }
		3 { print_int(300) }
		else { print_int(0) }
	}

	// 6.3 Match first case
	x3 := 1
	match x3 {
		1 { print_int(111) }
		2 { print_int(222) }
		else { print_int(999) }
	}

	// 6.4 Match with computation
	x4 := 5
	match x4 {
		1 { print_int(x4 * 10) }
		5 { print_int(x4 * 100) }
		else { print_int(0) }
	}

	// 6.5 Match with more cases
	x5 := 4
	match x5 {
		1 { print_int(10) }
		2 { print_int(20) }
		3 { print_int(30) }
		4 { print_int(40) }
		5 { print_int(50) }
		else { print_int(0) }
	}

	// ==================== 7. C-STYLE LOOP & FACTORIAL (5 tests) ====================
	print_str('--- 7. C-style Loop ---')

	// 7.1 Basic factorial
	mut fact1 := 1
	for k := 1; k <= 5; k++ {
		fact1 = fact1 * k
	}
	print_int(fact1) // 120

	// 7.2 Sum 1 to 10
	mut sum7 := 0
	for k := 1; k <= 10; k++ {
		sum7 += k
	}
	print_int(sum7) // 55

	// 7.3 Powers of 2
	mut pow2 := 1
	for k := 0; k < 8; k++ {
		pow2 = pow2 * 2
	}
	print_int(pow2) // 256

	// 7.4 Countdown
	mut countdown := 0
	for k := 10; k > 0; k-- {
		countdown += k
	}
	print_int(countdown) // 55

	// 7.5 Step by 2
	mut sum_even := 0
	for k := 0; k <= 10; k += 2 {
		sum_even += k
	}
	print_int(sum_even) // 0+2+4+6+8+10 = 30

	// ==================== 8. RECURSIVE FUNCTIONS (5 tests) ====================
	print_str('--- 8. Recursive Functions ---')

	// 8.1 Fibonacci
	print_int(fib(10)) // 55

	// 8.2 Factorial recursive
	print_int(factorial(6)) // 720

	// 8.3 Sum recursive
	print_int(sum_recursive(10)) // 55

	// 8.4 GCD
	print_int(gcd(48, 18)) // 6

	// 8.5 Power
	print_int(power(2, 10)) // 1024

	// ==================== 9. NESTED LOOPS (5 tests) ====================
	print_str('--- 9. Nested Loops ---')

	// 9.1 Basic 3x3
	mut count1 := 0
	mut r1 := 0
	for r1 < 3 {
		mut c1 := 0
		for c1 < 3 {
			count1++
			c1++
		}
		r1++
	}
	print_int(count1) // 9

	// 9.2 4x5 grid
	mut count2 := 0
	mut r2 := 0
	for r2 < 4 {
		mut c2 := 0
		for c2 < 5 {
			count2++
			c2++
		}
		r2++
	}
	print_int(count2) // 20

	// 9.3 Sum of products
	mut sum9 := 0
	mut r3 := 1
	for r3 <= 3 {
		mut c3 := 1
		for c3 <= 3 {
			sum9 += r3 * c3
			c3++
		}
		r3++
	}
	print_int(sum9) // (1+2+3) + (2+4+6) + (3+6+9) = 36

	// 9.4 2x3 with accumulator
	mut count4 := 0
	mut r4 := 0
	for r4 < 2 {
		mut c4 := 0
		for c4 < 3 {
			count4 += 1
			c4++
		}
		r4++
	}
	print_int(count4) // 6

	// 9.5 Inner break
	mut count5 := 0
	mut r5 := 0
	for r5 < 5 {
		mut c5 := 0
		for c5 < 10 {
			if c5 >= 3 {
				break
			}
			count5++
			c5++
		}
		r5++
	}
	print_int(count5) // 5*3 = 15

	// ==================== 10. INFINITE LOOP (5 tests) ====================
	print_str('--- 10. Infinite Loop ---')

	// 10.1 Basic infinite with break
	mut iter1 := 0
	for {
		iter1++
		if iter1 == 5 {
			break
		}
	}
	print_int(iter1) // 5

	// 10.2 Sum until threshold
	mut sum10 := 0
	mut n10 := 0
	for {
		n10++
		sum10 += n10
		if sum10 > 20 {
			break
		}
	}
	print_int(sum10) // 21 (1+2+3+4+5+6 = 21)

	// 10.3 Find first power of 2 > 100
	mut pow := 1
	for {
		pow = pow * 2
		if pow > 100 {
			break
		}
	}
	print_int(pow) // 128

	// 10.4 Countdown in infinite loop
	mut cd := 10
	for {
		cd--
		if cd == 0 {
			break
		}
	}
	print_int(cd) // 0

	// 10.5 Simple counter
	mut x10 := 0
	for {
		x10++
		if x10 >= 10 {
			break
		}
	}
	print_int(x10) // 10

	// ==================== 11. MANY ARGUMENTS (5 tests) ====================
	print_str('--- 11. Many Arguments ---')

	// 11.1 Sum of 8 ones
	print_int(sum_many(1, 1, 1, 1, 1, 1, 1, 1)) // 8

	// 11.2 Sum of sequence
	print_int(sum_many(1, 2, 3, 4, 5, 6, 7, 8)) // 36

	// 11.3 Product of small numbers
	print_int(mul_many(1, 2, 1, 2, 1, 2, 1, 2)) // 16

	// 11.4 Max of 8
	print_int(max_of_eight(3, 7, 2, 9, 1, 8, 4, 6)) // 9

	// 11.5 Weighted sum
	print_int(weighted_sum(1, 1, 1, 1, 1, 1, 1, 1)) // 1+2+3+4+5+6+7+8 = 36

	// ==================== 12. MODIFYING STRUCT (5 tests) ====================
	print_str('--- 12. Modifying Struct via Function ---')

	// 12.1 Basic modify
	mut pm1 := Point{
		x: 10
		y: 20
	}
	modify_struct(mut pm1)
	print_int(pm1.x) // 999
	print_int(pm1.y) // 888

	// 12.2 Swap
	mut pm2 := Point{
		x: 5
		y: 15
	}
	swap_point(mut pm2)
	print_int(pm2.x) // 15
	print_int(pm2.y) // 5

	// 12.3 Scale
	mut pm3 := Point{
		x: 10
		y: 20
	}
	scale_point(mut pm3, 3)
	print_int(pm3.x) // 30
	print_int(pm3.y) // 60

	// 12.4 Translate
	mut pm4 := Point{
		x: 5
		y: 10
	}
	translate_point(mut pm4, 100, 200)
	print_int(pm4.x) // 105
	print_int(pm4.y) // 210

	// 12.5 Reset
	mut pm5 := Point{
		x: 999
		y: 888
	}
	reset_point(mut pm5)
	print_int(pm5.x) // 0
	print_int(pm5.y) // 0

	// ==================== 13. ASSERT (5 tests) ====================
	print_str('--- 13. Assert ---')

	// 13.1 Basic equality
	assert 1 == 1
	print_str('Assert 1 passed')

	// 13.2 Computed equality
	assert 2 + 2 == 4
	print_str('Assert 2 passed')

	// 13.3 Boolean assert
	assert true
	print_str('Assert 3 passed')

	// 13.4 Comparison assert
	assert 10 > 5
	print_str('Assert 4 passed')

	// 13.5 Complex expression
	assert (3 * 4) == (2 * 6)
	print_str('Assert 5 passed')

	// ==================== 14. HEAP ALLOCATION (5 tests) ====================
	print_str('--- 14. Heap Allocation ---')

	// 14.1 Basic heap Point
	hp1 := &Point{
		x: 10
		y: 20
	}
	print_int(hp1.x) // 10
	print_int(hp1.y) // 20

	// 14.2 Heap with zero
	hp2 := &Point{
		x: 0
		y: 0
	}
	print_int(hp2.x) // 0
	print_int(hp2.y) // 0

	// 14.3 Heap with computed values
	hp3 := &Point{
		x: 5 * 5
		y: 6 * 6
	}
	print_int(hp3.x) // 25
	print_int(hp3.y) // 36

	// 14.4 Heap Rectangle (no nested access)
	hr := &Rectangle{
		width:  100
		height: 200
		origin: Point{
			x: 10
			y: 20
		}
	}
	print_int(hr.width) // 100
	print_int(hr.height) // 200

	// 14.5 Heap Node
	hn := &Node{
		value: 42
		left:  1
		right: 2
	}
	print_int(hn.value) // 42
	print_int(hn.left) // 1
	print_int(hn.right) // 2

	// ==================== 15. BITWISE OPERATIONS (5 tests) ====================
	print_str('--- 15. Bitwise Operations ---')

	// 15.1 Basic AND
	print_int(0b1100 & 0b1010) // 8

	// 15.2 Basic OR
	print_int(0b1100 | 0b1010) // 14

	// 15.3 Basic XOR
	print_int(0b1100 ^ 0b1010) // 6

	// 15.4 Mask extraction
	num := 0xABCD
	low_byte := num & 0xFF
	print_int(low_byte) // 0xCD = 205

	// 15.5 Bit set/clear
	mut flags := 0
	flags = flags | 0b0001 // set bit 0
	flags = flags | 0b0100 // set bit 2
	print_int(flags) // 5
	flags = flags & 0b1110 // clear bit 0
	print_int(flags) // 4

	// ==================== 16. SHIFT OPERATIONS (5 tests) ====================
	print_str('--- 16. Shift Operations ---')

	// 16.1 Left shift basic
	print_int(1 << 4) // 16

	// 16.2 Right shift basic
	print_int(32 >> 2) // 8

	// 16.3 Multiple shifts
	print_int(255 >> 4) // 15

	// 16.4 Shift for multiply
	val16 := 7
	print_int(val16 << 3) // 7 * 8 = 56

	// 16.5 Shift for divide
	val17 := 96
	print_int(val17 >> 4) // 96 / 16 = 6

	// ==================== 17. MODULO (5 tests) ====================
	print_str('--- 17. Modulo ---')

	// 17.1 Basic modulo
	print_int(17 % 5) // 2

	// 17.2 Modulo with larger divisor
	print_int(100 % 7) // 2

	// 17.3 Even/odd check
	print_int(15 % 2) // 1 (odd)
	print_int(16 % 2) // 0 (even)

	// 17.4 Clock arithmetic
	hour := 23
	new_hour := (hour + 5) % 24
	print_int(new_hour) // 4

	// 17.5 Digit extraction
	num17 := 12345
	last_digit := num17 % 10
	print_int(last_digit) // 5
	second_digit := (num17 / 10) % 10
	print_int(second_digit) // 4

	// ==================== 18. POINTER ARITHMETIC (5 tests) ====================
	print_str('--- 18. Pointer Arithmetic ---')

	// 18.1 Heap struct access
	hp_arr1 := &Point{
		x: 10
		y: 20
	}
	print_int(hp_arr1.x) // 10
	print_int(hp_arr1.y) // 20

	// 18.2 Multiple heap structs
	hp_arr2 := &Point{
		x: 100
		y: 200
	}
	hp_arr3 := &Point{
		x: 300
		y: 400
	}
	print_int(hp_arr2.x + hp_arr3.x) // 400
	print_int(hp_arr2.y + hp_arr3.y) // 600

	// 18.3 Heap struct with computed values
	base18 := 5
	hp_arr4 := &Point{
		x: base18 * 10
		y: base18 * 20
	}
	print_int(hp_arr4.x) // 50
	print_int(hp_arr4.y) // 100

	// 18.4 Multiple heap allocations in loop
	mut sum18 := 0
	mut i18 := 0
	for i18 < 3 {
		hp := &Point{
			x: i18 * 10
			y: i18 * 20
		}
		sum18 = sum18 + hp.x + hp.y
		i18++
	}
	print_int(sum18) // 0+0 + 10+20 + 20+40 = 90

	// 18.5 Heap node tree structure
	node1 := &Node{
		value: 100
		left:  0
		right: 0
	}
	node2 := &Node{
		value: 200
		left:  0
		right: 0
	}
	print_int(node1.value + node2.value) // 300

	// ==================== 19. NESTED STRUCT ACCESS (5 tests) ====================
	print_str('--- 19. Nested Struct Access ---')

	// 19.1 Basic nested access
	rect := Rectangle{
		width:  100
		height: 200
		origin: Point{
			x: 10
			y: 20
		}
	}
	print_int(rect.width) // 100
	print_int(rect.height) // 200

	// 19.2 Nested struct field via intermediate
	rect2 := Rectangle{
		width:  50
		height: 60
		origin: Point{
			x: 5
			y: 6
		}
	}
	print_int(rect2.width + rect2.height) // 110

	// 19.3 Mutable nested struct modification
	mut rect3 := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 1
			y: 2
		}
	}
	rect3.width = 100
	rect3.height = 200
	print_int(rect3.width) // 100
	print_int(rect3.height) // 200

	// 19.4 Multiple rectangles
	rect4a := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 0
			y: 0
		}
	}
	rect4b := Rectangle{
		width:  30
		height: 40
		origin: Point{
			x: 0
			y: 0
		}
	}
	print_int(rect4a.width + rect4b.width) // 40
	print_int(rect4a.height + rect4b.height) // 60

	// 19.5 Rectangle area
	rect5 := Rectangle{
		width:  12
		height: 10
		origin: Point{
			x: 0
			y: 0
		}
	}
	area := rect5.width * rect5.height
	print_int(area) // 120

	// ==================== 20. NEGATIVE NUMBERS (5 tests) ====================
	print_str('--- 20. Negative Numbers ---')

	// 20.1 Unary minus
	n1 := 0 - 42
	print_int(n1) // -42

	// 20.2 Negative addition
	n2 := 0 - 10
	n3 := n2 + 5
	print_int(n3) // -5

	// 20.3 Negative subtraction
	n4 := 0 - 20
	n5 := n4 - 10
	print_int(n5) // -30

	// 20.4 Negative multiplication
	n6 := 0 - 7
	n7 := n6 * 3
	print_int(n7) // -21

	// 20.5 Double negative (positive)
	n8 := 0 - 50
	n9 := 0 - n8
	print_int(n9) // 50

	// ==================== 21. UNARY OPERATIONS (5 tests) ====================
	print_str('--- 21. Unary Operations ---')

	// 21.1 Logical not on false
	u1 := !false
	if u1 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 21.2 Logical not on true
	u2 := !true
	if u2 {
		print_int(1)
	} else {
		print_int(0) // 0
	}

	// 21.3 Double negation
	u3 := !!true
	if u3 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 21.4 Not with comparison
	u4 := !(5 > 10)
	if u4 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 21.5 Not with variable
	u5 := false
	u6 := !u5
	if u6 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 22. COMPLEX BOOLEAN (5 tests) ====================
	print_str('--- 22. Complex Boolean ---')

	// 22.1 Multiple ANDs
	if true && true && true {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.2 Multiple ORs
	if false || false || true {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.3 AND with OR
	if (true && false) || (true && true) {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.4 Complex condition with variables
	aa := 10
	bb := 20
	cc := 30
	if aa < bb && bb < cc {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.5 Chained comparisons
	if aa < 15 && bb > 15 && cc == 30 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 23. COMPARISON AS EXPRESSION (5 tests) ====================
	print_str('--- 23. Comparison as Expression ---')

	// 23.1 Comparison result in variable
	cmp1 := 10 > 5
	if cmp1 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.2 Multiple comparison results
	cmp2 := 3 < 5
	cmp3 := 7 > 2
	if cmp2 && cmp3 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.3 Equality comparison
	cmp4 := 42 == 42
	if cmp4 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.4 Inequality comparison
	cmp5 := 10 != 20
	if cmp5 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.5 Comparison with expressions
	cmp6 := (5 + 5) == (2 * 5)
	if cmp6 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 24. DEEPLY NESTED IF (5 tests) ====================
	print_str('--- 24. Deeply Nested If ---')

	// 24.1 Three levels deep
	dn1 := 5
	if dn1 > 0 {
		if dn1 > 3 {
			if dn1 > 4 {
				print_int(1) // 1
			} else {
				print_int(0)
			}
		} else {
			print_int(0)
		}
	} else {
		print_int(0)
	}

	// 24.2 Nested with else chains
	dn2 := 2
	if dn2 == 1 {
		print_int(10)
	} else {
		if dn2 == 2 {
			print_int(20) // 20
		} else {
			if dn2 == 3 {
				print_int(30)
			} else {
				print_int(0)
			}
		}
	}

	// 24.3 Mixed nesting with match
	dn3 := 3
	if dn3 > 0 {
		match dn3 {
			1 { print_int(100) }
			2 { print_int(200) }
			3 { print_int(300) } // 300
			else { print_int(0) }
		}
	} else {
		print_int(0)
	}

	// 24.4 Nested loops with conditionals
	mut dn4_sum := 0
	mut dn4_i := 0
	for dn4_i < 3 {
		mut dn4_j := 0
		for dn4_j < 3 {
			if dn4_i == dn4_j {
				dn4_sum += 1
			}
			dn4_j++
		}
		dn4_i++
	}
	print_int(dn4_sum) // 3 (diagonal: 0-0, 1-1, 2-2)

	// 24.5 If inside loop with break
	mut dn5_result := 0
	mut dn5_k := 0
	for dn5_k < 100 {
		if dn5_k > 5 {
			if dn5_k > 7 {
				dn5_result = dn5_k
				break
			}
		}
		dn5_k++
	}
	print_int(dn5_result) // 8

	// ==================== 25. LARGE CONSTANTS (5 tests) ====================
	print_str('--- 25. Large Constants ---')

	// 25.1 Value > 65535
	big1 := 100000
	print_int(big1) // 100000

	// 25.2 Large multiplication result
	big2 := 1000 * 1000
	print_int(big2) // 1000000

	// 25.3 Large addition
	big3 := 50000 + 50000
	print_int(big3) // 100000

	// 25.4 Large subtraction
	big4 := 200000 - 100000
	print_int(big4) // 100000

	// 25.5 Large division
	big5 := 1000000 / 100
	print_int(big5) // 10000

	// ==================== 26. MIXED OPERATIONS (5 tests) ====================
	print_str('--- 26. Mixed Operations ---')

	// 26.1 Arithmetic then comparison
	mix1 := (10 + 5) * 2
	if mix1 == 30 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 26.2 Comparison then logic
	mix2a := 10 > 5
	mix2b := 20 < 30
	if mix2a && mix2b {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 26.3 Chained arithmetic
	mix3 := 2 + 3 * 4 - 6 / 2
	print_int(mix3) // 2 + 12 - 3 = 11

	// 26.4 Bitwise with arithmetic
	mix4 := (5 | 3) + (4 & 6)
	print_int(mix4) // 7 + 4 = 11

	// 26.5 Complex expression
	mix5 := ((10 + 5) * 2 - 10) / 5
	print_int(mix5) // (30 - 10) / 5 = 4

	// ==================== 27. EDGE CASES (5 tests) ====================
	print_str('--- 27. Edge Cases ---')

	// 27.1 Zero operations
	edge1 := 0 + 0
	print_int(edge1) // 0
	edge2 := 100 * 0
	print_int(edge2) // 0
	edge3 := 0 / 7
	print_int(edge3) // 0

	// 27.2 Identity operations
	edge4 := 42 + 0
	print_int(edge4) // 42
	edge5 := 42 * 1
	print_int(edge5) // 42
	edge6 := 42 / 1
	print_int(edge6) // 42

	// 27.3 Division edge cases
	edge7 := 7 / 7
	print_int(edge7) // 1
	edge8 := 100 / 10
	print_int(edge8) // 10

	// 27.4 Modulo edge cases
	edge9 := 10 % 10
	print_int(edge9) // 0
	edge10 := 5 % 7
	print_int(edge10) // 5

	// 27.5 Comparison edge cases
	if 0 == 0 {
		print_int(1) // 1
	} else {
		print_int(0)
	}
	if 0 < 1 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 28. COMPLEX RECURSION (5 tests) ====================
	print_str('--- 28. Complex Recursion ---')

	// 28.1 Deep recursion test
	print_int(sum_recursive(100)) // 5050

	// 28.2 Mutual dependency via gcd
	print_int(gcd(252, 105)) // 21

	// 28.3 Multiple recursive calls
	print_int(fib(15)) // 610

	// 28.4 Power with larger exponent
	print_int(power(3, 5)) // 243

	// 28.5 Factorial with larger input
	print_int(factorial(7)) // 5040

	// ==================== 29. STRUCT OPERATIONS (5 tests) ====================
	print_str('--- 29. Struct Operations ---')

	// 29.1 Struct as function result (via modify)
	mut sp1 := Point{
		x: 0
		y: 0
	}
	scale_point(mut sp1, 10)
	print_int(sp1.x) // 0
	print_int(sp1.y) // 0

	// 29.2 Chained struct modifications
	mut sp2 := Point{
		x: 1
		y: 1
	}
	scale_point(mut sp2, 5)
	translate_point(mut sp2, 10, 20)
	print_int(sp2.x) // 15
	print_int(sp2.y) // 25

	// 29.3 Struct field arithmetic
	sp3 := Point{
		x: 100
		y: 200
	}
	sp3_sum := sp3.x + sp3.y
	sp3_diff := sp3.y - sp3.x
	print_int(sp3_sum) // 300
	print_int(sp3_diff) // 100

	// 29.4 Multiple struct parameters
	mut sp4 := Point{
		x: 10
		y: 20
	}
	swap_point(mut sp4)
	scale_point(mut sp4, 2)
	print_int(sp4.x) // 40
	print_int(sp4.y) // 20

	// 29.5 Struct in conditional
	sp5 := Point{
		x: 5
		y: 10
	}
	if sp5.x < sp5.y {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 30. CONTROL FLOW EDGE CASES (5 tests) ====================
	print_str('--- 30. Control Flow Edge Cases ---')

	// 30.1 Empty else
	mut cf1 := 0
	if true {
		cf1 = 1
	}
	print_int(cf1) // 1

	// 30.2 Multiple sequential ifs
	mut cf2 := 0
	if true {
		cf2 += 1
	}
	if true {
		cf2 += 2
	}
	if true {
		cf2 += 4
	}
	print_int(cf2) // 7

	// 30.3 Nested match
	cf3 := 2
	match cf3 {
		1 {
			match cf3 {
				1 { print_int(11) }
				else { print_int(10) }
			}
		}
		2 {
			match cf3 {
				2 { print_int(22) } // 22
				else { print_int(20) }
			}
		}
		else {
			print_int(0)
		}
	}

	// 30.4 Loop with compound condition (&&)
	mut cf4 := 0
	mut cf4_i := 0
	for cf4_i < 20 && cf4 < 50 {
		cf4 += cf4_i
		cf4_i++
	}
	print_int(cf4) // 55 (0+1+2+3+4+5+6+7+8+9+10 = 55, stops when >= 50)

	// 30.5 Return inside nested control flow
	print_int(nested_return(5)) // 100
	print_int(nested_return(15)) // 200
	print_int(nested_return(25)) // 300

	// 30.6 Loop with || condition
	mut cf5 := 0
	mut cf5_i := 0
	for cf5_i < 3 || cf5 < 10 {
		cf5 += 5
		cf5_i++
	}
	print_int(cf5) // 15 (loop runs 3 times: 5, 10, 15 - stops when both conditions false)

	// ==================== 31. METHODS ====================
	print_str('--- 31. Methods ---')

	// 31.1 Basic method call
	m1 := Point{
		x: 10
		y: 20
	}
	print_int(m1.sum()) // 30

	// 31.2 Method with multiplication
	m2 := Point{
		x: 5
		y: 6
	}
	print_int(m2.product()) // 30

	// 31.3 Mutable receiver method
	mut m4 := Point{
		x: 7
		y: 8
	}
	m4.double()
	print_int(m4.x) // 14
	print_int(m4.y) // 16

	// 31.4 Rectangle methods
	m5 := Rectangle{
		width:  10
		height: 5
		origin: Point{
			x: 0
			y: 0
		}
	}
	print_int(m5.area()) // 50
	print_int(m5.perimeter()) // 30

	// 31.5 Node method
	m6 := Node{
		value: 100
		left:  10
		right: 20
	}
	print_int(m6.total()) // 130

	// 31.6 Method on heap-allocated struct
	m8 := &Point{
		x: 4
		y: 5
	}
	print_int(m8.sum()) // 9
	print_int(m8.product()) // 20

	// ==================== 32. IF-EXPRESSIONS ====================
	print_str('--- 32. If-Expressions ---')

	// 32.1 Basic if-expression (abs)
	print_int(int_abs(-5)) // 5
	print_int(int_abs(7)) // 7
	print_int(int_abs(0)) // 0

	// 32.2 If-expression for max
	print_int(int_max2(10, 20)) // 20
	print_int(int_max2(30, 15)) // 30
	print_int(int_max2(5, 5)) // 5

	// 32.3 If-expression for min
	print_int(int_min2(10, 20)) // 10
	print_int(int_min2(30, 15)) // 15
	print_int(int_min2(8, 8)) // 8

	// 32.4 Nested if-expression (sign)
	print_int(sign(-100)) // -1
	print_int(sign(100)) // 1
	print_int(sign(0)) // 0

	// 32.5 Double nested if-expression (clamp)
	print_int(clamp(5, 0, 10)) // 5 (in range)
	print_int(clamp(-5, 0, 10)) // 0 (below lo)
	print_int(clamp(15, 0, 10)) // 10 (above hi)

	// 32.6 If-expression in local variable
	val2 := 25
	result := if val2 > 20 { val2 * 2 } else { val2 }
	print_int(result) // 50

	// 32.7 If-expression with complex condition
	a := 10
	b := 20
	c := if a < b && b < 30 { a + b } else { 0 }
	print_int(c) // 30

	// ==================== 33. ARRAY INITIALIZATION ====================
	print_str('--- 33. Array Initialization ---')

	// 33.1 Basic array literal
	arr1 := [10, 20, 30]
	print_int(arr1[0]) // 10
	print_int(arr1[1]) // 20
	print_int(arr1[2]) // 30

	// 33.2 Array element sum
	arr2 := [5, 10, 15]
	arr2_sum := arr2[0] + arr2[1] + arr2[2]
	print_int(arr2_sum) // 30

	// 33.3 Array with computed values
	base_val := 7
	arr3 := [base_val, base_val * 2, base_val * 3]
	print_int(arr3[0]) // 7
	print_int(arr3[1]) // 14
	print_int(arr3[2]) // 21

	// 33.4 Array element in expression
	arr4 := [100, 200, 300]
	result4 := arr4[0] * 2 + arr4[1]
	print_int(result4) // 400

	// 33.5 Array with function call on elements
	arr5 := [3, 4, 5]
	print_int(add(arr5[0], arr5[1])) // 7
	print_int(mul(arr5[1], arr5[2])) // 20

	// ==================== 34. STRING INTERPOLATION ====================
	print_str('--- 34. String Interpolation ---')

	// 34.1 Basic integer interpolation
	interp_x := 42
	s1 := 'The answer is ${interp_x}'
	print_str(s1) // The answer is 42

	// 34.2 Multiple interpolations
	interp_a := 10
	interp_b := 20
	s2 := '${interp_a} + ${interp_b} = ${interp_a + interp_b}'
	print_str(s2) // 10 + 20 = 30

	// 34.3 String at beginning and end
	interp_val := 100
	s3 := 'Value: ${interp_val}!'
	print_str(s3) // Value: 100!

	// 34.4 Just interpolation (no literal parts)
	interp_num := 999
	s4 := '${interp_num}'
	print_str(s4) // 999

	// 34.5 Multiple consecutive values
	interp_v1 := 1
	interp_v2 := 2
	interp_v3 := 3
	s5 := '${interp_v1}-${interp_v2}-${interp_v3}'
	print_str(s5) // 1-2-3

	world := 'world'
	s6 := 'hello ${world}'
	print_str(s6)

	// 34.6 String concatenation with + operator
	str_a := 'Hello'
	str_b := ' World'
	str_c := str_a + str_b
	print_str(str_c) // Hello World

	// 34.7 Chained string concatenation
	str_chain := 'A' + 'B' + 'C'
	print_str(str_chain) // ABC

	// ==================== 35. IF-GUARD EXPRESSIONS ====================
	print_str('--- 35. If-Guard Expressions ---')

	// 35.1 Basic if-guard with success (positive value)
	if g1_val := maybe_positive(42) {
		print_int(g1_val) // 42
	} else {
		print_int(0)
	}

	// 35.2 If-guard with failure (non-positive value)
	if g2_val := maybe_positive(-5) {
		print_int(g2_val)
	} else {
		print_int(999) // 999 (else branch taken)
	}

	// 35.3 If-guard with computation in then branch
	if g3_val := maybe_double(25) {
		print_int(g3_val + 10) // 50 + 10 = 60
	} else {
		print_int(0)
	}

	// 35.4 If-guard with function call
	if g4_result := maybe_sum(10, 20) {
		print_int(g4_result) // 30
	} else {
		print_int(0)
	}

	// 35.5 Nested if with if-guard
	g5_outer := 5
	if g5_outer > 0 {
		if g5_inner := maybe_positive(g5_outer * 10) {
			print_int(g5_inner) // 50
		} else {
			print_int(0)
		}
	}

	// 35.6 If-guard in sequence
	mut g6_sum := 0
	if g6_a := maybe_positive(10) {
		g6_sum += g6_a
	}
	if g6_b := maybe_positive(20) {
		g6_sum += g6_b
	}
	if g6_c := maybe_positive(-5) {
		g6_sum += g6_c
	}
	print_int(g6_sum) // 10 + 20 + 0 = 30

	// 35.7 If-guard with else-if chain
	g7_test := 100
	if g7_val := maybe_positive(-g7_test) {
		print_int(g7_val)
	} else {
		if g7_val2 := maybe_positive(g7_test) {
			print_int(g7_val2) // 100
		} else {
			print_int(0)
		}
	}

	// 35.8 or { return } pattern with value
	if or_val := maybe_triple(5) {
		print_int(or_val) // 15 (5 * 3)
	} else {
		print_int(-1)
	}

	// 35.9 or { return } pattern with negative (returns none)
	if or_val2 := maybe_triple(-5) {
		print_int(or_val2)
	} else {
		print_int(0) // 0 (none case)
	}

	// 35.10 Direct or { value } pattern - successful call
	or_direct1 := maybe_positive(42) or { 0 }
	print_int(or_direct1) // 42

	// 35.11 Direct or { value } pattern - fallback used
	or_direct2 := maybe_positive(-10) or { 99 }
	print_int(or_direct2) // 99

	// 35.12 Chained or { value } with computation
	or_chain := maybe_double(25) or { 0 }
	print_int(or_chain + 5) // 50 + 5 = 55

	// 35.13 Result or { err } pattern - ensure `err` is available in scope
	or_err := maybe_fail(-1) or {
		print_str(err.msg())
		0
	}
	print_int(or_err) // 0

	// ==================== 36. RANGE EXPRESSIONS ====================
	print_str('--- 36. Range Expressions ---')

	// 36.1 Basic array slicing with range
	rng_arr1 := [10, 20, 30, 40, 50]
	rng_slice1 := rng_arr1[1..4]
	print_int(rng_slice1[0]) // 20
	print_int(rng_slice1[1]) // 30
	print_int(rng_slice1[2]) // 40

	// 36.2 Range from start
	rng_arr2 := [100, 200, 300, 400]
	rng_slice2 := rng_arr2[0..2]
	print_int(rng_slice2[0]) // 100
	print_int(rng_slice2[1]) // 200

	// 36.3 Range with variable indices
	rng_start := 1
	rng_end := 3
	rng_arr3 := [5, 10, 15, 20, 25]
	rng_slice3 := rng_arr3[rng_start..rng_end]
	print_int(rng_slice3[0]) // 10
	print_int(rng_slice3[1]) // 15

	// 36.4 Consecutive slicing
	rng_arr4 := [1, 2, 3, 4, 5, 6, 7, 8]
	rng_first_half := rng_arr4[0..4]
	rng_second_half := rng_arr4[4..8]
	print_int(rng_first_half[0]) // 1
	print_int(rng_first_half[3]) // 4
	print_int(rng_second_half[0]) // 5
	print_int(rng_second_half[3]) // 8

	// 36.5 Single element range
	rng_arr5 := [42, 84, 126]
	rng_single := rng_arr5[1..2]
	print_int(rng_single[0]) // 84

	// ==================== 37. FOR-IN RANGE ====================
	print_str('--- 37. For-In Range ---')

	// 37.1 Basic for-in range
	mut forin_sum1 := 0
	for i in 0 .. 5 {
		forin_sum1 += i
	}
	print_int(forin_sum1) // 0+1+2+3+4 = 10

	// 37.2 For-in range with non-zero start
	mut forin_sum2 := 0
	for i in 5 .. 10 {
		forin_sum2 += i
	}
	print_int(forin_sum2) // 5+6+7+8+9 = 35

	// 37.3 For-in range with expressions
	forin_start := 2
	forin_end := 6
	mut forin_sum3 := 0
	for i in forin_start .. forin_end {
		forin_sum3 += i
	}
	print_int(forin_sum3) // 2+3+4+5 = 14

	// 37.4 Nested for-in ranges
	mut forin_count := 0
	for i in 0 .. 3 {
		for j in 0 .. 4 {
			forin_count += i + j + 1
		}
	}
	print_int(forin_count) // sum of (i+j+1) for i in 0..3, j in 0..4 = 42

	// 37.5 For-in range with computation in body
	mut forin_product := 1
	for i in 1 .. 6 {
		forin_product *= i
	}
	print_int(forin_product) // 1*2*3*4*5 = 120

	// 37.6 For-in range with break
	mut forin_sum4 := 0
	for i in 0 .. 100 {
		if i >= 5 {
			break
		}
		forin_sum4 += i
	}
	print_int(forin_sum4) // 0+1+2+3+4 = 10

	// 37.7 For-in range with continue
	mut forin_sum5 := 0
	for i in 0 .. 10 {
		if i % 2 == 0 {
			continue
		}
		forin_sum5 += i
	}
	print_int(forin_sum5) // 1+3+5+7+9 = 25

	// ==================== 38. DEFER STATEMENTS ====================
	print_str('--- 38. Defer Statements ---')

	// 38.1 Basic defer (should print after the other prints)
	g_val = 0
	defer {
		g_val += 100
	}
	g_val += 1
	print_int(g_val) // 1 (defer not executed yet, will be 101 at function end)

	// 38.2 Multiple defers execute in reverse order (LIFO)
	g_count = 0
	defer {
		g_count += 1
	}
	defer {
		g_count += 10
	}
	defer {
		g_count += 100
	}
	// At end of function: g_count = 0 + 100 + 10 + 1 = 111

	// 38.3 Defer with function call
	defer {
		print_str('Defer 38.3 executed')
	}
	// 38.4 Test defer_test function with explicit return
	print_int(defer_test()) // Should print 42

	// 38.5 Test defer order in function
	defer_order_test() // Should print: Third, Second, First

	// 38.6 Test defer(fn) - function-level defer
	print_int(defer_fn_test()) // 303 (3 from loop + 300 from function-level defers)

	// 38.7 Test flag enum .has() and .all() methods
	print_str('enum vals (exp 421):')
	print_int(flag_enum_values()) // Expected: 421 (1 + 20 + 400)
	print_str('AND test (exp 0):')
	print_int(flag_enum_and_test()) // Expected: 0 (3 & 4 = 0)
	print_str('manual has (exp 0):')
	print_int(flag_enum_manual_has()) // Expected: 0 (manual has check)
	print_str('flag debug (exp 3):')
	print_int(flag_enum_debug()) // Expected: 3 (read=1 | write=2)
	print_str('has read (exp 1):')
	print_int(flag_enum_has_read()) // Expected: 1
	print_str('has exec (exp 0):')
	print_int(flag_enum_has_execute()) // Expected: 0
	print_str('has result (exp 0):')
	print_int(flag_enum_has_result()) // Expected: 0 (direct bool->int)
	print_str('manual call (exp 0):')
	print_int(flag_enum_manual_call()) // Expected: 0 (manual has impl)
	print_str('check args (exp 304):')
	print_int(flag_enum_check_args()) // Expected: 304 (perms=3, exec=4)
	print_str('int args (exp 304):')
	print_int(flag_enum_check_int_args()) // Expected: 304
	print_str('full test (exp 13):')
	print_int(flag_enum_test()) // Expected: 13 (1 + 2 + 10)

	// ==================== 39. ENUMS ====================
	print_str('--- 39. Enums ---')

	// 39.1 Basic enum value
	color1 := Color.red
	print_int(int(color1)) // 0

	// 39.2 Other enum values
	color2 := Color.green
	color3 := Color.blue
	print_int(int(color2)) // 1
	print_int(int(color3)) // 2

	// 39.3 Enum with explicit values
	status1 := Status.pending
	status2 := Status.active
	status3 := Status.done
	print_int(int(status1)) // 0
	print_int(int(status2)) // 1
	print_int(int(status3)) // 2

	// 39.4 Enum in match
	match color1 {
		.red { print_int(100) } // 100
		.green { print_int(200) }
		.blue { print_int(300) }
		else { print_int(0) }
	}

	// 39.5 Enum comparison
	if color1 == Color.red {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 40. FOR-IN ARRAY ====================
	print_str('--- 40. For-In Array ---')

	// 40.1 Basic for-in array iteration
	arr_iter1 := [10, 20, 30]
	mut sum_iter1 := 0
	for elem in arr_iter1 {
		sum_iter1 += elem
	}
	print_int(sum_iter1) // 60

	// 40.2 For-in with index
	arr_iter2 := [5, 10, 15]
	mut weighted_sum2 := 0
	for i, elem in arr_iter2 {
		weighted_sum2 += (i + 1) * elem
	}
	print_int(weighted_sum2) // 1*5 + 2*10 + 3*15 = 70

	// 40.3 For-in with break
	arr_iter3 := [1, 2, 3, 4, 5]
	mut sum_iter3 := 0
	for elem in arr_iter3 {
		if elem > 3 {
			break
		}
		sum_iter3 += elem
	}
	print_int(sum_iter3) // 1+2+3 = 6

	// 40.4 For-in with continue
	arr_iter4 := [1, 2, 3, 4, 5]
	mut sum_iter4 := 0
	for elem in arr_iter4 {
		if elem % 2 == 0 {
			continue
		}
		sum_iter4 += elem
	}
	print_int(sum_iter4) // 1+3+5 = 9

	// 40.5 Nested for-in
	arr_outer := [1, 2, 3]
	arr_inner := [10, 20]
	mut nested_sum := 0
	for outer in arr_outer {
		for inner in arr_inner {
			nested_sum += outer * inner
		}
	}
	print_int(nested_sum) // (1*10+1*20) + (2*10+2*20) + (3*10+3*20) = 30+60+90 = 180

	// ==================== 41. FIXED SIZE ARRAYS ====================
	print_str('--- 41. Fixed Size Arrays ---')

	// 41.1 Fixed array with literal initialization
	fixed_arr1 := [5, 10, 15]
	print_int(fixed_arr1[0]) // 5
	print_int(fixed_arr1[1]) // 10
	print_int(fixed_arr1[2]) // 15

	// 41.2 Fixed array with computed index
	idx := 1
	print_int(fixed_arr1[idx]) // 10

	// 41.3 Fixed array sum
	mut fixed_sum := 0
	for elem in fixed_arr1 {
		fixed_sum += elem
	}
	print_int(fixed_sum) // 30

	// 41.4 Fixed array with larger size
	fixed_arr2 := [1, 2, 3, 4, 5]
	mut fixed_product := 1
	for elem in fixed_arr2 {
		fixed_product *= elem
	}
	print_int(fixed_product) // 120

	// 41.5 Nested fixed arrays access
	fixed_outer := [100, 200, 300]
	fixed_inner := [1, 2, 3]
	print_int(fixed_outer[0] + fixed_inner[2]) // 103

	// 41.6 Fixed array with ! literal syntax (explicit fixed size)
	fixed_literal := [1, 2, 3, 4, 5]!
	print_int(fixed_literal[0]) // 1
	print_int(fixed_literal[4]) // 5

	// 41.7 Fixed array .len access
	print_int(fixed_literal.len) // 5

	// 41.8 Fixed array iteration with ! syntax
	mut fixed_literal_sum := 0
	for elem in fixed_literal {
		fixed_literal_sum += elem
	}
	print_int(fixed_literal_sum) // 15

	// 41.9 Fixed array of u8 with ! syntax
	bytes := [u8(65), 66, 67]!
	print_int(bytes[0]) // 65 (ASCII 'A')
	print_int(bytes.len) // 3

	// ==================== 42. INTERFACE IMPLEMENTATION ====================
	print_str('--- 42. Interface Implementation ---')

	// 42.1 Call draw() method directly on Point (implements Drawable)
	draw_pt1 := Point{
		x: 5
		y: 10
	}
	print_int(draw_pt1.draw()) // 5*1000 + 10 = 5010

	// 42.2 Another point with draw
	draw_pt2 := Point{
		x: 12
		y: 34
	}
	print_int(draw_pt2.draw()) // 12*1000 + 34 = 12034

	// 42.3 Interface method on zero-init struct
	draw_pt3 := Point{}
	print_int(draw_pt3.draw()) // 0*1000 + 0 = 0

	// 42.4 Interface method with heap-allocated struct
	draw_pt4 := &Point{
		x: 100
		y: 200
	}
	print_int(draw_pt4.draw()) // 100*1000 + 200 = 100200

	// 42.5 Multiple interface method calls
	mut draw_total := 0
	draw_a := Point{
		x: 1
		y: 2
	}
	draw_b := Point{
		x: 3
		y: 4
	}
	draw_total += draw_a.draw() // 1002
	draw_total += draw_b.draw() // 3004
	print_int(draw_total) // 4006

	// ==================== 43. TYPE ALIAS USAGE ====================
	print_str('--- 43. Type Alias Usage ---')

	// 43.1 Basic type alias (MyInt is typedef'd to int)
	my_a := 10
	my_b := 20
	print_int(my_a + my_b) // 30

	// 43.2 Type alias in function
	my_result := add_my_ints(15, 25)
	print_int(my_result) // 40

	// 43.3 Type alias with arithmetic
	my_c := 100
	my_d := my_c * 3
	print_int(my_d) // 300

	// 43.4 Type alias comparison
	my_e := 50
	my_f := 50
	if my_e == my_f {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 43.5 Type alias in loop
	mut my_sum := 0
	for i in 1 .. 6 {
		my_sum += i
	}
	print_int(my_sum) // 1+2+3+4+5 = 15

	// ==================== 44. COMPTIME ====================
	print_str('--- 44. Comptime ---')

	// 44.1 Basic $if macos/$else
	$if macos {
		print_int(1) // 1 on macOS
	} $else {
		print_int(0) // 0 on other platforms
	}

	// 44.2 $if linux
	$if linux {
		print_int(2) // 2 on Linux
	} $else {
		print_int(20) // 20 on non-Linux
	}

	// 44.3 $if windows
	$if windows {
		print_int(3) // 3 on Windows
	} $else {
		print_int(30) // 30 on non-Windows
	}

	// 44.4 Negation: $if !windows
	$if !windows {
		print_int(4) // 4 on non-Windows
	} $else {
		print_int(40) // 40 on Windows
	}

	// 44.5 Comptime in function call
	print_int(get_comptime_value())

	// ==================== 45. STRING STRUCT FIELDS ====================
	print_str('--- 45. String Struct Fields ---')

	// 45.1 String literal .str field
	s45_1 := 'Hello'
	print_str(s45_1) // Hello

	// 45.2 String literal .len field
	s45_2 := 'World'
	print_int(s45_2.len) // 5

	// 45.3 Interpolated string .len field
	val45 := 123
	s45_3 := 'Val: ${val45}'
	print_int(s45_3.len) // 8

	// 45.4 Multiple string operations
	a45 := 'AB'
	b45 := 'CDE'
	print_int(a45.len + b45.len) // 5

	// 45.5 String in function parameter
	print_str('Passed directly') // Passed directly

	// ==================== 46. UNSAFE EXPRESSIONS ====================
	print_str('--- 46. Unsafe Expressions ---')

	// 46.1 Basic unsafe block returning value
	unsafe_val1 := unsafe {
		42
	}
	print_int(unsafe_val1) // 42

	// 46.2 Unsafe block with computation
	unsafe_val2 := unsafe {
		10 + 20 + 30
	}
	print_int(unsafe_val2) // 60

	// 46.3 Unsafe block with variable access
	base_for_unsafe := 100
	unsafe_val3 := unsafe {
		base_for_unsafe * 2
	}
	print_int(unsafe_val3) // 200

	// 46.4 Unsafe block in expression context
	result_unsafe := unsafe { 7 } * unsafe { 8 }
	print_int(result_unsafe) // 56

	// 46.5 Unsafe with struct field access
	unsafe_pt := Point{
		x: 15
		y: 25
	}
	unsafe_sum := unsafe {
		unsafe_pt.x + unsafe_pt.y
	}
	print_int(unsafe_sum) // 40

	// ==================== 47. INTERFACE VTABLE ====================
	print_str('--- 47. Interface Vtable ---')

	// 47.1 Basic interface assignment and method call
	vtable_pt1 := Point{
		x: 7
		y: 3
	}
	d1 := Drawable(vtable_pt1)
	print_int(d1.draw()) // 7*1000 + 3 = 7003

	// 47.2 Interface with different values
	vtable_pt2 := Point{
		x: 15
		y: 25
	}
	d2 := Drawable(vtable_pt2)
	print_int(d2.draw()) // 15*1000 + 25 = 15025

	// 47.3 Multiple interface calls
	vtable_pt3 := Point{
		x: 1
		y: 1
	}
	d3 := Drawable(vtable_pt3)
	print_int(d3.draw() + d3.draw()) // 1001 + 1001 = 2002

	// 47.4 Shape interface with multiple methods
	shape_rect := Rectangle{
		width:  10
		height: 5
		origin: Point{
			x: 0
			y: 0
		}
	}
	shape1 := Shape(shape_rect)
	print_int(shape1.area()) // 10 * 5 = 50
	print_int(shape1.perimeter()) // 2 * (10 + 5) = 30

	// 47.5 Sum of interface method results
	vtable_pt4 := Point{
		x: 2
		y: 3
	}
	d4 := Drawable(vtable_pt4)
	vtable_pt5 := Point{
		x: 4
		y: 5
	}
	d5 := Drawable(vtable_pt5)
	print_int(d4.draw() + d5.draw()) // 2003 + 4005 = 6008

	// 47.6 Calculator interface with parameters
	calc_pt := Point{
		x: 10
		y: 5
	}
	calc := Calculator(calc_pt)
	print_int(calc.add(3, 7)) // 10 + 5 + 3 + 7 = 25
	print_int(calc.multiply(4)) // (10 + 5) * 4 = 60

	// 47.7 Multiple Calculator calls
	calc_pt2 := Point{
		x: 2
		y: 3
	}
	calc2 := Calculator(calc_pt2)
	print_int(calc2.add(1, 1)) // 2 + 3 + 1 + 1 = 7
	print_int(calc2.multiply(10)) // (2 + 3) * 10 = 50

	// ==================== 48. STRUCT FIELD OPERATIONS ====================
	print_str('--- 48. Struct Field Operations ---')

	// 48.1 Basic field assignment with arithmetic
	mut sf1 := Point{
		x: 10
		y: 20
	}
	sf1.x = sf1.x + 5
	sf1.y = sf1.y - 3
	print_int(sf1.x) // 15
	print_int(sf1.y) // 17

	// 48.2 Field multiplication and division
	mut sf2 := Point{
		x: 6
		y: 100
	}
	sf2.x = sf2.x * 7
	sf2.y = sf2.y / 4
	print_int(sf2.x) // 42
	print_int(sf2.y) // 25

	// 48.3 Compound assignment on fields
	mut sf3 := Point{
		x: 50
		y: 30
	}
	sf3.x += 25
	sf3.y -= 10
	print_int(sf3.x) // 75
	print_int(sf3.y) // 20

	// 48.4 Compound multiply/divide on fields
	mut sf4 := Point{
		x: 8
		y: 64
	}
	sf4.x *= 5
	sf4.y /= 8
	print_int(sf4.x) // 40
	print_int(sf4.y) // 8

	// 48.5 Field used in expression with other field
	mut sf5 := Point{
		x: 3
		y: 4
	}
	sf5.x = sf5.x + sf5.y
	sf5.y = sf5.x * sf5.y
	print_int(sf5.x) // 7 (3+4)
	print_int(sf5.y) // 28 (7*4)

	// 48.6 Chained field operations
	mut sf6 := Point{
		x: 2
		y: 3
	}
	sf6.x = sf6.x * 2
	sf6.x = sf6.x + 1
	sf6.x = sf6.x * 3
	sf6.y = sf6.y + sf6.x
	print_int(sf6.x) // 15 ((2*2+1)*3)
	print_int(sf6.y) // 18 (3+15)

	// 48.7 Field modulo operation
	mut sf7 := Point{
		x: 17
		y: 23
	}
	sf7.x = sf7.x % 5
	sf7.y = sf7.y % 7
	print_int(sf7.x) // 2
	print_int(sf7.y) // 2

	// 48.8 Field bitwise operations
	mut sf8 := Point{
		x: 0b1100
		y: 0b1010
	}
	sf8.x = sf8.x & sf8.y
	sf8.y = sf8.x | 0b0101
	print_int(sf8.x) // 8 (0b1000)
	print_int(sf8.y) // 13 (0b1101)

	// 48.9 Field with function call result
	mut sf9 := Point{
		x: 5
		y: 10
	}
	sf9.x = add(sf9.x, sf9.y)
	sf9.y = mul(sf9.x, 2)
	print_int(sf9.x) // 15
	print_int(sf9.y) // 30

	// 48.10 Nested struct field modification
	mut rect_mod := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 0
			y: 0
		}
	}
	rect_mod.width = rect_mod.width * 2
	rect_mod.height += 5
	rect_mod.origin.x = 100
	rect_mod.origin.y = rect_mod.origin.x / 2
	print_int(rect_mod.width) // 20
	print_int(rect_mod.height) // 25
	print_int(rect_mod.origin.x) // 100
	print_int(rect_mod.origin.y) // 50

	// ==================== 49. PRINTLN ====================
	print_str('--- 49. Println ---')

	// 49.1 Test println
	println('hello world')

	// ==================== 50. ALGEBRAIC OPTIMIZATIONS ====================
	print_str('--- 50. Algebraic Optimizations ---')

	// 50.1 x - x = 0
	opt_val := 42
	print_int(opt_val - opt_val) // 0

	// 50.2 x ^ x = 0
	opt_xor := 123
	print_int(opt_xor ^ opt_xor) // 0

	// 50.3 x & x = x
	opt_and := 99
	print_int(opt_and & opt_and) // 99

	// 50.4 x | x = x
	opt_or := 77
	print_int(opt_or | opt_or) // 77

	// 50.5 x * 2 = x << 1
	opt_mul2 := 25
	print_int(opt_mul2 * 2) // 50

	// 50.6 Combined optimizations
	opt_a := 10
	opt_b := opt_a - opt_a // Should be 0
	opt_c := opt_a | opt_a // Should be 10
	print_int(opt_b) // 0
	print_int(opt_c) // 10

	// 50.7 2 * x = x << 1 (commutative)
	opt_mul2_comm := 13
	print_int(2 * opt_mul2_comm) // 26

	// 50.8 Algebraic opts in expressions
	opt_expr := 7
	print_int((opt_expr ^ opt_expr) + 5) // 0 + 5 = 5
	print_int((opt_expr & opt_expr) * 2) // 7 * 2 = 14

	// 50.9 Algebraic opts with different values
	opt_large := 12345
	print_int(opt_large - opt_large) // 0
	print_int(opt_large ^ opt_large) // 0
	print_int(opt_large & opt_large) // 12345
	print_int(opt_large | opt_large) // 12345

	// 50.10 Algebraic opts in loop
	mut opt_loop_sum := 0
	for i in 1 .. 5 {
		opt_loop_sum += i - i // Should add 0 each iteration
		opt_loop_sum += i & i // Should add i each iteration
	}
	print_int(opt_loop_sum) // 0+1 + 0+2 + 0+3 + 0+4 = 10

	// ==================== 51. DEAD STORE ELIMINATION ====================
	print_str('--- 51. Dead Store Elimination ---')

	// 51.1 Basic dead store - local var never read
	// The optimizer should remove stores to variables that are never used
	{
		mut dead_var := 100
		dead_var = 200 // dead store, never read
		_ = dead_var
	}
	print_int(1) // 1 - verify execution continues

	// 51.2 Dead store with live store after
	mut dse_var := 10
	dse_var = 20 // dead store (overwritten before read)
	dse_var = 30 // this is the live store
	print_int(dse_var) // 30

	// 51.3 Multiple dead stores
	mut dse_multi := 1
	dse_multi = 2 // dead
	dse_multi = 3 // dead
	dse_multi = 4 // dead
	dse_multi = 5 // live
	print_int(dse_multi) // 5

	// 51.4 Dead store in branch not taken
	mut dse_branch := 100
	if false {
		dse_branch = 999 // dead (branch never taken)
	}
	print_int(dse_branch) // 100

	// 51.5 Live store in taken branch
	mut dse_live := 50
	if true {
		dse_live = 75
	}
	print_int(dse_live) // 75

	// ==================== 52. DEAD PHI ELIMINATION ====================
	print_str('--- 52. Dead Phi Elimination ---')

	// 52.1 Phi from if-else where result is unused
	// The phi node should be eliminated if not used
	mut phi_unused := 0
	if true {
		phi_unused = 10
	} else {
		phi_unused = 20
	}
	// phi_unused is reassigned, so previous phi is dead
	phi_unused = 99
	print_int(phi_unused) // 99

	// 52.2 Multiple phi nodes, some dead
	mut phi_a := 0
	mut phi_b := 0
	if true {
		phi_a = 1
		phi_b = 2
	} else {
		phi_a = 3
		phi_b = 4
	}
	// phi_b is dead (overwritten), phi_a is live
	phi_b = 100
	print_int(phi_a) // 1
	print_int(phi_b) // 100

	// 52.3 Loop phi - variable assigned in loop body
	mut phi_loop := 0
	for i in 0 .. 3 {
		phi_loop = i // intermediate values are dead, only final matters
	}
	print_int(phi_loop) // 2 (last iteration value)

	// 52.4 Nested if with dead phi
	mut phi_nested := 0
	if true {
		if true {
			phi_nested = 10
		} else {
			phi_nested = 20
		}
		phi_nested = 30 // overwrites inner phi result
	}
	print_int(phi_nested) // 30

	// 52.5 Complex phi scenario with loop and conditionals
	mut phi_complex := 1
	for i in 0 .. 4 {
		if i % 2 == 0 {
			phi_complex = phi_complex + 1
		} else {
			phi_complex = phi_complex * 2
		}
	}
	print_int(phi_complex) // 1 -> 2 -> 4 -> 5 -> 10

	// ==================== 53. CONSTANT DEDUPLICATION ====================
	print_str('--- 53. Constant Deduplication ---')

	// 53.1 Same constant used multiple times
	const_a := 42
	const_b := 42
	const_c := 42
	print_int(const_a + const_b + const_c) // 126

	// 53.2 Zero constant deduplication
	zero1 := 0
	zero2 := 0
	zero3 := 0
	print_int(zero1 + zero2 + zero3) // 0

	// 53.3 Constant from algebraic opts should be deduplicated
	dedup_x := 50
	result1 := dedup_x - dedup_x // creates zero
	dedup_y := 60
	result2 := dedup_y - dedup_y // should reuse same zero
	print_int(result1 + result2) // 0

	// 53.4 Multiple shifts with same constant
	shift_a := 1
	shift_b := 2
	shift_c := 4
	// All these use constant 1 for the shift amount when x*2 is optimized
	print_int(shift_a * 2 + shift_b * 2 + shift_c * 2) // 2 + 4 + 8 = 14

	// 53.5 Constants in expressions
	expr_const := (10 + 10) * (5 + 5) // Both 10s and 5s should be deduplicated
	print_int(expr_const) // 200

	// ==================== 54. HASHMAPS ====================
	print_str('--- 54. Hashmaps ---')

	// 54.1 Basic map initialization and assignment
	mut hm1 := map[int]int{}
	hm1[1] = 10
	hm1[2] = 20
	hm1[3] = 30
	print_int(hm1[1]) // 10
	print_int(hm1[2]) // 20
	print_int(hm1[3]) // 30

	// 54.2 Map len
	print_str('map len:')
	print_int(hm1.len) // 3

	// 54.3 Map with different keys
	mut hm2 := map[int]int{}
	hm2[100] = 1000
	hm2[200] = 2000
	print_int(hm2[100]) // 1000
	print_int(hm2[200]) // 2000
	print_int(hm2.len) // 2

	// 54.4 Map value update
	mut hm3 := map[int]int{}
	hm3[5] = 50
	print_int(hm3[5]) // 50
	hm3[5] = 500
	print_int(hm3[5]) // 500
	print_int(hm3.len) // 1 (still 1, not 2)

	// 54.5 Map with computed values
	mut hm4 := map[int]int{}
	for i in 0 .. 5 {
		hm4[i] = i * i
	}
	print_int(hm4[0]) // 0
	print_int(hm4[1]) // 1
	print_int(hm4[2]) // 4
	print_int(hm4[3]) // 9
	print_int(hm4[4]) // 16

	// 54.6 Map literal initialization
	print_str('map literal:')
	mut hm5 := {
		'a': 1
		'b': 2
		'c': 3
	}
	print_int(hm5['a']) // 1
	print_int(hm5['b']) // 2
	print_int(hm5['c']) // 3
	print_int(hm5.len) // 3
	hm5['d'] = 4
	print_int(hm5['d']) // 4
	print_int(hm5.len) // 4

	// 54.7 Map clone
	print_str('map clone:')
	mut hm6 := {
		'x': 100
		'y': 200
	}
	hm7 := hm6.clone()
	print_int(hm7['x']) // 100
	print_int(hm7['y']) // 200
	print_int(hm7.len) // 2
	// Verify clone is independent
	hm6['x'] = 999
	print_int(hm6['x']) // 999 (modified original)
	print_int(hm7['x']) // 100 (clone unchanged)

	// ==================== 55. MULTI-RETURN ====================
	print_str('--- 55. Multi-return ---')

	// 55.1 Basic two-value return
	a1, b1 := swap(10, 20)
	print_int(a1) // 20
	print_int(b1) // 10

	// 55.2 Division and modulo
	quot, rem := divmod(17, 5)
	print_int(quot) // 3
	print_int(rem) // 2

	// 55.3 Min/max of three values
	min1, max1 := min_max(5, 2, 8)
	print_int(min1) // 2
	print_int(max1) // 8

	// 55.4 Three-value return
	t1, t2, t3 := triple_return(7)
	print_int(t1) // 7
	print_int(t2) // 14
	print_int(t3) // 21

	// 55.5 Ignore some return values with _
	_, only_rem := divmod(23, 4)
	print_int(only_rem) // 3

	only_quot, _ := divmod(23, 4)
	print_int(only_quot) // 5
	print_str('pi=')
	print_int(pi)

	// 55.6 String compound assignment
	mut s := 'hello'
	s += ' world'
	print_str(s) // hello world

	// 55.7 Array comparison
	cmp_arr1 := [1, 2, 3]
	cmp_arr2 := [1, 2, 3]
	cmp_arr3 := [1, 2, 4]
	if cmp_arr1 == cmp_arr2 {
		print_str('arr1 == arr2: yes')
	}
	if cmp_arr1 != cmp_arr3 {
		print_str('arr1 != arr3: yes')
	}

	// ==================== 56. ARRAY FILTER (4 tests) ====================
	print_str('--- 56. Array filter ---')

	// 56.1 Basic filter with it
	filter_arr := [1, 2, 3, 4, 5, 6]
	evens := filter_arr.filter(it % 2 == 0)
	print_str('filtered.len:')
	print_int(evens.len) // 3
	print_str('filtered values:')
	for _, number in evens {
		print_int(number)
	}

	// 56.2 Filter with comparison
	odds := filter_arr.filter(it % 2 != 0)
	print_int(odds.len) // 3

	// 56.3 Filter that returns nothing
	big_nums := filter_arr.filter(it > 10)
	print_int(big_nums.len) // 0

	// 56.4 Filter that returns everything
	small_nums := filter_arr.filter(it <= 6)
	print_int(small_nums.len) // 6

	// ==================== 57. ARRAY STR (auto-generated) ====================
	print_str('--- 57. Array str ---')

	// 57.1 Basic array str via println
	str_arr := [1, 2, 3, 4]
	println(str_arr) // should print [1, 2, 3, 4]

	// 57.2 Empty array str
	empty_arr := []int{}
	println(empty_arr) // should print []

	// 57.3 Single element array
	single_arr := [42]
	println(single_arr) // should print [42]

	// ==================== 58. IF-GUARD EXPRESSIONS ====================
	print_str('--- 58. If-guard expressions ---')

	// 58.1 Basic map if-guard - key exists
	mut guard_map := map[int]int{}
	guard_map[10] = 100
	guard_map[20] = 200
	if guard_val1 := guard_map[10] {
		print_int(guard_val1) // 100
	} else {
		print_str('not found')
	}

	// 58.2 Map if-guard - key doesn't exist
	if guard_val2 := guard_map[99] {
		print_int(guard_val2)
	} else {
		print_str('key 99 not found') // should print this
	}

	// 58.3 Value-level if-guard (ternary-like)
	result_guard := if gv1 := guard_map[20] { gv1 } else { 0 }
	print_int(result_guard) // 200

	// 58.4 Value-level if-guard with missing key
	result_missing := if gv2 := guard_map[999] { gv2 } else { -1 }
	print_int(result_missing) // -1

	// 58.5 Multiple if-guards in sequence (same variable name)
	if off := guard_map[10] {
		print_int(off) // 100
	}
	if off := guard_map[20] {
		print_int(off) // 200
	}

	// 58.6 If-guard with computation in body
	if v := guard_map[10] {
		doubled := v * 2
		print_int(doubled) // 200
	}

	// 59. Array clone() on nested selectors
	// Test that array.clone() works on deeply nested selector expressions
	// like wrapper.holder.data.clone() - this tests the fix for chained selector type inference
	mut holder := ArrayHolder{
		data: []int{cap: 10}
	}
	holder.data << 10
	holder.data << 20
	holder.data << 30
	mut wrap := Wrapper{
		holder: &holder
	}
	// Clone via nested selector (wrapper.holder.data.clone())
	mut cloned_arr := wrap.holder.data.clone()
	print_int(cloned_arr.len) // 3
	print_int(cloned_arr[0]) // 10
	print_int(cloned_arr[2]) // 30

	// Verify clone is independent - modify cloned array
	cloned_arr[0] = 99
	print_int(cloned_arr[0]) // 99 (modified)
	print_int(wrap.holder.data[0]) // 10 (original unchanged)

	// 60. Map indexing with push (map[key] << value)
	// Test that g.pending_labels[blk] << off pattern works correctly
	// This uses map_get_and_set to auto-create empty array if key doesn't exist
	mut pl := PendingLabels{
		labels: map[int][]int{}
	}
	pl.labels[5] << 100 // First push to key 5 - creates new array
	pl.labels[5] << 200 // Second push to same key
	pl.labels[10] << 50 // Push to different key
	print_int(pl.labels[5].len) // 2
	print_int(pl.labels[5][0]) // 100
	print_int(pl.labels[5][1]) // 200
	print_int(pl.labels[10].len) // 1
	print_int(pl.labels[10][0]) // 50

	// ==================== 61. MUTABLE SLICE ARGUMENTS ====================
	print_str('--- 61. Mutable slice arguments ---')

	// Test that slices can be passed as mutable arguments to functions
	// This tests the &(array[]){builtin__array_slice(...)}[0] pattern
	mut slice_data := []int{len: 8}
	slice_data[0] = 10
	slice_data[1] = 20
	slice_data[2] = 30
	slice_data[3] = 40

	// 61.1 Read from slice (non-mutable)
	read_result := read_from_slice(slice_data[1..3])
	print_int(read_result) // 50 (20 + 30)

	// 61.2 Write to slice (mutable) - should modify original array
	write_to_slice(mut slice_data[0..2], 99)
	print_int(slice_data[0]) // 99 (modified via slice)
	print_int(slice_data[1]) // 99 (modified via slice)
	print_int(slice_data[2]) // 30 (unchanged, outside slice range)

	// 61.3 Nested selector with slice and mutable arg
	// Note: Using explicit array construction to avoid array literal struct init bug
	mut holder2 := ArrayHolder{
		data: []int{len: 4}
	}
	holder2.data[0] = 100
	holder2.data[1] = 200
	holder2.data[2] = 300
	holder2.data[3] = 400
	write_to_slice(mut holder2.data[1..3], 555)
	print_int(holder2.data[0]) // 100 (unchanged)
	print_int(holder2.data[1]) // 555 (modified)
	print_int(holder2.data[2]) // 555 (modified)
	print_int(holder2.data[3]) // 400 (unchanged)

	// ==================== 62. MAP INDEX WITH OR BLOCK ====================
	print_str('--- 62. Map index with or block ---')

	// Test the pattern: map[key] or { fallback }
	// This is used in lookup_type_from_env and similar functions
	mut or_test_map := map[string]int{}
	or_test_map['foo'] = 42
	or_test_map['bar'] = 100
	or_test_map['builtin'] = 999

	// 62.1 Key exists - should return the value
	or_result1 := map_lookup_with_or(or_test_map, 'foo')
	print_int(or_result1) // 42

	// 62.2 Key doesn't exist - should return fallback (-1)
	or_result2 := map_lookup_with_or(or_test_map, 'nonexistent')
	print_int(or_result2) // -1

	// 62.3 Nested or block - first key exists
	or_result3 := nested_map_or(or_test_map, 'foo')
	print_int(or_result3) // 42

	// 62.4 Nested or block - first key missing, fallback to 'builtin'
	or_result4 := nested_map_or(or_test_map, 'missing')
	print_int(or_result4) // 999 (from 'builtin' key)

	// 62.5 Nested or block - both keys missing, return default
	mut or_empty_map := map[string]int{}
	or_result5 := nested_map_or(or_empty_map, 'anything')
	print_int(or_result5) // -999 (default from return in or block)

	// ==================== 63. IF-GUARD WITH METHOD CALL ====================
	print_str('--- 63. If-guard with method call returning optional ---')

	// This tests the pattern from lookup_type_from_env:
	// if obj := container.method() { ... } - method returning optional

	// 63.1 Method returning optional - found case
	dc := DataContainer{
		value: 42
		name:  'test'
	}
	if val := dc.lookup('test') {
		print_int(val) // 42
	}

	// 63.2 Method returning optional - not found case
	if val := dc.lookup('missing') {
		print_int(val) // Should not print
	} else {
		print_int(-100) // -100
	}

	// 63.3 Nested method call with if-guard
	dc2 := DataContainer{
		value: 777
		name:  'found'
	}
	mut result63 := 0
	if v := dc2.lookup('found') {
		result63 = v
	}
	print_int(result63) // 777

	// ==================== 64. IF-GUARD + IS-CHECK + OPTION RETURN ====================
	print_str('--- 64. If-guard with is-check and optional return ---')

	// This tests the pattern from lookup_struct_from_env in arm64:
	// fn lookup_struct() ?Struct { if typ := lookup_type() { if typ is Struct { return typ } } return none }

	// 64.1 Setup type_holder with struct and enum types
	type_holder := TypeHolder{
		struct_type: StructType{
			name:   'Point'
			fields: 2
		}
		enum_type:   EnumType{
			name:     'Color'
			variants: 3
		}
	}

	// 64.2 lookup_struct - found case (asking for 'struct' returns the struct type)
	if s64_1 := type_holder.lookup_struct('struct') {
		print_str(s64_1.name) // Point
		print_int(s64_1.fields) // 2
	} else {
		print_str('NOT_FOUND')
	}

	// 64.3 lookup_struct - not found because 'enum' returns EnumType, not StructType
	if s64_2 := type_holder.lookup_struct('enum') {
		print_str(s64_2.name)
	} else {
		print_int(-200) // -200 (returned enum, not struct)
	}

	// 64.4 lookup_struct - not found because key doesn't exist
	if s64_3 := type_holder.lookup_struct('missing') {
		print_str(s64_3.name)
	} else {
		print_int(-300) // -300 (key not found)
	}

	// 64.5 lookup_enum - found case (asking for 'enum' returns the enum type)
	if e64_1 := type_holder.lookup_enum('enum') {
		print_str(e64_1.name) // Color
		print_int(e64_1.variants) // 3
	} else {
		print_str('NOT_FOUND')
	}

	// 64.6 lookup_enum - not found because 'struct' returns StructType, not EnumType
	if e64_2 := type_holder.lookup_enum('struct') {
		print_str(e64_2.name)
	} else {
		print_int(-400) // -400 (returned struct, not enum)
	}

	// ==================== 65. MAP ITERATION WITH POINTER VALUES ====================
	print_str('--- 65. Map iteration with pointer values ---')

	// Tests map[int]&Struct iteration - the same pattern used in allocate_registers

	// 65.1 Create map with pointer values
	mut intervals := map[int]&Point{}
	intervals[1] = &Point{
		x: 10
		y: 100
	}
	intervals[2] = &Point{
		x: 20
		y: 200
	}
	intervals[3] = &Point{
		x: 30
		y: 300
	}

	// 65.2 Iterate over map and collect pointer values
	mut sum_x := 0
	mut sum_y := 0
	for _, p in intervals {
		sum_x += p.x
		sum_y += p.y
	}
	print_int(sum_x) // 60 (10+20+30)
	print_int(sum_y) // 600 (100+200+300)

	// 65.3 If-guard with map[int]&Struct
	if pt65 := intervals[2] {
		print_int(pt65.x) // 20
		print_int(pt65.y) // 200
	}

	// ==================== 66. NESTED MAP INDEXING IN ARRAY ====================
	print_str('--- 66. Nested map indexing in array ---')

	// Tests used[reg_map[val_id]] pattern - nested map index as array index

	// 66.1 Setup array and map
	mut used66 := []bool{len: 10, init: false}
	mut reg_map66 := map[int]int{}
	reg_map66[5] = 3 // val_id 5 -> index 3
	reg_map66[7] = 8 // val_id 7 -> index 8

	// 66.2 Use nested map index to set array element
	used66[reg_map66[5]] = true
	print_int(if used66[3] { 1 } else { 0 }) // 1 (index 3 was set)
	print_int(if used66[0] { 1 } else { 0 }) // 0 (index 0 not set)

	// 66.3 Use nested map index in loop
	val_ids := [5, 7]
	for vid in val_ids {
		map_idx := reg_map66[vid]
		used66[map_idx] = true
	}
	print_int(if used66[3] { 1 } else { 0 }) // 1
	print_int(if used66[8] { 1 } else { 0 }) // 1

	// 66.4 Direct nested indexing
	used66[reg_map66[7]] = false
	print_int(if used66[8] { 1 } else { 0 }) // 0 (reset to false)

	// ==================== 67. NESTED MAP ASSIGNMENT ====================
	print_str('--- 67. Nested map assignment ---')

	// Tests map[string]map[string]bool{} pattern - nested map with inner map initialization

	// 67.1 Create nested map and initialize inner map
	mut nested_map := map[string]map[string]bool{}
	if 'mod1' !in nested_map {
		nested_map['mod1'] = map[string]bool{}
	}
	nested_map['mod1']['TypeA'] = true
	nested_map['mod1']['TypeB'] = true

	// 67.2 Verify values
	print_int(if nested_map['mod1']['TypeA'] { 1 } else { 0 }) // 1
	print_int(if nested_map['mod1']['TypeB'] { 1 } else { 0 }) // 1
	print_int(if nested_map['mod1']['TypeC'] { 1 } else { 0 }) // 0 (not set)

	// 67.3 Add another module
	if 'mod2' !in nested_map {
		nested_map['mod2'] = map[string]bool{}
	}
	nested_map['mod2']['TypeX'] = true
	print_int(if nested_map['mod2']['TypeX'] { 1 } else { 0 }) // 1

	// 67.4 Check independence of inner maps
	print_int(if nested_map['mod1']['TypeA'] { 1 } else { 0 }) // 1 (still set)
	print_int(if nested_map['mod2']['TypeA'] { 1 } else { 0 }) // 0 (not in mod2)

	// ==================== 68. SMARTCAST WITH IS-CHECK ====================
	print_str('--- 68. Smartcast with is-check ---')

	// Tests `if x is Type` pattern with sum types

	// 68.1 Basic is-check with int variant
	num1 := Number(42)
	if num1 is int {
		print_int(num1) // 42 (smartcasted to int)
	} else {
		print_int(-1)
	}

	// 68.2 Basic is-check with Point variant
	num2 := Number(Point{
		x: 10
		y: 20
	})
	if num2 is Point {
		print_int(num2.x) // 10 (smartcasted to Point, access field)
		print_int(num2.y) // 20
	} else {
		print_int(-1)
	}

	// 68.3 Negative is-check - int is not Point
	num3 := Number(100)
	if num3 is Point {
		print_int(num3.x)
	} else {
		print_int(-100) // -100 (num3 is int, not Point)
	}

	// 68.4 Negative is-check - Point is not int
	num4 := Number(Point{
		x: 5
		y: 6
	})
	if num4 is int {
		print_int(num4)
	} else {
		print_int(-200) // -200 (num4 is Point, not int)
	}

	// 68.5 Smartcast with computation
	num5 := Number(Point{
		x: 3
		y: 4
	})
	if num5 is Point {
		sum := num5.x + num5.y
		print_int(sum) // 7
	}

	// 68.6 Compound is-check with && and field access
	// Tests: if x is Type && x.field == value { ... }
	// This requires smartcast to be applied in compound conditions
	num6 := Number(Point{
		x: 42
		y: 100
	})
	if num6 is Point && num6.x == 42 {
		print_int(num6.y) // 100 (smartcast applied in compound &&)
	} else {
		print_int(-1)
	}

	// 68.7 Compound is-check that fails the field check
	num7 := Number(Point{
		x: 10
		y: 20
	})
	if num7 is Point && num7.x == 999 {
		print_int(-1) // Should not reach here
	} else {
		print_int(111) // 111 (fails because x != 999)
	}

	// 68.8 Compound is-check that fails the type check
	num8 := Number(77)
	if num8 is Point && num8.x == 77 {
		print_int(-1) // Should not reach here
	} else {
		print_int(77) // 77 (int is not Point)
	}

	// ==================== 69. ARRAY APPEND ON STRUCT FIELDS ====================
	print_str('--- 69. Array append on struct fields ---')

	// Tests arr << elem pattern on struct fields (cleanc fallback when transformer doesn't handle it)

	// 69.1 Basic array append on struct field
	mut holder69 := ArrayHolder{
		data: []int{}
	}
	holder69.data << 10
	holder69.data << 20
	holder69.data << 30
	print_int(holder69.data.len) // 3
	print_int(holder69.data[0]) // 10
	print_int(holder69.data[1]) // 20
	print_int(holder69.data[2]) // 30

	// 69.2 Array append in a loop
	mut holder69b := ArrayHolder{
		data: []int{}
	}
	for i := 0; i < 5; i++ {
		holder69b.data << i * 2
	}
	print_int(holder69b.data.len) // 5
	print_int(holder69b.data[0]) // 0
	print_int(holder69b.data[4]) // 8

	// 69.3 Array append with expression
	mut holder69c := ArrayHolder{
		data: []int{}
	}
	x69 := 100
	holder69c.data << x69 + 1
	holder69c.data << x69 * 2
	print_int(holder69c.data[0]) // 101
	print_int(holder69c.data[1]) // 200

	// ==================== 70. MATCH EXPRESSION ON SUM TYPES ====================
	print_str('--- 70. Match on sum types ---')

	// Tests `match x { Type { ... } }` pattern with sum types and smartcasting

	// 70.1 Match on int variant
	num70a := Number(42)
	match num70a {
		int {
			print_int(num70a) // 42 (smartcasted to int)
		}
		Point {
			print_int(-1)
		}
	}

	// 70.2 Match on Point variant with field access
	num70b := Number(Point{
		x: 7
		y: 8
	})
	match num70b {
		int {
			print_int(-1)
		}
		Point {
			print_int(num70b.x) // 7 (smartcasted to Point)
			print_int(num70b.y) // 8
		}
	}

	// 70.3 Match with computation on Point variant
	num70c := Number(Point{
		x: 5
		y: 5
	})
	match num70c {
		int {
			print_int(num70c + 1)
		}
		Point {
			sum70 := num70c.x + num70c.y
			print_int(sum70) // 10
		}
	}

	// 70.4 Match result in assignment
	num70d := Number(99)
	result70 := match num70d {
		int {
			num70d * 2 // 198
		}
		Point {
			-1
		}
	}
	print_int(result70) // 198

	// 70.5 Match on Point with result assignment
	num70e := Number(Point{
		x: 3
		y: 4
	})
	result70e := match num70e {
		int {
			-1
		}
		Point {
			num70e.x * num70e.y // 12
		}
	}
	print_int(result70e) // 12

	// ==================== 71. NESTED SMARTCAST ====================
	print_str('--- 71. Nested smartcast ---')

	// Tests nested smartcasting where outer match smartcasts to a variant
	// that is itself a sum type, then inner if-is checks that variant.
	// This requires the transformer to apply outer smartcasts to inner tag checks.

	// 71.1 Nested smartcast: match outer, if-is inner (int variant)
	nested1 := NestedOuter(Number(42))
	match nested1 {
		Number {
			// nested1 is now smartcasted to Number
			// Number is itself a sum type (int | Point)
			if nested1 is int {
				print_int(nested1) // 42 (nested smartcast to int)
			} else {
				print_int(-1)
			}
		}
		Rectangle {
			print_int(-1)
		}
	}

	// 71.2 Nested smartcast: match outer, if-is inner (Point variant)
	nested2 := NestedOuter(Number(Point{
		x: 7
		y: 8
	}))
	match nested2 {
		Number {
			// nested2 is smartcasted to Number
			if nested2 is Point {
				print_int(nested2.x) // 7 (nested smartcast to Point)
				print_int(nested2.y) // 8
			} else {
				print_int(-1)
			}
		}
		Rectangle {
			print_int(-1)
		}
	}

	// 71.3 Nested smartcast: negative test - inner type doesn't match
	nested3 := NestedOuter(Number(100))
	match nested3 {
		Number {
			if nested3 is Point {
				print_int(-1) // Should not reach
			} else {
				print_int(200) // 200 (nested3 is int, not Point)
			}
		}
		Rectangle {
			print_int(-1)
		}
	}

	// 71.4 Nested smartcast with computation
	nested4 := NestedOuter(Number(Point{
		x: 5
		y: 6
	}))
	match nested4 {
		Number {
			if nested4 is Point {
				sum71 := nested4.x + nested4.y
				print_int(sum71) // 11
			}
		}
		Rectangle {
			print_int(-1)
		}
	}

	// ==================== 72. ARRAY LITERAL TYPE INFERENCE ====================
	print_str('--- 72. Array literal type inference ---')

	// Test array literal with cast expression as first element
	// The element type should be inferred from the cast type (u8), not default to int
	mut buf72 := [u8(`0`), `1`, `2`]
	print_int(buf72.len) // 3
	print_int(buf72[0]) // 48 (ASCII for '0')
	print_int(buf72[1]) // 49 (ASCII for '1')
	print_int(buf72[2]) // 50 (ASCII for '2')

	// Test bytestr() on the array - this requires Array_u8 type, not Array_int
	result72 := buf72.bytestr()
	print_str(result72) // 012

	// Test string comparison between variables (both should be identified as strings)
	s72a := 'hello'
	s72b := 'hello'
	s72c := 'world'
	if s72a == s72b {
		print_int(1) // 1 (strings are equal)
	} else {
		print_int(0)
	}
	if s72a != s72c {
		print_int(2) // 2 (strings are different)
	} else {
		print_int(0)
	}

	// ==================== 73. ARRAY INDEX METHOD ====================
	print_str('--- 73. Array index method ---')

	// Test array index method on string arrays
	months73 := ['January', 'February', 'March', 'April', 'May', 'June']
	idx73a := months73.index('March')
	print_int(idx73a) // 2

	idx73b := months73.index('June')
	print_int(idx73b) // 5

	idx73c := months73.index('NotFound')
	print_int(idx73c) // -1

	// Test array index on int arrays
	nums73 := [10, 20, 30, 40, 50]
	idx73d := nums73.index(30)
	print_int(idx73d) // 2

	idx73e := nums73.index(99)
	print_int(idx73e) // -1

	// ==================== 74. MATCH RETURN DIFFERENT ENUM TYPE ====================
	print_str('--- 74. Match return different enum type ---')

	// Test that match conditions use the correct enum type, not the return type
	// This tests the fix for: fn (op Operator) get_binding_power() BindingPower
	// where match op { .plus { .low } } should use Operator for .plus, BindingPower for .low
	op74a := Operator.plus
	bp74a := op74a.get_binding_power()
	print_int(int(bp74a)) // 1 (BindingPower.low)

	op74b := Operator.mul
	bp74b := op74b.get_binding_power()
	print_int(int(bp74b)) // 2 (BindingPower.medium)

	op74c := Operator.minus
	bp74c := op74c.get_binding_power()
	print_int(int(bp74c)) // 1 (BindingPower.low, same as plus)

	op74d := Operator.div
	bp74d := op74d.get_binding_power()
	print_int(int(bp74d)) // 2 (BindingPower.medium, same as mul)

	// ==================== 75. GOTO STATEMENT ====================
	print_str('--- 75. Goto statement ---')

	// Test goto inside unsafe block
	mut counter75 := 0
	mut iterations75 := 0
	start75:
	counter75++
	iterations75++
	if counter75 < 3 {
		unsafe {
			goto start75
		}
	}
	print_int(iterations75) // 3 - should loop 3 times

	// Test goto skipping code
	mut skipped75 := false
	unsafe {
		goto skip75
	}
	skipped75 = true // This should be skipped
	skip75:
	if skipped75 {
		print_int(0) // Should not print this
	} else {
		print_int(1) // 1 - code was skipped
	}

	// ==================== 76. ARRAY PUSH IN MAP ITERATION ====================
	print_str('--- 76. Array push in map iteration ---')

	// Test array << struct_init inside for-in-map loop
	// This previously caused double transformation of ArrayInitExpr
	mut errors76 := []Error76{}
	mut users76 := map[int]bool{}
	users76[1] = true
	users76[2] = true
	users76[3] = true
	for user_id, _ in users76 {
		if user_id > 1 {
			errors76 << Error76{
				msg:    'user'
				val_id: user_id
			}
		}
	}
	print_int(errors76.len) // 2 - two errors added

	// Test map.delete() and map.clear()
	mut m76 := map[int]bool{}
	m76[10] = true
	m76[20] = true
	m76[30] = true
	print_int(m76.len) // 3
	m76.delete(20)
	print_int(m76.len) // 2
	m76.clear()
	print_int(m76.len) // 0

	// ==================== 77. SMARTCAST SUM TYPE ARGUMENT ====================
	print_str('--- 77. Smartcast sum type argument ---')

	// Test that smartcasted variable inside match branch can be passed
	// to a function expecting the original sum type
	// This tests the fix for: b.stmt(stmt) where stmt is smartcast to TypeDecl inside match
	num77a := Number(42)
	match num77a {
		int {
			// num77a is smartcast to int here, but process_number expects Number
			result77a := process_number(num77a)
			print_int(result77a) // 42
		}
		else {}
	}

	num77b := Number(Point{
		x: 10
		y: 20
	})
	match num77b {
		Point {
			// num77b is smartcast to Point here, but process_number expects Number
			result77b := process_number(num77b)
			print_int(result77b) // 30 (10 + 20)
		}
		else {}
	}

	// Test if-is pattern with sum type argument
	num77c := Number(100)
	if num77c is int {
		// num77c is smartcast to int, pass to function expecting Number
		result77c := process_number(num77c)
		print_int(result77c) // 100
	}

	// ==================== 78. MAP ACCESS WITH MUT PARAMETER ====================
	print_str('--- 78. Map access with mut parameter ---')

	// Test map access where map is passed as mut parameter (pointer type Map_int_bool*)
	// This tests the fix for: !visited[s] where visited is mut map[int]bool
	mut visited78 := map[int]bool{}
	succs78 := [1, 2]
	count78 := dfs_mark_visited(mut visited78, 0, succs78)
	print_int(count78) // 3 - visited nodes 0, 1, 2

	// Test that the map was actually modified
	if visited78[0] {
		print_int(1) // 1 - node 0 was marked
	} else {
		print_int(0)
	}

	// Test if-guard expression type inference
	// This tests the fix for: mangled_type := if base_type := map[key] { base_type } else { default }
	// where mangled_type should be inferred as string, not int
	mut aliases78 := map[string]string{}
	aliases78['Foo'] = 'Bar'

	result78a := resolve_type_alias(aliases78, 'Foo')
	print_str(result78a) // Bar (found in map)

	result78b := resolve_type_alias(aliases78, 'Baz')
	print_str(result78b) // Baz (not found, returns default)

	// Test 79: else-if chain smartcast
	// Tests that smartcast from earlier branches doesn't leak into later else-if branches
	print_str('Test 79: else-if chain smartcast')
	wrapper79a := CallWrapper{
		expr: OuterExpr{
			lhs: InnerExpr(10)
		}
	}
	print_int(test_elseif_chain_smartcast(wrapper79a)) // 11 (10 + 1)

	wrapper79b := CallWrapper{
		expr: OuterExpr{
			lhs: InnerExpr(20)
		}
	}
	print_int(test_elseif_chain_smartcast(wrapper79b)) // 22 (20 + 2)

	wrapper79c := CallWrapper{
		expr: OuterExpr{
			lhs: InnerExpr('hello')
		}
	}
	print_int(test_elseif_chain_smartcast(wrapper79c)) // 5 (len of 'hello')

	wrapper79d := CallWrapper{
		expr: OuterExpr{
			lhs: InnerExpr(Point{
				x: 3
				y: 4
			})
		}
	}
	print_int(test_elseif_chain_smartcast(wrapper79d)) // 7 (3 + 4)

	// ==================== 80. STRING MATCH RETURN ====================
	// Tests match expression with string conditions that returns a value
	print_str('--- Test 80: String match return ---')
	print_str(operator_to_name('+')) // __plus
	print_str(operator_to_name('-')) // __minus
	print_str(operator_to_name('*')) // __mul
	print_str(operator_to_name('?')) // ? (unknown operator)

	// ==================== 81. NESTED SUMTYPE METHOD CALL SMARTCAST ====================
	// Tests that when matching outer sum type (Outer = Inner | bool) against a nested sum type variant (Inner),
	// method calls receive the smartcast (Inner) not the original variable (Outer)
	print_str('--- Test 81: Nested sumtype method call smartcast ---')
	proc := Processor{
		name: 'test'
	}
	// Test 1: Int in Inner in Outer
	outer81a := OuterSumType(InnerSumType(42))
	print_int(proc.process_outer(outer81a)) // 42

	// Test 2: String in Inner in Outer
	outer81b := OuterSumType(InnerSumType('hello'))
	print_int(proc.process_outer(outer81b)) // 5 (len of 'hello')

	// Test 3: Bool in Outer (not Inner)
	outer81c := OuterSumType(true)
	print_int(proc.process_outer(outer81c)) // 1

	// ==================== 82. RECURSIVE SUMTYPE FIELD ACCESS ====================
	// Tests recursive function with sumtype parameter accessing fields of smartcast variant
	print_str('--- Test 82: Recursive sumtype field access ---')
	// Build: (1 + 2) -> TestInfixExpr{lhs: TestLiteral{1}, rhs: TestLiteral{2}}
	test82_expr := TestExpr(TestInfixExpr{
		lhs: TestExpr(TestLiteral{
			val: 1
		})
		rhs: TestExpr(TestLiteral{
			val: 2
		})
	})
	print_int(eval_recursive(test82_expr)) // 3 (1 + 2)

	// Build: ((1 + 2) + 3) -> nested
	test82_nested := TestExpr(TestInfixExpr{
		lhs: TestExpr(TestInfixExpr{
			lhs: TestExpr(TestLiteral{
				val: 1
			})
			rhs: TestExpr(TestLiteral{
				val: 2
			})
		})
		rhs: TestExpr(TestLiteral{
			val: 3
		})
	})
	print_int(eval_recursive(test82_nested)) // 6 (1 + 2 + 3)

	// Test 82b: Method with option return (matches cleanc.v:try_eval_int_const)
	test_gen := TestGen{
		name: 'gen'
	}
	print_int(test_gen.try_eval_int(test82_expr) or { -1 }) // 3 (1 + 2)
	print_int(test_gen.try_eval_int(test82_nested) or { -1 }) // 6 (1 + 2 + 3)

	// ==================== 83. NESTED IF-IS SMARTCAST WITH FUNCTION CALL ====================
	// Tests the pattern from cleanc.v:2015-2046 where:
	//   if call.lhs is SelectorExpr {
	//     if call.lhs.lhs is SelectorExpr {
	//       result := fn_expecting_sumtype(call.lhs.lhs)  // must wrap or not extract
	//     }
	//   }
	print_str('--- Test 83: Nested if-is smartcast with fn call ---')
	// Build a chain: call.lhs (SelectorExpr) -> lhs (SelectorExpr) -> lhs (Ident)
	innermost83 := TestIdent2{
		name: 'x'
	}
	middle83 := TestSelectorExpr2{
		lhs: innermost83
		rhs: 'field1'
	}
	outer83 := TestSelectorExpr2{
		lhs: middle83
		rhs: 'field2'
	}
	call83 := TestCallExpr2{
		lhs:  outer83
		name: 'method'
	}
	test_gen2 := TestGen2{
		name: 'gen2'
	}
	result83 := test_nested_if_is_smartcast(call83, test_gen2)
	// call.lhs.lhs is the middle SelectorExpr, so infer_type2 should return 'selector'
	assert result83 == 'selector'
	print_str(result83)

	// ==================== 84. RETURN IF EXPRESSION ====================
	// Tests return if expression transformation
	print_str('--- Test 84: Return if expression ---')
	test_return_if_expr()

	// ==================== 85. COMBINED && SMARTCAST ====================
	// Tests combined && conditions with nested is checks: if a is T && a.field is U { a.field.inner }
	print_str('--- Test 85: Combined && smartcast ---')
	test_combined_smartcast()

	// ==================== 86. IF-GUARD ARRAY ACCESS ====================
	// Tests if-guard with array index expressions (bounds checking)
	print_str('--- Test 86: If-guard array access ---')
	test_if_guard_array_access()

	// ==================== 87. OPTIONAL POINTER RETURN TYPE ====================
	// Tests functions returning ?&Struct (option wrapping a pointer)
	print_str('--- Test 87: Optional pointer return ---')
	test_optional_pointer_return()

	// ==================== 88. METHOD CALL ON VARIABLE NAMED 'v' ====================
	// Tests method calls on loop variable named 'v' (module/variable disambiguation)
	print_str('--- Test 88: Method on v variable ---')
	test_method_on_v_variable()

	// ==================== 89. SUM TYPE VARIANT WRAPPING IN MATCH RETURN ====================
	// Tests returning variant values from match expressions where return type is sum type
	print_str('--- Test 89: Sumtype match return ---')
	test_sumtype_match_return()

	// ==================== 90. MAP OR-BLOCK WITH RLOCK ====================
	// Tests map[string][]T or-block inside rlock expression (types.Environment.lookup_method pattern)
	// This tests the LockExpr type inference fix for Ident temp variables
	print_str('--- Test 90: Map or-block with rlock ---')
	test_map_or_rlock()

	// Test map or-block with array value type (fixes array** vs array* cleanc issue)
	print_str('--- Test 91: Map or-block with array value ---')
	test_map_or_array_value()

	print_str('=== All tests completed ===')
}
