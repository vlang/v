// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.ast
import v2.types
// import v2.token

pub struct Builder {
mut:
	mod       &Module
	cur_func  int     = -1
	cur_block BlockID = -1

	// Maps AST variable name to SSA ValueID (pointer to stack slot)
	vars map[string]ValueID

	// Maps variable name to struct type name (for method resolution)
	var_struct_types map[string]string

	// Stack for break/continue targets
	loop_stack []LoopInfo

	// Maps struct name to TypeID
	struct_types map[string]TypeID

	// Current match expression type for enum shorthand
	cur_match_type string

	// Deferred statements for current function (executed in reverse order at return)
	defer_stmts [][]ast.Stmt

	// Maps enum value (EnumName__field) to integer value
	enum_values map[string]int

	// Maps variable name to array length (for fixed-size arrays)
	var_array_sizes map[string]int
}

struct LoopInfo {
	head BlockID
	exit BlockID
}

pub fn Builder.new(mod &Module) &Builder {
	return &Builder{
		mod:              mod
		vars:             map[string]ValueID{}
		var_struct_types: map[string]string{}
		loop_stack:       []LoopInfo{}
		struct_types:     map[string]TypeID{}
		enum_values:      map[string]int{}
		var_array_sizes:  map[string]int{}
	}
}

// Convert v2.types.Type to SSA TypeID
fn (mut b Builder) type_to_ssa(t types.Type) TypeID {
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				size := if t.size == 0 { 64 } else { int(t.size) } // int defaults to 64-bit
				return b.mod.type_store.get_int(size)
			} else if t.props.has(.float) {
				return b.mod.type_store.get_float(int(t.size))
			} else if t.props.has(.boolean) {
				return b.mod.type_store.get_int(8) // bool as i8
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Pointer {
			elem_type := b.type_to_ssa(t.base_type)
			return b.mod.type_store.get_ptr(elem_type)
		}
		types.Array {
			elem_type := b.type_to_ssa(t.elem_type)
			return b.mod.type_store.get_ptr(elem_type) // arrays are pointers in SSA
		}
		types.Struct {
			// Check if already registered
			if struct_id := b.struct_types[t.name] {
				return struct_id
			}
			// Convert and register
			mut ssa_fields := []TypeID{}
			mut ssa_field_names := []string{}
			for field in t.fields {
				ssa_fields << b.type_to_ssa(field.typ)
				ssa_field_names << field.name
			}
			struct_id := b.mod.type_store.register(Type{
				kind:        .struct_t
				fields:      ssa_fields
				field_names: ssa_field_names
			})
			b.struct_types[t.name] = struct_id
			return struct_id
		}
		types.String {
			// String is a special type - return the string struct
			if struct_id := b.struct_types['string'] {
				return struct_id
			}
			// Should not happen if register_string_type() was called
			return 0
		}
		types.Alias {
			return b.type_to_ssa(t.base_type)
		}
		types.Char {
			return b.mod.type_store.get_int(8)
		}
		types.Void {
			return 0 // void type
		}
		else {
			// Fallback for unhandled types
			return b.mod.type_store.get_int(64)
		}
	}
}

pub fn (mut b Builder) build(file ast.File) {
	// 0. Pre-pass: Register Types (Structs), Globals, and Enums
	// We must process these first so types exist when we compile functions.
	for stmt in file.stmts {
		match stmt {
			ast.StructDecl { b.stmt(stmt) }
			ast.GlobalDecl { b.stmt(stmt) }
			ast.EnumDecl { b.stmt(stmt) }
			else {}
		}
	}

	// 1. First pass: Register all functions (so calls work)
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			// For MVP, assume (i32, i32) -> i32
			i32_t := b.mod.type_store.get_int(64)

			// Map params
			mut param_types := []TypeID{}

			// For methods, add receiver as first parameter (always as pointer)
			if stmt.is_method {
				if stmt.receiver.typ is ast.Ident {
					if struct_t := b.struct_types[stmt.receiver.typ.name] {
						param_types << b.mod.type_store.get_ptr(struct_t)
					} else {
						param_types << i32_t
					}
				} else {
					param_types << i32_t
				}
			}

			// FIX: params are inside the 'typ' (FnType) struct
			for _ in stmt.typ.params {
				param_types << i32_t
			}

			// Create Function Skeleton
			// For methods, use mangled name: TypeName_methodName
			mut fn_name := stmt.name
			if stmt.is_method {
				if stmt.receiver.typ is ast.Ident {
					fn_name = '${stmt.receiver.typ.name}_${stmt.name}'
				}
			}
			// We discard the returned ID because we assume linear order in the next pass
			b.mod.new_function(fn_name, i32_t, param_types)
		}
	}

	// 2. Second pass: Generate Body
	// We rely on index matching for simplicity in this demo.
	mut fn_idx := 0
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			b.build_fn(stmt, fn_idx)
			fn_idx++
		}
	}
}

fn (mut b Builder) build_fn(decl ast.FnDecl, fn_id int) {
	b.cur_func = fn_id
	b.vars.clear()
	b.var_struct_types.clear()
	b.defer_stmts.clear()

	// Create Entry Block
	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry

	// Define Arguments
	i32_t := b.mod.type_store.get_int(64)

	// Handle method receiver as first parameter
	// Note: receivers are always passed as pointers internally
	if decl.is_method {
		receiver := decl.receiver
		mut receiver_type := i32_t
		mut struct_type_name := ''

		if receiver.typ is ast.Ident {
			struct_type_name = receiver.typ.name
			if struct_t := b.struct_types[receiver.typ.name] {
				// Always use pointer for receiver (structs are passed by reference)
				receiver_type = b.mod.type_store.get_ptr(struct_t)
			}
		}

		// 1. Create Argument Value for receiver
		arg_val := b.mod.add_value_node(.argument, receiver_type, receiver.name, 0)
		b.mod.funcs[fn_id].params << arg_val

		// 2. Allocate Stack Slot
		stack_ptr := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(receiver_type),
			[])

		// 3. Store Argument to Stack
		b.mod.add_instr(.store, entry, 0, [arg_val, stack_ptr])

		// 4. Register variable
		b.vars[receiver.name] = stack_ptr

		// 5. Track struct type for method resolution
		if struct_type_name != '' {
			b.var_struct_types[receiver.name] = struct_type_name
		}
	}

	// FIX: Access params via decl.typ.params
	for _, param in decl.typ.params {
		// Determine actual parameter type
		mut param_type := i32_t
		mut struct_type_name := ''

		// Check if parameter type is a struct (look up by name)
		if param.typ is ast.Ident {
			if struct_t := b.struct_types[param.typ.name] {
				struct_type_name = param.typ.name
				// For mut params or large structs (>16 bytes), use pointer
				// This matches ARM64 ABI where structs > 16 bytes are passed by reference
				struct_type := b.mod.type_store.types[struct_t]
				struct_size := struct_type.fields.len * 8 // Approximate size
				if param.is_mut || struct_size > 16 {
					param_type = b.mod.type_store.get_ptr(struct_t)
				} else {
					param_type = struct_t
				}
			}
		}

		// 1. Create Argument Value
		arg_val := b.mod.add_value_node(.argument, param_type, param.name, 0)
		b.mod.funcs[fn_id].params << arg_val

		// 2. Allocate Stack Slot (so we can modify it if needed)
		stack_ptr := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(param_type),
			[])

		// 3. Store Argument to Stack
		b.mod.add_instr(.store, entry, 0, [arg_val, stack_ptr])

		// 4. Register variable
		b.vars[param.name] = stack_ptr

		// 5. Track struct type for method resolution
		if struct_type_name != '' {
			b.var_struct_types[param.name] = struct_type_name
		}
	}

	// Process Statements
	b.stmts(decl.stmts)
	// FIX: Ensure the function ends with a return to prevent fallthrough
	if !b.is_block_terminated(b.cur_block) {
		// Emit deferred statements before implicit return
		b.emit_deferred_stmts()

		ret_type := b.mod.funcs[fn_id].typ
		if ret_type != 0 {
			// Return 0 for non-void functions (satisfy C signature)
			zero := b.mod.add_value_node(.constant, ret_type, '0', 0)
			b.mod.add_instr(.ret, b.cur_block, 0, [zero])
		} else {
			// void return
			b.mod.add_instr(.ret, b.cur_block, 0, [])
		}
	}
}

fn (mut b Builder) stmts(stmts []ast.Stmt) {
	for s in stmts {
		b.stmt(s)
	}
}

// stmt_for_in handles `for i in start..end { ... }` and `for elem in array { ... }`
fn (mut b Builder) stmt_for_in(node ast.ForStmt, for_in ast.ForInStmt) {
	// Get loop variable name(s)
	mut key_name := ''
	mut value_name := ''
	if for_in.key !is ast.EmptyExpr {
		if for_in.key is ast.Ident {
			key_name = for_in.key.name
		} else if for_in.key is ast.ModifierExpr {
			if for_in.key.expr is ast.Ident {
				key_name = for_in.key.expr.name
			}
		}
	}
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	// Check if this is a range expression or array iteration
	if for_in.expr is ast.RangeExpr {
		// Range iteration: for i in start..end
		b.stmt_for_in_range(node, for_in, value_name)
	} else {
		// Array iteration: for elem in array or for i, elem in array
		b.stmt_for_in_array(node, for_in, key_name, value_name)
	}
}

// stmt_for_in_range handles `for i in start..end { ... }`
fn (mut b Builder) stmt_for_in_range(node ast.ForStmt, for_in ast.ForInStmt, var_name string) {
	i64_t := b.mod.type_store.get_int(64)

	range_expr := for_in.expr as ast.RangeExpr
	start_val := b.expr(range_expr.start)
	end_val := b.expr(range_expr.end)

	// Allocate loop variable on stack
	ptr_t := b.mod.type_store.get_ptr(i64_t)
	var_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	b.vars[var_name] = var_ptr

	// Initialize loop variable to start
	b.mod.add_instr(.store, b.cur_block, 0, [start_val, var_ptr])

	// Create control flow blocks
	// We use a post block so that continue jumps to the increment, not the condition
	head_blk := b.mod.add_block(b.cur_func, 'for_in.head')
	body_blk := b.mod.add_block(b.cur_func, 'for_in.body')
	post_blk := b.mod.add_block(b.cur_func, 'for_in.post')
	exit_blk := b.mod.add_block(b.cur_func, 'for_in.exit')

	// For continue, we want to jump to post (which increments then goes to head)
	// For break, we want to jump to exit
	b.loop_stack << LoopInfo{
		head: post_blk // continue jumps here (to increment)
		exit: exit_blk // break jumps here
	}

	// Jump to head
	head_val := b.mod.blocks[head_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Head: check i < end
	b.cur_block = head_blk
	cur_val := b.mod.add_instr(.load, b.cur_block, i64_t, [var_ptr])
	cond := b.mod.add_instr(.lt, b.cur_block, i64_t, [cur_val, end_val])
	body_val := b.mod.blocks[body_blk].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond, body_val, exit_val])

	// Body
	b.cur_block = body_blk
	b.stmts(node.stmts)

	// Jump to post at end of body (if not already terminated by break/continue/return)
	if !b.is_block_terminated(b.cur_block) {
		post_val := b.mod.blocks[post_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [post_val])
	}

	// Post: increment i and jump back to head
	b.cur_block = post_blk
	cur_val2 := b.mod.add_instr(.load, b.cur_block, i64_t, [var_ptr])
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	new_val := b.mod.add_instr(.add, b.cur_block, i64_t, [cur_val2, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_val, var_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Exit
	b.cur_block = exit_blk
	b.loop_stack.pop()
}

// stmt_for_in_array handles `for elem in array { ... }` and `for i, elem in array { ... }`
fn (mut b Builder) stmt_for_in_array(node ast.ForStmt, for_in ast.ForInStmt, key_name string, value_name string) {
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)
	elem_ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Get the array pointer - addr returns pointer to where array ptr is stored
	array_ptr_ptr := b.addr(for_in.expr)
	// Load the actual array pointer
	array_ptr := b.mod.add_instr(.load, b.cur_block, ptr_t, [array_ptr_ptr])

	// For simplicity, we assume arrays have a fixed known length or we get it from context
	// In V's Array struct, length is typically in a .len field
	// For now, we'll need the array length - this is a simplified implementation
	// that assumes arrays are pointers to contiguous memory with a known length

	// Allocate index variable on stack (always need index for array access)
	idx_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, idx_ptr])

	// If key variable is specified, register it
	if key_name != '' {
		b.vars[key_name] = idx_ptr
	}

	// Allocate value variable on stack
	value_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	b.vars[value_name] = value_ptr

	// Create control flow blocks
	head_blk := b.mod.add_block(b.cur_func, 'for_in_arr.head')
	body_blk := b.mod.add_block(b.cur_func, 'for_in_arr.body')
	post_blk := b.mod.add_block(b.cur_func, 'for_in_arr.post')
	exit_blk := b.mod.add_block(b.cur_func, 'for_in_arr.exit')

	b.loop_stack << LoopInfo{
		head: post_blk // continue jumps here (to increment)
		exit: exit_blk // break jumps here
	}

	// Jump to head
	head_val := b.mod.blocks[head_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Head: check idx < array.len
	b.cur_block = head_blk
	cur_idx := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])

	// Get array length from tracked sizes or the expression
	mut arr_len_val := 0
	if for_in.expr is ast.Ident {
		if size := b.var_array_sizes[for_in.expr.name] {
			arr_len_val = size
		}
	} else if for_in.expr is ast.ArrayInitExpr {
		arr_len_val = for_in.expr.exprs.len
	}
	if arr_len_val == 0 {
		arr_len_val = 100 // Fallback
	}
	arr_len := b.mod.add_value_node(.constant, i64_t, '${arr_len_val}', 0)

	cond := b.mod.add_instr(.lt, b.cur_block, i64_t, [cur_idx, arr_len])
	body_val := b.mod.blocks[body_blk].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond, body_val, exit_val])

	// Body: load array element into value variable
	b.cur_block = body_blk
	cur_idx2 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
		array_ptr,
		cur_idx2,
	])
	elem_val := b.mod.add_instr(.load, b.cur_block, i64_t, [elem_addr])
	b.mod.add_instr(.store, b.cur_block, 0, [elem_val, value_ptr])

	b.stmts(node.stmts)

	if !b.is_block_terminated(b.cur_block) {
		post_val := b.mod.blocks[post_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [post_val])
	}

	// Post: increment index and jump back to head
	b.cur_block = post_blk
	cur_idx3 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	new_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [cur_idx3, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_idx, idx_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Exit
	b.cur_block = exit_blk
	b.loop_stack.pop()
}

fn (mut b Builder) stmt(node ast.Stmt) {
	// println('stmt ${node}')
	match node {
		ast.AssignStmt {
			// x := 10 or x = 10
			// 1. Calc RHS
			if node.rhs.len == 0 {
				println('AssignStmt node.rhs.len == 0')
				println(node)
				return
			}
			if node.lhs.len == 0 {
				println('AssignStmt node.lhs.len == 0')
				println(node)
				return
			}
			rhs_val := b.expr(node.rhs[0])

			// 2. Get LHS Address
			// If declaration, allocate new stack slot

			if node.op == .decl_assign {
				mut ident_node := node.lhs[0]
				mut ident := ast.Ident{}
				// Unwrap 'mut x'
				if ident_node is ast.ModifierExpr {
					mod := ident_node as ast.ModifierExpr
					ident = mod.expr as ast.Ident
				} else {
					ident = ident_node as ast.Ident
				}
				// ident := ident_node as ast.Ident
				name := ident.name
				// Alloca

				// Get type from RHS or default to i32
				rhs_type := b.mod.values[rhs_val].typ
				ptr_t := b.mod.type_store.get_ptr(rhs_type)

				stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

				// Store
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, stack_ptr])
				b.vars[name] = stack_ptr

				// Track struct/enum type for method resolution and match shorthand
				rhs_expr := node.rhs[0]
				if rhs_expr is ast.InitExpr {
					if rhs_expr.typ is ast.Ident {
						b.var_struct_types[name] = rhs_expr.typ.name
					}
				} else if rhs_expr is ast.PrefixExpr {
					// Handle &Point{} heap allocation
					if rhs_expr.expr is ast.InitExpr {
						init_expr := rhs_expr.expr as ast.InitExpr
						if init_expr.typ is ast.Ident {
							b.var_struct_types[name] = init_expr.typ.name
						}
					}
				} else if rhs_expr is ast.SelectorExpr {
					// Track enum type for variables assigned enum values (e.g., color1 := Color.red)
					if rhs_expr.lhs is ast.Ident {
						enum_name := '${rhs_expr.lhs.name}__${rhs_expr.rhs.name}'
						if enum_name in b.enum_values {
							b.var_struct_types[name] = rhs_expr.lhs.name
						}
					}
				} else if rhs_expr is ast.ArrayInitExpr {
					// Track array size for for-in loops
					mut arr_size := rhs_expr.exprs.len
					if rhs_expr.len !is ast.EmptyExpr {
						if rhs_expr.len is ast.BasicLiteral {
							if rhs_expr.len.kind == .number {
								arr_size = rhs_expr.len.value.int()
							}
						}
					}
					if arr_size > 0 {
						b.var_array_sizes[name] = arr_size
					}
				}
			} else if node.op in [.plus_assign, .minus_assign, .mul_assign, .div_assign] {
				// Compound assignment: x += 1, x -= 1, x *= 2, x /= 2
				ptr := b.addr(node.lhs[0])
				val_typ := b.mod.type_store.types[b.mod.values[ptr].typ].elem_type

				lhs_val := b.mod.add_instr(.load, b.cur_block, val_typ, [ptr])
				op := match node.op {
					.plus_assign { OpCode.add }
					.minus_assign { OpCode.sub }
					.mul_assign { OpCode.mul }
					.div_assign { OpCode.sdiv }
					else { OpCode.add }
				}
				res := b.mod.add_instr(op, b.cur_block, val_typ, [lhs_val, rhs_val])
				b.mod.add_instr(.store, b.cur_block, 0, [res, ptr])
			} else {
				// Assignment to existing variable, field, or array index
				ptr := b.addr(node.lhs[0])
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
			}
		}
		ast.ReturnStmt {
			// In V/Go semantics: evaluate return value FIRST, then run defer
			// 1. Evaluate return expression (if any)
			mut ret_val := ValueID(0)
			if node.exprs.len > 0 {
				ret_val = b.expr(node.exprs[0])
			}
			// 2. Execute deferred statements in reverse order
			b.emit_deferred_stmts()
			// 3. Return the pre-computed value
			if node.exprs.len > 0 {
				b.mod.add_instr(.ret, b.cur_block, 0, [ret_val])
			} else {
				b.mod.add_instr(.ret, b.cur_block, 0, [])
			}
		}
		ast.DeferStmt {
			// Collect deferred statements to be executed before return
			b.defer_stmts << node.stmts
		}
		ast.ExprStmt {
			b.expr(node.expr)
		}
		ast.BlockStmt {
			b.stmts(node.stmts)
		}
		ast.ForStmt {
			// Check if this is a for-in loop: `for i in 1..10` or `for elem in array`
			if node.init is ast.ForInStmt {
				b.stmt_for_in(node, node.init)
				return
			}

			// 1. Init
			if node.init !is ast.EmptyStmt {
				b.stmt(node.init)
			}

			// 2. Control Flow Blocks
			head_blk := b.mod.add_block(b.cur_func, 'for.head')
			body_blk := b.mod.add_block(b.cur_func, 'for.body')
			exit_blk := b.mod.add_block(b.cur_func, 'for.exit')

			b.loop_stack << LoopInfo{
				head: head_blk
				exit: exit_blk
			}

			// Jump to Head
			head_val := b.mod.blocks[head_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

			// 3. Head (Condition)
			b.cur_block = head_blk
			body_val := b.mod.blocks[body_blk].val_id
			exit_val := b.mod.blocks[exit_blk].val_id

			if node.cond !is ast.EmptyExpr {
				cond_val := b.expr(node.cond)
				b.mod.add_instr(.br, b.cur_block, 0, [cond_val, body_val, exit_val])
			} else {
				// Infinite loop
				b.mod.add_instr(.jmp, b.cur_block, 0, [body_val])
			}

			// 4. Body
			b.cur_block = body_blk
			b.stmts(node.stmts)

			// 5. Post
			if node.post !is ast.EmptyStmt {
				b.stmt(node.post)
			}

			// Loop back
			if !b.is_block_terminated(b.cur_block) {
				b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])
			}

			// 6. Exit
			b.cur_block = exit_blk
			b.loop_stack.pop()
		}
		ast.FlowControlStmt {
			if b.loop_stack.len == 0 {
				return
			}
			info := b.loop_stack.last()
			target := if node.op == .key_break { info.exit } else { info.head }

			target_val := b.mod.blocks[target].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [target_val])
		}
		ast.StructDecl {
			// Register Struct Type
			mut field_types := []TypeID{}
			mut field_names := []string{}
			for field in node.fields {
				// Check if field type is a known struct type
				mut field_type := b.mod.type_store.get_int(64) // default to int64
				if field.typ is ast.Ident {
					type_name := field.typ.name
					if st := b.struct_types[type_name] {
						// Field is a nested struct type
						field_type = st
					}
				}
				field_types << field_type
				field_names << field.name
			}

			// We manually constructing the struct type in the store
			// In a real compiler, we'd map AST types to SSA types properly
			t := Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
				width:       0
			}
			type_id := b.mod.type_store.register(t)
			// Register struct name for type lookup
			b.struct_types[node.name] = type_id
		}
		ast.GlobalDecl {
			for field in node.fields {
				// Check if field type is a known struct type
				mut field_type := b.mod.type_store.get_int(64) // default to int64
				if field.typ is ast.Ident {
					type_name := field.typ.name
					if st := b.struct_types[type_name] {
						// Field is a struct type
						field_type = st
					}
				}
				// Register global
				b.mod.add_global(field.name, field_type, false)
			}
		}
		ast.ConstDecl {
			for field in node.fields {
				// Constants infer type from their value - default to int64
				field_type := b.mod.type_store.get_int(64)
				// Evaluate initial value
				initial_value := b.eval_const_expr(field.value)
				// Register constant with initial value
				b.mod.add_global_with_value(field.name, field_type, true, initial_value)
			}
		}
		ast.EnumDecl {
			// Enums are lowered to integer constants
			// Each field gets an incrementing value starting from 0 (or explicit value)
			field_type := b.mod.type_store.get_int(64)
			mut next_value := 0
			for field in node.fields {
				// If field has explicit value, use it
				if field.value is ast.BasicLiteral {
					if field.value.kind == .number {
						next_value = field.value.value.int()
					}
				}
				// Register enum value with its integer value
				const_name := '${node.name}__${field.name}'
				b.enum_values[const_name] = next_value
				// Also register as global for native backends
				b.mod.add_global_with_value(const_name, field_type, true, i64(next_value))
				next_value++
			}
		}
		ast.LabelStmt {
			// Labels are used for labeled loops (break/continue targets)
			// For now, we just process the associated statement if any
			// TODO: Track labels for labeled break/continue support
			if node.stmt !is ast.EmptyStmt {
				b.stmt(node.stmt)
			}
		}
		ast.InterfaceDecl {
			// Interfaces are similar to structs but define a vtable
			// For SSA purposes, register as a struct type with method pointers
			mut field_types := []TypeID{}
			mut field_names := []string{}
			for field in node.fields {
				// Interface fields are method signatures (FnType) or regular fields
				field_type := b.mod.type_store.get_int(64) // Default to int64 for now
				field_types << field_type
				field_names << field.name
			}
			// Register interface as a struct type
			t := Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
				width:       0
			}
			type_id := b.mod.type_store.register(t)
			b.struct_types[node.name] = type_id
		}
		ast.TypeDecl {
			// Type declarations: type aliases or sum types
			if node.variants.len > 0 {
				// Sum type: register as a tagged union
				// For now, treat as an integer (the tag)
				field_type := b.mod.type_store.get_int(64)
				t := Type{
					kind:        .struct_t
					fields:      [field_type, field_type] // tag + data
					field_names: ['_tag', '_data']
					width:       0
				}
				type_id := b.mod.type_store.register(t)
				b.struct_types[node.name] = type_id
			} else if node.base_type !is ast.EmptyExpr {
				// Type alias: look up the base type
				if node.base_type is ast.Ident {
					base_name := node.base_type.name
					if st := b.struct_types[base_name] {
						// Alias to existing struct type
						b.struct_types[node.name] = st
					} else {
						// Alias to primitive type - register as int64
						t := Type{
							kind:  .int_t
							width: 64
						}
						type_id := b.mod.type_store.register(t)
						b.struct_types[node.name] = type_id
					}
				}
			}
		}
		ast.AssertStmt {
			// Assert: if condition is false, abort/exit
			// Generate: if (!cond) { unreachable/abort }
			i64_t := b.mod.type_store.get_int(64)

			// Evaluate the assertion condition
			cond_val := b.expr(node.expr)

			// Create blocks for pass and fail paths
			pass_blk := b.mod.add_block(b.cur_func, 'assert.pass')
			fail_blk := b.mod.add_block(b.cur_func, 'assert.fail')

			// Branch: if cond is true, go to pass; else go to fail
			pass_val := b.mod.blocks[pass_blk].val_id
			fail_val := b.mod.blocks[fail_blk].val_id
			b.mod.add_instr(.br, b.cur_block, 0, [cond_val, pass_val, fail_val])

			// Fail block: call abort() or exit(1) then unreachable
			b.cur_block = fail_blk
			// Call C.exit(1) to terminate
			exit_fn := b.mod.add_value_node(.unknown, 0, 'exit', 0)
			one := b.mod.add_value_node(.constant, i64_t, '1', 0)
			b.mod.add_instr(.call, b.cur_block, i64_t, [exit_fn, one])
			b.mod.add_instr(.unreachable, b.cur_block, 0, [])

			// Continue in pass block
			b.cur_block = pass_blk
		}
		else {
			// println('Builder: Unhandled stmt ${node.type_name()}')
		}
	}
}

fn (mut b Builder) expr(node ast.Expr) ValueID {
	match node {
		ast.BasicLiteral {
			return b.expr_basic_literal(node)
		}
		ast.Ident {
			return b.expr_ident(node)
		}
		ast.InitExpr {
			return b.expr_init(node)
		}
		ast.SelectorExpr {
			// Check if this is an enum value access (EnumName.value)
			if node.lhs is ast.Ident {
				enum_name := '${node.lhs.name}__${node.rhs.name}'
				if enum_val := b.enum_values[enum_name] {
					i64_t := b.mod.type_store.get_int(64)
					return b.mod.add_value_node(.constant, i64_t, '${enum_val}', 0)
				}
			}
			// Check for enum shorthand (.value) - LHS is EmptyExpr
			if node.lhs is ast.EmptyExpr {
				if b.cur_match_type != '' {
					enum_name := '${b.cur_match_type}__${node.rhs.name}'
					if enum_val := b.enum_values[enum_name] {
						i64_t := b.mod.type_store.get_int(64)
						return b.mod.add_value_node(.constant, i64_t, '${enum_val}', 0)
					}
				}
			}
			return b.expr_selector(node)
		}
		ast.IndexExpr {
			return b.expr_index(node)
		}
		ast.CastExpr {
			return b.expr(node.expr)
		}
		ast.ParenExpr {
			return b.expr(node.expr)
		}
		ast.InfixExpr {
			return b.expr_infix(node)
		}
		ast.IfExpr {
			return b.expr_if(node)
		}
		ast.MatchExpr {
			return b.expr_match(node)
		}
		ast.CallExpr {
			return b.expr_call(node)
		}
		ast.StringLiteral {
			return b.expr_string_literal(node)
		}
		ast.StringInterLiteral {
			return b.expr_string_inter_literal(node)
		}
		ast.CallOrCastExpr {
			return b.expr_call_or_cast(node)
		}
		ast.PrefixExpr {
			return b.expr_prefix(node)
		}
		ast.PostfixExpr {
			return b.expr_postfix(node)
		}
		ast.ModifierExpr {
			// Handle 'mut x' - just unwrap and process the inner expression
			return b.expr(node.expr)
		}
		ast.ArrayInitExpr {
			return b.expr_array_init(node)
		}
		ast.IfGuardExpr {
			return b.expr_if_guard(node)
		}
		ast.RangeExpr {
			return b.expr_range(node)
		}
		ast.Type {
			// Handle type expressions (none, nil, etc.)
			// For none/nil, return 0 (null/false value)
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i64_t, '0', 0)
		}
		ast.Keyword {
			// Handle keywords like none, true, false
			i64_t := b.mod.type_store.get_int(64)
			match node.tok {
				.key_none, .key_nil {
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				.key_true {
					return b.mod.add_value_node(.constant, i64_t, '1', 0)
				}
				.key_false {
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				else {
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
			}
		}
		ast.ComptimeExpr {
			// Handle comptime expressions like $if macos { ... } $else { ... }
			return b.expr_comptime(node)
		}
		else {
			println('Builder: Unhandled expr ${node.type_name()}')
			// Return constant 0 (i32) to prevent cascading void errors
			i32_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i32_t, '0', 0)
		}
	}
}

fn (mut b Builder) expr_basic_literal(node ast.BasicLiteral) ValueID {
	if node.kind == .number {
		// Constant
		i32_t := b.mod.type_store.get_int(64)
		val := b.mod.add_value_node(.constant, i32_t, node.value, 0)
		return val
	} else if node.kind in [.key_true, .key_false] {
		i32_t := b.mod.type_store.get_int(64)
		val_str := if node.kind == .key_true { '1' } else { '0' }
		val := b.mod.add_value_node(.constant, i32_t, val_str, 0)
		return val
	}
	return 0
}

fn (mut b Builder) expr_ident(node ast.Ident) ValueID {
	ptr := b.addr(node)
	// Get type pointed to
	ptr_typ := b.mod.values[ptr].typ
	val_typ := b.mod.type_store.types[ptr_typ].elem_type

	return b.mod.add_instr(.load, b.cur_block, val_typ, [ptr])
}

fn (mut b Builder) expr_init(node ast.InitExpr) ValueID {
	// Struct Init: MyStruct{ a: 1, b: 2 }
	// 1. Allocate Struct
	// Need to find the TypeID for the struct.
	mut struct_t := 0

	// Try to get struct type from the type name in the init expression
	if node.typ is ast.Ident {
		if st := b.struct_types[node.typ.name] {
			struct_t = st
		}
	}

	// Fallback: search for first struct type (for backwards compatibility)
	if struct_t == 0 {
		for i, t in b.mod.type_store.types {
			if t.kind == .struct_t {
				struct_t = i
				break
			}
		}
	}

	ptr_t := b.mod.type_store.get_ptr(struct_t)
	struct_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

	// 2. Initialize Fields
	// Build a map of explicitly initialized fields by name
	mut init_fields := map[string]ast.Expr{}
	for field in node.fields {
		init_fields[field.name] = field.value
	}

	// Get struct type info to iterate all fields
	struct_type := b.mod.type_store.types[struct_t]

	// Initialize all fields (explicit value or zero)
	for i, field_name in struct_type.field_names {
		idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), i.str(),
			0)

		// GEP to field
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(b.mod.type_store.get_int(64)),
			[struct_ptr, idx_val])

		if expr := init_fields[field_name] {
			// Explicitly initialized
			val := b.expr(expr)
			b.mod.add_instr(.store, b.cur_block, 0, [val, field_ptr])
		} else {
			// Zero initialize
			zero_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64),
				'0', 0)
			b.mod.add_instr(.store, b.cur_block, 0, [zero_val, field_ptr])
		}
	}

	// 3. Return Pointer (Structs are value types in V, but usually passed by ref in SSA construction phase or loaded)
	return struct_ptr
}

fn (mut b Builder) expr_selector(node ast.SelectorExpr) ValueID {
	// Load value from field
	ptr := b.addr(node)
	// Get the actual field type from the pointer type
	ptr_val := b.mod.values[ptr]
	ptr_typ := b.mod.type_store.types[ptr_val.typ]
	field_typ := ptr_typ.elem_type // Dereference the pointer type to get field type
	return b.mod.add_instr(.load, b.cur_block, field_typ, [ptr])
}

fn (mut b Builder) expr_index(node ast.IndexExpr) ValueID {
	// Check if this is a range-based slice (arr[start..end])
	if node.expr is ast.RangeExpr {
		return b.expr_slice(node.lhs, node.expr)
	}
	// Load value from index
	ptr := b.addr(node)
	i32_t := b.mod.type_store.get_int(64) // Assume i32
	return b.mod.add_instr(.load, b.cur_block, i32_t, [ptr])
}

fn (mut b Builder) expr_slice(base ast.Expr, range_expr ast.RangeExpr) ValueID {
	// Array slicing: arr[start..end]
	// Returns a new heap-allocated array containing elements from start to end-1
	i64_t := b.mod.type_store.get_int(64)
	elem_ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Get base array pointer
	base_ptr := b.addr(base)

	// Auto-dereference if it's a pointer-to-pointer
	base_val := b.mod.values[base_ptr]
	ptr_typ := b.mod.type_store.types[base_val.typ]
	elem_typ_id := ptr_typ.elem_type
	elem_typ := b.mod.type_store.types[elem_typ_id]

	mut actual_base := base_ptr
	if elem_typ.kind == .ptr_t {
		actual_base = b.mod.add_instr(.load, b.cur_block, elem_typ_id, [base_ptr])
	}

	// Evaluate start and end indices
	start_val := b.expr(range_expr.start)
	end_val := b.expr(range_expr.end)

	// Calculate slice length: end - start
	slice_len := b.mod.add_instr(.sub, b.cur_block, i64_t, [end_val, start_val])

	// Calculate allocation size: len * 8 (sizeof int64)
	elem_size := b.mod.add_value_node(.constant, i64_t, '8', 0)
	alloc_size := b.mod.add_instr(.mul, b.cur_block, i64_t, [slice_len, elem_size])

	// Call malloc to allocate heap memory
	malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
	slice_ptr := b.mod.add_instr(.call, b.cur_block, elem_ptr_t, [malloc_fn, alloc_size])

	// Copy elements in a loop: for i = 0; i < len; i++ { slice[i] = base[start + i] }
	head_blk := b.mod.add_block(b.cur_func, 'slice.head')
	body_blk := b.mod.add_block(b.cur_func, 'slice.body')
	exit_blk := b.mod.add_block(b.cur_func, 'slice.exit')

	// Allocate loop counter on stack
	counter_ptr := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(i64_t),
		[])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, counter_ptr])

	// Jump to loop head
	head_val := b.mod.blocks[head_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Loop head: check counter < slice_len
	b.cur_block = head_blk
	counter := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])
	cond := b.mod.add_instr(.lt, b.cur_block, i64_t, [counter, slice_len])
	body_val := b.mod.blocks[body_blk].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond, body_val, exit_val])

	// Loop body: copy one element
	b.cur_block = body_blk
	counter2 := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])

	// Source: base[start + counter]
	src_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [start_val, counter2])
	src_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
		actual_base,
		src_idx,
	])
	elem_val := b.mod.add_instr(.load, b.cur_block, i64_t, [src_ptr])

	// Dest: slice[counter]
	dst_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [slice_ptr, counter2])
	b.mod.add_instr(.store, b.cur_block, 0, [elem_val, dst_ptr])

	// Increment counter
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	counter3 := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])
	new_counter := b.mod.add_instr(.add, b.cur_block, i64_t, [counter3, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_counter, counter_ptr])

	// Jump back to loop head
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Continue at exit block
	b.cur_block = exit_blk

	return slice_ptr
}

fn (mut b Builder) expr_infix(node ast.InfixExpr) ValueID {
	i32_t := b.mod.type_store.get_int(64)

	// Handle logical operators: && and ||
	// For simplicity, we use bitwise AND/OR on boolean (0/1) values.
	// This gives correct results when operands are already 0 or 1.
	// True short-circuit evaluation would require control flow.
	if node.op == .and {
		left := b.expr(node.lhs)
		right := b.expr(node.rhs)
		return b.mod.add_instr(.and_, b.cur_block, i32_t, [left, right])
	}

	if node.op == .logical_or {
		left := b.expr(node.lhs)
		right := b.expr(node.rhs)
		return b.mod.add_instr(.or_, b.cur_block, i32_t, [left, right])
	}

	left := b.expr(node.lhs)
	right := b.expr(node.rhs)

	// Map Token Op to SSA OpCode
	op := match node.op {
		.plus { OpCode.add }
		.minus { OpCode.sub }
		.mul { OpCode.mul }
		.div { OpCode.sdiv }
		.mod { OpCode.srem }
		.amp { OpCode.and_ }
		.pipe { OpCode.or_ }
		.xor { OpCode.xor }
		.left_shift { OpCode.shl }
		.right_shift { OpCode.ashr }
		.gt { OpCode.gt }
		.lt { OpCode.lt }
		.eq { OpCode.eq }
		.ne { OpCode.ne }
		.ge { OpCode.ge }
		.le { OpCode.le }
		else { OpCode.add }
	}

	return b.mod.add_instr(op, b.cur_block, i32_t, [left, right])
}

// expr_comptime handles compile-time conditionals like $if macos { ... } $else { ... }
fn (mut b Builder) expr_comptime(node ast.ComptimeExpr) ValueID {
	// The inner expression should be an IfExpr
	if node.expr is ast.IfExpr {
		return b.expr_comptime_if(node.expr)
	}
	// For other comptime expressions, just evaluate them (e.g., $embed_file)
	return b.expr(node.expr)
}

// expr_comptime_if handles $if/$else compile-time conditionals
fn (mut b Builder) expr_comptime_if(node ast.IfExpr) ValueID {
	// Evaluate the comptime condition
	cond_result := b.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - emit then branch
		return b.stmts_with_value(node.stmts)
	} else {
		// Condition is false - emit else branch if present
		if node.else_expr !is ast.EmptyExpr {
			if node.else_expr is ast.IfExpr {
				// Could be $else if or plain $else
				if node.else_expr.cond is ast.EmptyExpr {
					// Plain $else block
					return b.stmts_with_value(node.else_expr.stmts)
				} else {
					// $else $if - recursive comptime evaluation
					return b.expr_comptime_if(node.else_expr)
				}
			}
		}
	}
	// No branch taken - return 0
	i64_t := b.mod.type_store.get_int(64)
	return b.mod.add_value_node(.constant, i64_t, '0', 0)
}

// eval_comptime_cond evaluates a compile-time condition expression
fn (b Builder) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			// Platform and feature flags
			return b.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			// Handle negation: !macos
			if cond.op == .not {
				return !b.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			// Handle && and ||
			if cond.op == .and {
				return b.eval_comptime_cond(cond.lhs) && b.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return b.eval_comptime_cond(cond.lhs) || b.eval_comptime_cond(cond.rhs)
			}
		}
		ast.PostfixExpr {
			// Handle optional feature check: feature?
			if cond.op == .question {
				if cond.expr is ast.Ident {
					return b.eval_comptime_flag(cond.expr.name)
				}
			}
		}
		ast.ParenExpr {
			return b.eval_comptime_cond(cond.expr)
		}
		else {}
	}
	return false
}

// eval_comptime_flag evaluates a single comptime flag/identifier
fn (b Builder) eval_comptime_flag(name string) bool {
	// OS checks - use comptime conditionals with direct returns
	match name {
		'macos', 'darwin' {
			$if macos {
				return true
			}
			return false
		}
		'linux' {
			$if linux {
				return true
			}
			return false
		}
		'windows' {
			$if windows {
				return true
			}
			return false
		}
		'freebsd' {
			$if freebsd {
				return true
			}
			return false
		}
		'posix', 'unix' {
			$if macos {
				return true
			} $else $if linux {
				return true
			} $else $if freebsd {
				return true
			}
			return false
		}
		// Architecture checks
		'amd64', 'x86_64' {
			$if amd64 {
				return true
			}
			return false
		}
		'arm64', 'aarch64' {
			$if arm64 {
				return true
			}
			return false
		}
		'x86' {
			// x86 (32-bit) is rarely used in modern systems
			return false
		}
		// Common feature flags (typically false in simple compilers)
		'freestanding', 'ios', 'android', 'termux', 'debug', 'test' {
			return false
		}
		else {
			// Unknown flag - default to false
			return false
		}
	}
}

fn (mut b Builder) expr_if(node ast.IfExpr) ValueID {
	// If cond is empty, it's a plain 'else' block from a parent IfExpr
	if node.cond is ast.EmptyExpr {
		return b.stmts_with_value(node.stmts)
	}

	// Check if this is an if-guard expression
	if node.cond is ast.IfGuardExpr {
		return b.expr_if_with_guard(node, node.cond)
	}

	// Check if this is an if-expression (has else and branches have values)
	has_else := node.else_expr !is ast.EmptyExpr
	is_expr := has_else && b.branch_has_value(node.stmts)

	// 1. Evaluate Condition
	cond_val := b.expr(node.cond)

	// 2. Create Blocks
	then_blk := b.mod.add_block(b.cur_func, 'if.then')
	merge_blk := b.mod.add_block(b.cur_func, 'if.end')
	mut else_blk := merge_blk

	if has_else {
		else_blk = b.mod.add_block(b.cur_func, 'if.else')
	}

	// 3. For if-expressions, allocate result storage
	i32_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i32_t)
	mut result_ptr := ValueID(0)
	if is_expr {
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	}

	// 4. Emit Branch
	then_val := b.mod.blocks[then_blk].val_id
	else_val := b.mod.blocks[else_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, then_val, else_val])

	// 5. Build Then Block
	b.cur_block = then_blk
	then_result := b.stmts_with_value(node.stmts)
	if is_expr && then_result != 0 && !b.is_block_terminated(b.cur_block) {
		b.mod.add_instr(.store, b.cur_block, 0, [then_result, result_ptr])
	}
	if !b.is_block_terminated(b.cur_block) {
		merge_val := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
	}

	// 6. Build Else Block (if any)
	if has_else {
		b.cur_block = else_blk
		else_result := b.expr_if_else(node.else_expr)
		if is_expr && else_result != 0 && !b.is_block_terminated(b.cur_block) {
			b.mod.add_instr(.store, b.cur_block, 0, [else_result, result_ptr])
		}
		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	// 7. Continue generation at Merge Block
	b.cur_block = merge_blk

	// 8. Load result for if-expressions
	if is_expr && result_ptr != 0 {
		return b.mod.add_instr(.load, b.cur_block, i32_t, [result_ptr])
	}
	return 0
}

// expr_if_with_guard handles if statements with guard expressions
// `if x := opt() { ... } else { ... }`
fn (mut b Builder) expr_if_with_guard(node ast.IfExpr, guard ast.IfGuardExpr) ValueID {
	has_else := node.else_expr !is ast.EmptyExpr
	is_expr := has_else && b.branch_has_value(node.stmts)

	i32_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i32_t)

	// 1. Evaluate the RHS of the guard (the optional expression)
	stmt := guard.stmt
	mut rhs_val := ValueID(0)
	if stmt.rhs.len > 0 {
		rhs_val = b.expr(stmt.rhs[0])
	}

	// 2. For now, use the value directly as condition
	// In a full optional implementation, we'd check the error flag
	// For simplicity, we treat non-zero as success
	cond_val := rhs_val

	// 3. Create Blocks
	then_blk := b.mod.add_block(b.cur_func, 'if.then')
	merge_blk := b.mod.add_block(b.cur_func, 'if.end')
	mut else_blk := merge_blk

	if has_else {
		else_blk = b.mod.add_block(b.cur_func, 'if.else')
	}

	// 4. For if-expressions, allocate result storage
	mut result_ptr := ValueID(0)
	if is_expr {
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	}

	// 5. Emit Branch
	then_val := b.mod.blocks[then_blk].val_id
	else_val := b.mod.blocks[else_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, then_val, else_val])

	// 6. Build Then Block - bind guard variables here
	b.cur_block = then_blk

	// Bind the guard variables (x := ... binds x)
	b.expr_if_guard_bind(guard, rhs_val)

	then_result := b.stmts_with_value(node.stmts)
	if is_expr && then_result != 0 && !b.is_block_terminated(b.cur_block) {
		b.mod.add_instr(.store, b.cur_block, 0, [then_result, result_ptr])
	}
	if !b.is_block_terminated(b.cur_block) {
		merge_val := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
	}

	// 7. Build Else Block (if any)
	if has_else {
		b.cur_block = else_blk
		else_result := b.expr_if_else(node.else_expr)
		if is_expr && else_result != 0 && !b.is_block_terminated(b.cur_block) {
			b.mod.add_instr(.store, b.cur_block, 0, [else_result, result_ptr])
		}
		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	// 8. Continue generation at Merge Block
	b.cur_block = merge_blk

	// 9. Load result for if-expressions
	if is_expr && result_ptr != 0 {
		return b.mod.add_instr(.load, b.cur_block, i32_t, [result_ptr])
	}
	return 0
}

// expr_if_guard handles if-guard expressions: `if x := opt() {`
// When used as a condition, it evaluates the RHS and checks for success
// Returns a boolean value (0 = failure/none, 1 = success)
fn (mut b Builder) expr_if_guard(node ast.IfGuardExpr) ValueID {
	i32_t := b.mod.type_store.get_int(64)

	// Get the assignment statement
	stmt := node.stmt

	// For if-guard, we evaluate the RHS expression
	// In V, the RHS returns an optional type that we need to unwrap
	// For now, we assume any non-zero/non-none value is success
	if stmt.rhs.len == 0 {
		// No RHS - always false
		return b.mod.add_value_node(.constant, i32_t, '0', 0)
	}

	rhs_val := b.expr(stmt.rhs[0])

	// For the condition check, we need to determine if the optional succeeded
	// In a full implementation, this would check the error flag of the optional
	// For now, we'll assume the value itself indicates success (non-zero = success)
	// TODO: Proper optional type handling with error flags

	// Return the condition value (will be used in branch)
	// For now, return 1 (true) - actual unwrapping happens in expr_if when
	// the condition is an IfGuardExpr
	return rhs_val
}

// expr_if_guard_bind binds the variables from an if-guard expression
// This is called after the condition check succeeds
fn (mut b Builder) expr_if_guard_bind(node ast.IfGuardExpr, rhs_val ValueID) {
	stmt := node.stmt

	// Bind each LHS variable to the unwrapped value
	for i, lhs_expr in stmt.lhs {
		mut ident := ast.Ident{}

		// Unwrap 'mut x' if present
		if lhs_expr is ast.ModifierExpr {
			mod := lhs_expr as ast.ModifierExpr
			ident = mod.expr as ast.Ident
		} else if lhs_expr is ast.Ident {
			ident = lhs_expr
		} else {
			continue
		}

		name := ident.name

		// Get type from RHS value
		rhs_type := b.mod.values[rhs_val].typ
		ptr_t := b.mod.type_store.get_ptr(rhs_type)

		// Allocate stack slot for the variable
		stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

		// For single-value assignment, use the rhs_val directly
		// For multi-value (like tuple unpacking), we'd need to extract individual values
		if i == 0 {
			b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, stack_ptr])
		}

		// Register the variable
		b.vars[name] = stack_ptr
	}
}

// Helper to check if a branch has a value (last stmt is an expression)
fn (b Builder) branch_has_value(stmts []ast.Stmt) bool {
	if stmts.len == 0 {
		return false
	}
	last := stmts[stmts.len - 1]
	return last is ast.ExprStmt
}

// Process else expression which can be another IfExpr or statements
fn (mut b Builder) expr_if_else(else_expr ast.Expr) ValueID {
	if else_expr is ast.IfExpr {
		return b.expr_if(else_expr)
	}
	return 0
}

// Process statements and return the value of the last expression if any
fn (mut b Builder) stmts_with_value(stmts []ast.Stmt) ValueID {
	if stmts.len == 0 {
		return 0
	}
	// Process all but the last statement
	for i := 0; i < stmts.len - 1; i++ {
		b.stmt(stmts[i])
	}
	// Process the last statement and return its value if it's an expression
	last := stmts[stmts.len - 1]
	if last is ast.ExprStmt {
		return b.expr(last.expr)
	}
	b.stmt(last)
	return 0
}

fn (mut b Builder) expr_match(node ast.MatchExpr) ValueID {
	// 1. Eval Cond
	cond_val := b.expr(node.expr)

	// Try to infer the match expression type for enum shorthand
	old_match_type := b.cur_match_type
	if node.expr is ast.Ident {
		// Check if this variable has an enum type
		if var_type := b.var_struct_types[node.expr.name] {
			// If it's an enum type, set the context
			b.cur_match_type = var_type
		}
	} else if node.expr is ast.SelectorExpr {
		if node.expr.lhs is ast.Ident {
			// Check if LHS is an enum name
			for key, _ in b.enum_values {
				if key.starts_with('${node.expr.lhs.name}__') {
					b.cur_match_type = node.expr.lhs.name
					break
				}
			}
		}
	}

	// 2. Setup Blocks
	merge_blk := b.mod.add_block(b.cur_func, 'match.merge')
	mut default_blk := merge_blk

	// We need to collect all cases and branch blocks
	// Format: val -> block
	// Ops: [cond, default_blk, val1, blk1, val2, blk2...]

	mut cases := []ValueID{} // alternating val, blk_id

	// Pre-create blocks for branches to get their IDs
	mut branch_blks := []BlockID{}
	for i, branch in node.branches {
		name := if branch.cond.len == 0 { 'match.else' } else { 'match.case_${i}' }
		blk := b.mod.add_block(b.cur_func, name)
		branch_blks << blk

		if branch.cond.len == 0 {
			default_blk = blk
		} else {
			for expr in branch.cond {
				val := b.expr(expr)
				cases << val
				cases << b.mod.blocks[blk].val_id
			}
		}
	}

	// 3. Emit switch
	mut ops := []ValueID{}
	ops << cond_val
	ops << b.mod.blocks[default_blk].val_id
	ops << cases

	b.mod.add_instr(.switch_, b.cur_block, 0, ops)

	// 4. Build Branches
	for i, branch in node.branches {
		blk := branch_blks[i]
		b.cur_block = blk
		b.stmts(branch.stmts)

		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	b.cur_block = merge_blk
	b.cur_match_type = old_match_type
	return 0
}

fn (mut b Builder) expr_call(node ast.CallExpr) ValueID {
	// Resolve Args
	mut args := []ValueID{}
	for arg in node.args {
		args << b.expr(arg)
	}
	// Resolve Function Name
	mut name := ''
	mut is_method_call := false
	mut receiver_val := ValueID(0)

	lhs := node.lhs
	if lhs is ast.Ident {
		name = lhs.name
	} else if lhs is ast.SelectorExpr {
		method_name := lhs.rhs.name
		// Check if this is a method call (receiver.method()) or C.func()
		if lhs.lhs is ast.Ident {
			receiver_name := lhs.lhs.name
			// Check if receiver is 'C' (C interop)
			if receiver_name == 'C' {
				name = method_name
			} else if struct_type_name := b.var_struct_types[receiver_name] {
				// This is a method call - mangle the name
				name = '${struct_type_name}_${method_name}'
				is_method_call = true
				// Get the receiver - need to load the struct pointer from the variable
				// b.vars stores Ptr(Ptr(struct)), we need Ptr(struct)
				var_ptr := b.addr(lhs.lhs)
				ptr_typ := b.mod.values[var_ptr].typ
				elem_typ := b.mod.type_store.types[ptr_typ].elem_type
				receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ, [var_ptr])
			} else {
				// Unknown - just use method name
				name = method_name
			}
		} else {
			// Complex expression as receiver - try to evaluate
			name = method_name
		}
	}

	// For method calls, prepend receiver as first argument
	if is_method_call && receiver_val != 0 {
		args.prepend(receiver_val)
	}

	// Create a Value representing the function symbol (operand 0)
	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	args.prepend(fn_val)
	// For this demo, assuming ret type i32
	i32_t := b.mod.type_store.get_int(64)
	// Note: In real compiler, we need to lookup Function ID by name to get correct ret type
	return b.mod.add_instr(.call, b.cur_block, i32_t, args)
}

fn (mut b Builder) expr_string_literal(node ast.StringLiteral) ValueID {
	// Strip quotes from value (the parser includes them)
	val := node.value.trim("'").trim('"')

	// Check for C-string literal using the kind field
	if node.kind == .c {
		// C string: just return char* pointer
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.add_value_node(.constant, ptr_t, '"${val}"', 0)
	}

	// V string: create a string struct literal
	// The string_literal value represents a pointer to a stack-allocated string struct
	// (ARM64 backend creates the struct on stack and returns the pointer)
	string_type_id := b.struct_types['string']
	string_ptr_t := b.mod.type_store.get_ptr(string_type_id)
	return b.mod.add_value_node(.string_literal, string_ptr_t, val, val.len)
}

fn (mut b Builder) expr_string_inter_literal(node ast.StringInterLiteral) ValueID {
	// String interpolation: 'prefix${a}middle${b}suffix'
	// Lower to: sprintf(buf, "prefix%lldmiddle%lldsuffix", a, b)
	// Then wrap in a string struct using statement expression
	//
	// For now, use libc sprintf. Later this can use strconv functions.

	i8_t := b.mod.type_store.get_int(8)
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i8_t)

	// 1. Build the format string and collect argument values
	mut format_str := ''
	mut args := []ValueID{}

	for i, val in node.values {
		// Add the literal string part (strip quotes from first/last parts)
		mut clean_val := val
		if i == 0 {
			clean_val = clean_val.trim_left("'").trim_left('"')
		}
		if i == node.values.len - 1 {
			clean_val = clean_val.trim_right("'").trim_right('"')
		}
		format_str += clean_val

		// Add format specifier and argument for interpolation
		if i < node.inters.len {
			inter := node.inters[i]
			// Evaluate the interpolated expression
			arg_val := b.expr(inter.expr)
			args << arg_val

			// Determine format specifier based on format type
			format_str += b.get_printf_format(inter)
		}
	}

	// 2. Allocate buffer on stack (256 bytes should be enough for most strings)
	buf_size := 256
	array_type := b.mod.type_store.get_array(i8_t, buf_size)
	array_ptr_t := b.mod.type_store.get_ptr(array_type)
	buf_ptr := b.mod.add_instr(.alloca, b.cur_block, array_ptr_t, [])

	// 3. Create format string constant
	format_val := b.mod.add_value_node(.constant, ptr_t, '"${format_str}"', 0)

	// 4. Call sprintf(buf, format, args...)
	sprintf_fn := b.mod.add_value_node(.unknown, 0, 'sprintf', 0)
	mut call_args := []ValueID{}
	call_args << sprintf_fn
	call_args << buf_ptr
	call_args << format_val
	for arg in args {
		call_args << arg
	}
	b.mod.add_instr(.call, b.cur_block, i64_t, call_args)

	// 5. Call strlen to get the length
	strlen_fn := b.mod.add_value_node(.unknown, 0, 'strlen', 0)
	strlen_result := b.mod.add_instr(.call, b.cur_block, i64_t, [strlen_fn, buf_ptr])

	// 6. Create string struct by value using inline_string_init instruction
	// This will generate: (string){buf, strlen_result, 0}
	// Return pointer type (like string_literal) so ARM64 backend handles it correctly
	string_type_id := b.struct_types['string']
	string_ptr_t := b.mod.type_store.get_ptr(string_type_id)
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	return b.mod.add_instr(.inline_string_init, b.cur_block, string_ptr_t, [
		buf_ptr,
		strlen_result,
		zero,
	])
}

fn (b Builder) get_printf_format(inter ast.StringInter) string {
	// Convert V format specifier to printf format specifier
	// For now, default to %lld for integers (64-bit)
	base_fmt := match inter.format {
		.unformatted { '%lld' } // Default: assume integer
		.decimal { '%lld' }
		.hex { '%llx' }
		.octal { '%llo' }
		.binary { '%lld' } // C doesn't have binary, use decimal
		.float { '%f' }
		.exponent { '%e' }
		.exponent_short { '%g' }
		.character { '%c' }
		.string { '%s' }
		.pointer_address { '%p' }
	}

	// Handle width and precision if specified
	if inter.width > 0 && inter.precision > 0 {
		// Both width and precision specified
		return '%${inter.width}.${inter.precision}' + base_fmt[1..]
	} else if inter.width > 0 {
		return '%${inter.width}' + base_fmt[1..]
	} else if inter.precision > 0 {
		return '%.${inter.precision}' + base_fmt[1..]
	}
	return base_fmt
}

fn (mut b Builder) expr_call_or_cast(node ast.CallOrCastExpr) ValueID {
	// Check if this is a primitive type cast (int, i64, etc.)
	// These are not function calls - just return the expression value
	if node.lhs is ast.Ident {
		cast_name := node.lhs.name
		if cast_name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'f32', 'f64'] {
			// Type cast: just evaluate the expression (enums are already ints in SSA)
			return b.expr(node.expr)
		}
	}

	// Handle ambiguous calls like print_int(1111)
	mut args := []ValueID{}
	args << b.expr(node.expr)

	mut name := ''
	mut is_method_call := false
	mut receiver_val := ValueID(0)

	if node.lhs is ast.Ident {
		name = node.lhs.name
	} else if node.lhs is ast.SelectorExpr {
		method_name := node.lhs.rhs.name
		// Check if this is a method call (receiver.method()) or C.func()
		if node.lhs.lhs is ast.Ident {
			receiver_name := node.lhs.lhs.name
			if receiver_name == 'C' {
				name = method_name
			} else if struct_type_name := b.var_struct_types[receiver_name] {
				// This is a method call - mangle the name
				name = '${struct_type_name}_${method_name}'
				is_method_call = true
				// Get the receiver - need to load the struct pointer from the variable
				var_ptr := b.addr(node.lhs.lhs)
				ptr_typ := b.mod.values[var_ptr].typ
				elem_typ := b.mod.type_store.types[ptr_typ].elem_type
				receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ, [var_ptr])
			} else {
				name = method_name
			}
		} else {
			name = method_name
		}
	}

	// For method calls, prepend receiver as first argument
	if is_method_call && receiver_val != 0 {
		args.prepend(receiver_val)
	}

	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	args.prepend(fn_val)
	i32_t := b.mod.type_store.get_int(64)
	return b.mod.add_instr(.call, b.cur_block, i32_t, args)
}

fn (mut b Builder) expr_prefix(node ast.PrefixExpr) ValueID {
	i32_t := b.mod.type_store.get_int(64)

	// Handle address-of operator with struct init: &Point{} -> heap allocation
	if node.op == .amp {
		if node.expr is ast.InitExpr {
			return b.expr_heap_alloc(node.expr)
		}
		// For other &expr cases, just return the address
		return b.addr(node.expr)
	}

	right := b.expr(node.expr)
	match node.op {
		.minus {
			zero := b.mod.add_value_node(.constant, i32_t, '0', 0)
			return b.mod.add_instr(.sub, b.cur_block, i32_t, [zero, right])
		}
		.not {
			zero := b.mod.add_value_node(.constant, i32_t, '0', 0)
			return b.mod.add_instr(.eq, b.cur_block, i32_t, [right, zero])
		}
		else {
			return 0
		}
	}
}

fn (mut b Builder) expr_range(node ast.RangeExpr) ValueID {
	// RangeExpr represents a range like 0..10 or 0...10
	// We create a small struct with (start, end) values
	i64_t := b.mod.type_store.get_int(64)

	// Evaluate start and end expressions
	start_val := b.expr(node.start)
	end_val := b.expr(node.end)

	// Create a range type (struct with 2 int64 fields)
	// Check if we already have a range type registered
	mut range_t := TypeID(0)
	for i, t in b.mod.type_store.types {
		if t.kind == .struct_t && t.field_names.len == 2 && t.field_names[0] == '_range_start' {
			range_t = i
			break
		}
	}
	if range_t == 0 {
		// Register new range type
		t := Type{
			kind:        .struct_t
			fields:      [i64_t, i64_t]
			field_names: ['_range_start', '_range_end']
			width:       0
		}
		range_t = b.mod.type_store.register(t)
	}

	ptr_t := b.mod.type_store.get_ptr(range_t)
	range_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

	// Store start value at field 0
	idx0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	start_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(i64_t),
		[range_ptr, idx0])
	b.mod.add_instr(.store, b.cur_block, 0, [start_val, start_ptr])

	// Store end value at field 1
	idx1 := b.mod.add_value_node(.constant, i64_t, '1', 0)
	end_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(i64_t),
		[range_ptr, idx1])
	b.mod.add_instr(.store, b.cur_block, 0, [end_val, end_ptr])

	return range_ptr
}

fn (mut b Builder) expr_array_init(node ast.ArrayInitExpr) ValueID {
	// Array Init: [1, 2, 3] or []int{len: 10, cap: 20, init: 0}
	i64_t := b.mod.type_store.get_int(64)

	// Determine element count
	mut elem_count := node.exprs.len

	// Check if this is a sized array initialization (len: expr)
	if node.len !is ast.EmptyExpr {
		// For []T{len: n}, we need to evaluate the length expression
		len_val := b.expr(node.len)
		// For now, only support constant lengths
		if b.mod.values[len_val].kind == .constant {
			elem_count = b.mod.values[len_val].name.int()
		}
	}

	if elem_count == 0 {
		// Empty array - return null pointer for now
		return b.mod.add_value_node(.constant, b.mod.type_store.get_ptr(i64_t), '0', 0)
	}

	// Create array type
	array_type := b.mod.type_store.get_array(i64_t, elem_count)
	array_ptr_t := b.mod.type_store.get_ptr(array_type)
	elem_ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Allocate array on stack
	array_ptr := b.mod.add_instr(.alloca, b.cur_block, array_ptr_t, [])

	// Initialize elements
	if node.exprs.len > 0 {
		// Literal array: [1, 2, 3]
		for i, elem_expr in node.exprs {
			// Evaluate element expression
			elem_val := b.expr(elem_expr)

			// Compute element address using GEP
			idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
			elem_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
				array_ptr,
				idx_val,
			])

			// Store element value
			b.mod.add_instr(.store, b.cur_block, 0, [elem_val, elem_ptr])
		}
	} else if node.init !is ast.EmptyExpr {
		// Sized array with init value: []int{len: 10, init: 0}
		init_val := b.expr(node.init)

		// Initialize all elements with the init value
		for i in 0 .. elem_count {
			idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
			elem_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
				array_ptr,
				idx_val,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [init_val, elem_ptr])
		}
	} else {
		// Sized array without init: zero-initialize
		zero_val := b.mod.add_value_node(.constant, i64_t, '0', 0)

		for i in 0 .. elem_count {
			idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
			elem_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
				array_ptr,
				idx_val,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [zero_val, elem_ptr])
		}
	}

	// Return the array pointer
	return array_ptr
}

fn (mut b Builder) expr_heap_alloc(node ast.InitExpr) ValueID {
	// Heap allocation: &StructInit{}
	// 1. Find struct type
	mut struct_t := 0

	if node.typ is ast.Ident {
		if st := b.struct_types[node.typ.name] {
			struct_t = st
		}
	}

	if struct_t == 0 {
		for i, t in b.mod.type_store.types {
			if t.kind == .struct_t {
				struct_t = i
				break
			}
		}
	}

	// 2. Calculate size (fields.len * 8 for 64-bit)
	struct_type := b.mod.type_store.types[struct_t]
	mut size := struct_type.fields.len * 8
	if size == 0 {
		size = 16 // Default size for empty struct
	}

	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(struct_t)

	// 3. Call malloc
	malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
	size_val := b.mod.add_value_node(.constant, i64_t, size.str(), 0)
	heap_ptr := b.mod.add_instr(.call, b.cur_block, ptr_t, [malloc_fn, size_val])

	// 4. Initialize fields
	for i, field in node.fields {
		val := b.expr(field.value)
		idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)

		// GEP to field
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(i64_t),
			[heap_ptr, idx_val])
		b.mod.add_instr(.store, b.cur_block, 0, [val, field_ptr])
	}

	return heap_ptr
}

fn (mut b Builder) expr_postfix(node ast.PostfixExpr) ValueID {
	// Handle i++ / i--
	if node.expr is ast.Ident {
		name := (node.expr as ast.Ident).name
		if ptr := b.vars[name] {
			if ptr != 0 {
				i32_t := b.mod.type_store.get_int(64)

				// 1. Load current value
				old_val := b.mod.add_instr(.load, b.cur_block, i32_t, [ptr])

				// 2. Add/Sub 1
				one := b.mod.add_value_node(.constant, i32_t, '1', 0)
				op := if node.op == .inc { OpCode.add } else { OpCode.sub }
				new_val := b.mod.add_instr(op, b.cur_block, i32_t, [old_val, one])

				// 3. Store new value
				b.mod.add_instr(.store, b.cur_block, 0, [new_val, ptr])

				// Postfix returns the old value
				return old_val
			}
		}
	}
	return 0
}

fn (b Builder) is_block_terminated(blk_id int) bool {
	if blk_id >= b.mod.blocks.len {
		return false
	}
	blk := b.mod.blocks[blk_id]
	if blk.instrs.len == 0 {
		return false
	}

	last_val_id := blk.instrs.last()
	val := b.mod.values[last_val_id]
	if val.kind != .instruction {
		return false
	}

	instr := b.mod.instrs[val.index]
	return instr.op in [.ret, .br, .jmp, .unreachable]
}

// addr returns the ValueID (pointer) representing the L-Value of an expression
fn (mut b Builder) addr(node ast.Expr) ValueID {
	match node {
		ast.Ident {
			// Check locals
			if ptr := b.vars[node.name] {
				// FIX: Ensure it is a valid ID (0 is invalid now)
				if ptr != 0 {
					return ptr
				}
			}
			// Check globals
			for g in b.mod.globals {
				if g.name == node.name {
					// Globals are values in the values arena but effectively pointers
					// We need to find the ValueID that corresponds to this global
					// For this demo, we iterate values to find it (slow, but works)
					for v in b.mod.values {
						if v.kind == .global && v.name == node.name {
							return v.id
						}
					}
				}
			}
			return 0
		}
		ast.SelectorExpr {
			// struct.field
			base_ptr := b.addr(node.lhs)

			// Resolve the type of the base pointer
			base_val := b.mod.values[base_ptr]
			mut ptr_typ := b.mod.type_store.types[base_val.typ]

			// We expect ptr_typ to be Ptr -> (Struct) OR Ptr -> (Ptr -> Struct)
			// If it's Ptr -> Ptr -> ..., we must Load to get the actual struct pointer.

			// Unpack one level of pointer (the variable address)
			mut val_typ_id := ptr_typ.elem_type
			mut val_typ := b.mod.type_store.types[val_typ_id]

			mut actual_base := base_ptr

			// Check if the value stored is a pointer (Reference semantics for variable)
			if val_typ.kind == .ptr_t {
				// Load the pointer value
				actual_base = b.mod.add_instr(.load, b.cur_block, val_typ_id, [
					base_ptr,
				])

				// Update types for the loaded value
				// actual_base is now Ptr -> Struct
				ptr_typ = val_typ
				val_typ_id = ptr_typ.elem_type
				val_typ = b.mod.type_store.types[val_typ_id]
			}

			// Now val_typ should be the Struct
			if val_typ.kind != .struct_t {
				// Fallback or error. For now, try to proceed, but it might panic if we access fields.
				// In a real compiler, this checks if it's a struct.
			}

			// Find field index by name
			mut idx := -1
			for i, name in val_typ.field_names {
				if name == node.rhs.name {
					idx = i
					break
				}
			}
			if idx == -1 {
				// Fallback to old behavior for backwards compatibility
				idx = 0
				if node.rhs.name == 'y' || node.rhs.name == 'b' {
					idx = 1
				}
			}

			// Safety check for index
			if idx >= val_typ.fields.len {
				// TODO: The builder currently maps all function parameters to int64.
				// This workaround allows accessing the first field of such "opaque" types
				// (like s.str where s is int64) by returning the base address, as field 0 is at
				if idx == 0 && val_typ.kind != .struct_t {
					return actual_base
				}
				// If fields are empty (e.g. type resolution failed), prevent panic
				// Return a dummy value or handle error
				println('SSA Error: Struct fields empty or index out of bounds')
				return 0
			}

			idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), idx.str(),
				0)

			// GEP
			field_ptr_t := b.mod.type_store.get_ptr(val_typ.fields[idx])
			return b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_t, [
				actual_base,
				idx_val,
			])
		}
		ast.IndexExpr {
			// array[index]
			base_ptr := b.addr(node.lhs)
			index_val := b.expr(node.expr)

			// Auto-dereference if it's a pointer-to-pointer (variable holding array ptr)
			base_val := b.mod.values[base_ptr]
			ptr_typ := b.mod.type_store.types[base_val.typ]
			elem_typ_id := ptr_typ.elem_type
			elem_typ := b.mod.type_store.types[elem_typ_id]

			mut actual_base := base_ptr

			if elem_typ.kind == .ptr_t {
				actual_base = b.mod.add_instr(.load, b.cur_block, elem_typ_id, [
					base_ptr,
				])
			}

			return b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.values[actual_base].typ,
				[
				actual_base,
				index_val,
			])
		}
		else {
			return 0
		}
	}
}

// emit_deferred_stmts emits all deferred statements in reverse order
fn (mut b Builder) emit_deferred_stmts() {
	// Execute deferred statements in LIFO order (last defer first)
	for i := b.defer_stmts.len - 1; i >= 0; i-- {
		b.stmts(b.defer_stmts[i])
	}
}

// eval_const_expr evaluates a constant expression at compile time
fn (mut b Builder) eval_const_expr(expr ast.Expr) i64 {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .number {
				return expr.value.i64()
			}
		}
		ast.PrefixExpr {
			if expr.op == .minus {
				return -b.eval_const_expr(expr.expr)
			}
		}
		ast.InfixExpr {
			lhs := b.eval_const_expr(expr.lhs)
			rhs := b.eval_const_expr(expr.rhs)
			return match expr.op {
				.plus { lhs + rhs }
				.minus { lhs - rhs }
				.mul { lhs * rhs }
				.div { lhs / rhs }
				.mod { lhs % rhs }
				else { 0 }
			}
		}
		ast.ParenExpr {
			return b.eval_const_expr(expr.expr)
		}
		else {}
	}
	return 0
}
