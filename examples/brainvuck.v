import os
import term

// For a more detailed description of the brainfuck language, see:
// https://en.wikipedia.org/wiki/Brainfuck
// http://brainfuck.org/brainfuck.html ,
// http://brainfuck.org/epistle.html ,
// http://www.hevanet.com/cristofd/brainfuck/ .

const show_state = os.getenv('VERBOSE') != ''

struct BFState {
mut:
	pc      u16    // program counter (PC) register
	address u16    // a 16-bit address register, serving as an index to the memory below
	program string // the BF program
	memory  []u8 = []u8{len: 65536} // we have 2^16 bytes of memory
	targets map[int]int // a mapping for the program address of a `[` to its corresponding `]`, and from a `]` to its corresponding opening `[`.
}

fn BFState.new(program string) &BFState {
	mut state := &BFState{
		program: program
	}
	state.find_matching_pairs()
	return state
}

// show the current state of an BF interpreter. Useful for debugging.
fn (state &BFState) show() {
	println('PC: ${state.pc}')
	println('Address: ${state.address}')
	mut max_non_zero_address := -1
	for i := state.memory.len - 1; i >= 0; i-- {
		if state.memory[i] != 0 {
			max_non_zero_address = i
			break
		}
	}
	println('Memory: ${state.memory#[0..max_non_zero_address + 1]}')
	println('Memory[Address]: ${state.memory#[state.address..state.address + 1]}')
}

// find_matching_pairs fills in the `targets` mapping for all pairs of `[` and `]`,
// so that when interpreting, we would not have to search for them anymore.
fn (mut state BFState) find_matching_pairs() {
	mut stack := []int{}
	for i in 0 .. state.program.len {
		pi := state.program[i]
		match pi {
			`[` {
				stack << i
			}
			`]` {
				if stack.len == 0 {
					eprintln('> unmatched `]` found in the program, at position: ${i}')
					eprintln('program so far:')
					eprintln(state.program#[0..i + 1])
					exit(1)
				}
				pc := stack.pop()
				state.targets[pc] = i + 1
				state.targets[i] = pc + 1
				// eprintln('>>> found `[` at i $i; pc: $pc')
			}
			else {}
		}
	}
	if stack.len > 0 {
		eprintln('> found ${stack.len} unmatched `[`:')
		for i in stack {
			eprintln('  `[` at position: ${i}, program so far: `${state.program#[0..i + 1]}`')
		}
		exit(1)
	}
}

@[noreturn]
fn (state &BFState) panic_for_bracket(b1 rune, b2 rune) {
	panic('unbalanced `${b1}` found, its target `${b2}` is not known; address: ${state.address}, pc: ${state.pc}')
}

fn (mut state BFState) run() ? {
	// the BF interpreter starts here:
	for state.pc < state.program.len {
		// get the current program character (corresponding to our program counter), and interpret it according to BF's rules:
		match state.program[state.pc] {
			`>` {
				state.address++ // increment the address
			}
			`<` {
				state.address-- // decrement the address
			}
			`+` {
				state.memory[state.address]++ // increment the value at the address
			}
			`-` {
				state.memory[state.address]-- // decrement the value at the address
			}
			`.` {
				print(rune(state.memory[state.address])) // print the value at the address
				flush_stdout() // ensure that even single characters are printed immediately, and not buffered
			}
			`,` {
				inp := u8(term.utf8_getchar() or { 0 }) // read a character value from the standard input/terminal
				state.memory[state.address] = inp
			}
			`[` {
				if state.memory[state.address] == 0 {
					state.pc = u16(state.targets[state.pc])
					continue
				}
			}
			`]` {
				if state.memory[state.address] != 0 {
					state.pc = u16(state.targets[state.pc])
					continue
				}
			}
			`#` {
				state.show()
			}
			else {
				// The interpreter should ignore characters that are not part of the language.
				// I.e. they are treated like programmer comments.
			}
		}
		// increment the program counter to go to the next instruction
		state.pc++
		// go back to the line `for state.pc < state.program.len {`
	}
}

@[noreturn]
fn show_usage() {
	eprintln('you need to supply a brainfuck program/expression as a string argument,')
	eprintln('or filename.b, if it is located in a file (note the `.b` extension).')
	exit(1) // exit with non-zero exit code if there is no program to run
}

fn main() {
	if os.args.len < 2 {
		show_usage()
	}
	mut program := os.args[1] // our program is fed in as a string
	if program.ends_with('.b') {
		program = os.read_file(program) or {
			eprintln('error reading file ${program}: ${err}')
			show_usage()
		}
	}

	mut state := BFState.new(program)
	state.run()

	if show_state {
		state.show()
	}
}
