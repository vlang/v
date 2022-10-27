import os

fn main() {
	if os.args.len < 2 {
		eprintln('you need to supply a brainfuck program as a string argument')
		exit(1) // exit with non-zero exit code if there is no program to run
	}
	program := os.args[1] // our program is fed in as a string

	mut memory := []u8{len: 256} // we have 256 bytes of memory
	mut address := u8(0) // as well as an 8-bit address register

	mut stack := []int{} // our stack does not need a maximum length

	mut program_counter := 0 // program counter

	// interpreter starts here
	for program_counter < program.len {
		// we look at what the current character our program counter is seeing
		match program[program_counter] {
			`>` {
				address++ // increment the address
			}
			`<` {
				address-- // decrement the address
			}
			`+` {
				memory[address]++ // increment the value at the address
			}
			`-` {
				memory[address]-- // decrement the value at the address
			}
			`.` {
				print(memory[address].ascii_str()) // print the value at the address
			}
			`,` {
				input := os.input_opt('') or { '' } // read value and account for errors
				memory[address] = input[0] // this is so we can ignore newlines
				// because strings are 0-terminated, it also gives us a default value for free!
			}
			`[` {
				stack << program_counter // add loop start address to the call stack
			}
			`]` {
				if memory[address] != 0 {
					// set the program counter to the last loop start
					// so it jumps back and loops again
					program_counter = stack[stack.len - 1]
				} else {
					// otherwise remove the address from the stack and continue
					stack.pop()
				}
			}
			else {
				// the interpreter should ignore characters that are not part of the language
			}
		}
		// increment the program counter to go to the next instruction
		program_counter++
		// back to line 20!
	}

	// print the state of the interpreter at the end
	println('Address: $address')
	println('Memory: $memory')
}
