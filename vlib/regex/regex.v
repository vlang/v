/*
regex 1.0 alpha

Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains regex module

Know limitation:
- find is implemented in a trivial way
- not full compliant PCRE
- not compliant POSIX ERE
*/
module regex

import strings

pub const (
	v_regex_version          = '1.0 alpha' // regex module version

	max_code_len             = 256 // default small base code len for the regex programs
	max_quantifier           = 1073741824 // default max repetitions allowed for the quantifiers = 2^30
	// spaces chars (here only westerns!!) TODO: manage all the spaces from unicode
	spaces                   = [` `, `\t`, `\n`, `\r`, `\v`, `\f`]
	// new line chars for now only '\n'
	new_line_list            = [`\n`, `\r`]

	// Results
	no_match_found           = -1

	// Errors
	compile_ok               = 0 // the regex string compiled, all ok
	err_char_unknown         = -2 // the char used is unknow to the system
	err_undefined            = -3 // the compiler symbol is undefined
	err_internal_error       = -4 // Bug in the regex system!!
	err_cc_alloc_overflow    = -5 // memory for char class full!!
	err_syntax_error         = -6 // syntax error in regex compiling
	err_groups_overflow      = -7 // max number of groups reached
	err_groups_max_nested    = -8 // max number of nested group reached
	err_group_not_balanced   = -9 // group not balanced
	err_group_qm_notation    = -10 // group invalid notation
	err_invalid_or_with_cc   = -11 // invalid or on two consecutive char class
	err_neg_group_quantifier = -12 // negation groups can not have quantifier
	err_consecutive_dots     = -13 // two consecutive dots is an error
)

const (
	//*************************************
	// regex program instructions
	//*************************************
	ist_simple_char    = u32(0x7FFFFFFF) // single char instruction, 31 bit available to char
	// char class 11 0100 AA xxxxxxxx
	// AA = 00  regular class
	// AA = 01  Negated class ^ char
	ist_char_class     = u32(0xD1000000) // MASK
	ist_char_class_pos = u32(0xD0000000) // char class normal [abc]
	ist_char_class_neg = u32(0xD1000000) // char class negate [^abc]
	// dot char        10 0110 xx xxxxxxxx
	ist_dot_char       = u32(0x98000000) // match any char except \n
	// backslash chars 10 0100 xx xxxxxxxx
	ist_bsls_char      = u32(0x90000000) // backslash char
	// OR |            10 010Y xx xxxxxxxx
	ist_or_branch      = u32(0x91000000) // OR case
	// groups          10 010Y xx xxxxxxxx
	ist_group_start    = u32(0x92000000) // group start (
	ist_group_end      = u32(0x94000000) // group end   )
	// control instructions
	ist_prog_end       = u32(0x88000000) // 10 0010 xx xxxxxxxx
		//*************************************
)

/*
General Utilities
*/
// utf8util_char_len calculate the length in bytes of a utf8 char
[inline]
fn utf8util_char_len(b u8) int {
	return ((0xe5000000 >> ((b >> 3) & 0x1e)) & 3) + 1
}

// get_char get a char from position i and return an u32 with the unicode code
[direct_array_access; inline]
fn (re RE) get_char(in_txt string, i int) (u32, int) {
	ini := unsafe { in_txt.str[i] }
	// ascii 8 bit
	if (re.flag & regex.f_bin) != 0 || ini & 0x80 == 0 {
		return u32(ini), 1
	}
	// unicode char
	char_len := utf8util_char_len(ini)
	mut tmp := 0
	mut ch := u32(0)
	for tmp < char_len {
		ch = (ch << 8) | unsafe { in_txt.str[i + tmp] }
		tmp++
	}
	return ch, char_len
}

// get_charb get a char from position i and return an u32 with the unicode code
[direct_array_access; inline]
fn (re RE) get_charb(in_txt &u8, i int) (u32, int) {
	// ascii 8 bit
	if (re.flag & regex.f_bin) != 0 || unsafe { in_txt[i] } & 0x80 == 0 {
		return u32(unsafe { in_txt[i] }), 1
	}
	// unicode char
	char_len := utf8util_char_len(unsafe { in_txt[i] })
	mut tmp := 0
	mut ch := u32(0)
	for tmp < char_len {
		ch = (ch << 8) | unsafe { in_txt[i + tmp] }
		tmp++
	}
	return ch, char_len
}

[inline]
fn is_alnum(in_char u8) bool {
	mut tmp := in_char - `A`
	if tmp <= 25 {
		return true
	}
	tmp = in_char - `a`
	if tmp <= 25 {
		return true
	}
	tmp = in_char - `0`
	if tmp <= 9 {
		return true
	}
	if in_char == `_` {
		return true
	}
	return false
}

[inline]
fn is_not_alnum(in_char u8) bool {
	return !is_alnum(in_char)
}

[inline]
fn is_space(in_char u8) bool {
	return in_char in regex.spaces
}

[inline]
fn is_not_space(in_char u8) bool {
	return !is_space(in_char)
}

[inline]
fn is_digit(in_char u8) bool {
	tmp := in_char - `0`
	return tmp <= 0x09
}

[inline]
fn is_not_digit(in_char u8) bool {
	return !is_digit(in_char)
}

/*
[inline]
fn is_wordchar(in_char byte) bool {
	return is_alnum(in_char) || in_char == `_`
}

[inline]
fn is_not_wordchar(in_char byte) bool {
	return !is_alnum(in_char)
}
*/

[inline]
fn is_lower(in_char u8) bool {
	tmp := in_char - `a`
	return tmp <= 25
}

[inline]
fn is_upper(in_char u8) bool {
	tmp := in_char - `A`
	return tmp <= 25
}

pub fn (re RE) get_parse_error_string(err int) string {
	match err {
		regex.compile_ok { return 'compile_ok' }
		regex.no_match_found { return 'no_match_found' }
		regex.err_char_unknown { return 'err_char_unknown' }
		regex.err_undefined { return 'err_undefined' }
		regex.err_internal_error { return 'err_internal_error' }
		regex.err_cc_alloc_overflow { return 'err_cc_alloc_overflow' }
		regex.err_syntax_error { return 'err_syntax_error' }
		regex.err_groups_overflow { return 'err_groups_overflow' }
		regex.err_groups_max_nested { return 'err_groups_max_nested' }
		regex.err_group_not_balanced { return 'err_group_not_balanced' }
		regex.err_group_qm_notation { return 'err_group_qm_notation' }
		regex.err_invalid_or_with_cc { return 'err_invalid_or_with_cc' }
		regex.err_neg_group_quantifier { return 'err_neg_group_quantifier' }
		regex.err_consecutive_dots { return 'err_consecutive_dots' }
		else { return 'err_unknown' }
	}
}

// utf8_str convert and utf8 sequence to a printable string
[inline]
fn utf8_str(ch rune) string {
	mut i := 4
	mut res := ''
	for i > 0 {
		v := u8((ch >> ((i - 1) * 8)) & 0xFF)
		if v != 0 {
			res += '${v:1c}'
		}
		i--
	}
	return res
}

// simple_log default log function
fn simple_log(txt string) {
	print(txt)
}

/******************************************************************************
*
* Token Structs
*
******************************************************************************/
pub type FnValidator = fn (u8) bool

struct Token {
mut:
	ist rune
	// char
	ch     rune // char of the token if any
	ch_len u8   // char len
	// Quantifiers / branch
	rep_min int  // used also for jump next in the OR branch [no match] pc jump
	rep_max int  // used also for jump next in the OR branch [   match] pc jump
	greedy  bool // greedy quantifier flag
	// Char class
	cc_index int = -1
	// counters for quantifier check (repetitions)
	rep int
	// validator function pointer
	validator FnValidator
	// groups variables
	group_neg bool // negation flag for the group, 0 => no negation > 0 => negataion
	group_rep int  // repetition of the group
	group_id  int = -1 // id of the group
	goto_pc   int = -1 // jump to this PC if is needed
	// OR flag for the token
	next_is_or bool // true if the next token is an OR
	// dot_char token variables
	dot_check_pc  int = -1 // pc of the next token to check for dots
	bsls_check_pc int = -1 // pc of the next token to check for bsls
	cc_check_pc   int = -1 // pc of the next token to check for CC
	last_dot_flag bool // if true indicate that is the last dot_char in the regex
	// debug fields
	source_index int
}

[inline]
fn (mut tok Token) reset() {
	tok.rep = 0
}

/******************************************************************************
*
* Regex struct
*
******************************************************************************/
pub const (
	f_nl  = 0x00000001 // end the match when find a new line symbol
	f_ms  = 0x00000002 // match true only if the match is at the start of the string
	f_me  = 0x00000004 // match true only if the match is at the end of the string

	f_efm = 0x00000100 // exit on first token matched, used by search
	f_bin = 0x00000200 // work only on bytes, ignore utf-8
	// behaviour modifier flags
	f_src = 0x00020000 // search mode enabled
)

// Log function prototype
pub type FnLog = fn (string)

pub struct RE {
pub mut:
	prog     []Token
	prog_len int // regex program len
	// char classes storage
	cc       []CharClass // char class list
	cc_index int // index
	// groups
	group_count      int   // number of groups in this regex struct
	groups           []int // groups index results
	group_max_nested int = 3 // max nested group
	group_max        int = 8 // max allowed number of different groups

	state_list []StateObj

	group_csave_flag bool  // flag to enable continuous saving
	group_csave      []int //= []int{}  // groups continuous save list

	group_map map[string]int // groups names map

	group_stack []int
	group_data  []int
	// flags
	flag int // flag for optional parameters
	// Debug/log
	debug    int    // enable in order to have the unroll of the code 0 = NO_DEBUG, 1 = LIGHT 2 = VERBOSE
	log_func FnLog = simple_log // log function, can be customized by the user
	query    string // query string
}

// Reset RE object
[direct_array_access; inline]
fn (mut re RE) reset() {
	re.cc_index = 0

	mut i := 0
	for i < re.prog_len {
		re.prog[i].group_rep = 0 // clear repetition of the group
		re.prog[i].rep = 0 // clear repetition of the token
		i++
	}

	// init groups array
	if re.group_count > 0 {
		if re.groups.len == 0 {
			// first run alloc memory
			re.groups = []int{len: re.group_count * 2, init: -1}
		} else {
			// subsequent executions, only clean up the memory
			i = 0
			for i < re.groups.len {
				re.groups[i] = -1
				i++
			}
		}
	}

	// reset group_csave
	if re.group_csave_flag == true {
		re.group_csave.clear() // = []int{}
	}

	// reset state list
	re.state_list.clear()
	re.group_stack.clear()
}

// reset for search mode fail
// gcc bug, dont use [inline] or go 5 time slower
//[inline]
[direct_array_access]
fn (mut re RE) reset_src() {
	mut i := 0
	for i < re.prog_len {
		re.prog[i].group_rep = 0 // clear repetition of the group
		re.prog[i].rep = 0 // clear repetition of the token
		i++
	}
}

/******************************************************************************
*
* Backslashes chars
*
******************************************************************************/
struct BslsStruct {
	ch        rune        // meta char
	validator FnValidator // validator function pointer
}

const (
	bsls_validator_array = [
		BslsStruct{`w`, is_alnum},
		BslsStruct{`W`, is_not_alnum},
		BslsStruct{`s`, is_space},
		BslsStruct{`S`, is_not_space},
		BslsStruct{`d`, is_digit},
		BslsStruct{`D`, is_not_digit},
		BslsStruct{`a`, is_lower},
		BslsStruct{`A`, is_upper},
	]

	// these chars are escape if preceded by a \
	bsls_escape_list     = [`\\`, `|`, `.`, `:`, `*`, `+`, `-`, `{`, `}`, `[`, `]`, `(`, `)`, `?`,
		`^`, `!`]
)

enum BSLS_parse_state {
	start
	bsls_found
	bsls_char
	normal_char
}

// parse_bsls return (index, str_len) bsls_validator_array index, len of the backslash sequence if present
fn (re RE) parse_bsls(in_txt string, in_i int) (int, int) {
	mut status := BSLS_parse_state.start
	mut i := in_i

	for i < in_txt.len {
		// get our char
		char_tmp, char_len := re.get_char(in_txt, i)
		ch := u8(char_tmp)

		if status == .start && ch == `\\` {
			status = .bsls_found
			i += char_len
			continue
		}

		// check if is our bsls char, for now only one length sequence
		if status == .bsls_found {
			for c, x in regex.bsls_validator_array {
				if x.ch == ch {
					return c, i - in_i + 1
				}
			}
			status = .normal_char
			continue
		}

		// no BSLS validator, manage as normal escape char char
		if status == .normal_char {
			if ch in regex.bsls_escape_list {
				return regex.no_match_found, i - in_i + 1
			}
			return regex.err_syntax_error, i - in_i + 1
		}

		// at the present time we manage only one char after the \
		break
	}
	// not our bsls return KO
	return regex.err_syntax_error, i
}

/******************************************************************************
*
* Char class
*
******************************************************************************/
const (
	cc_null = 0 // empty cc token
	cc_char = 1 // simple char: a
	cc_int  = 2 // char interval: a-z
	cc_bsls = 3 // backslash char
	cc_end  = 4 // cc sequence terminator
)

struct CharClass {
mut:
	cc_type   int = regex.cc_null // type of cc token
	ch0       rune        // first char of the interval a-b  a in this case
	ch1       rune        // second char of the interval a-b b in this case
	validator FnValidator // validator function pointer
}

enum CharClass_parse_state {
	start
	in_char
	in_bsls
	separator
	finish
}

fn (re RE) get_char_class(pc int) string {
	buf := []u8{len: (re.cc.len)}
	mut buf_ptr := unsafe { &u8(&buf) }

	mut cc_i := re.prog[pc].cc_index
	mut i := 0
	mut tmp := 0
	for cc_i >= 0 && cc_i < re.cc.len && re.cc[cc_i].cc_type != regex.cc_end {
		if re.cc[cc_i].cc_type == regex.cc_bsls {
			unsafe {
				buf_ptr[i] = `\\`
				i++
				buf_ptr[i] = u8(re.cc[cc_i].ch0)
				i++
			}
		} else if re.cc[cc_i].ch0 == re.cc[cc_i].ch1 {
			tmp = 3
			for tmp >= 0 {
				x := u8((re.cc[cc_i].ch0 >> (tmp * 8)) & 0xFF)
				if x != 0 {
					unsafe {
						buf_ptr[i] = x
						i++
					}
				}
				tmp--
			}
		} else {
			tmp = 3
			for tmp >= 0 {
				x := u8((re.cc[cc_i].ch0 >> (tmp * 8)) & 0xFF)
				if x != 0 {
					unsafe {
						buf_ptr[i] = x
						i++
					}
				}
				tmp--
			}
			unsafe {
				buf_ptr[i] = `-`
				i++
			}
			tmp = 3
			for tmp >= 0 {
				x := u8((re.cc[cc_i].ch1 >> (tmp * 8)) & 0xFF)
				if x != 0 {
					unsafe {
						buf_ptr[i] = x
						i++
					}
				}
				tmp--
			}
		}
		cc_i++
	}
	unsafe {
		buf_ptr[i] = u8(0)
	}
	return unsafe { tos_clone(buf_ptr) }
}

fn (re RE) check_char_class(pc int, ch rune) bool {
	mut cc_i := re.prog[pc].cc_index
	for cc_i >= 0 && cc_i < re.cc.len && re.cc[cc_i].cc_type != regex.cc_end {
		if re.cc[cc_i].cc_type == regex.cc_bsls {
			if re.cc[cc_i].validator(u8(ch)) {
				return true
			}
		} else if ch >= re.cc[cc_i].ch0 && ch <= re.cc[cc_i].ch1 {
			return true
		}
		cc_i++
	}
	return false
}

// parse_char_class return (index, str_len, cc_type) of a char class [abcm-p], char class start after the [ char
fn (mut re RE) parse_char_class(in_txt string, in_i int) (int, int, rune) {
	mut status := CharClass_parse_state.start
	mut i := in_i

	mut tmp_index := re.cc_index
	res_index := re.cc_index

	mut cc_type := u32(regex.ist_char_class_pos)

	for i < in_txt.len {
		// check if we are out of memory for char classes
		if tmp_index >= re.cc.len {
			return regex.err_cc_alloc_overflow, 0, u32(0)
		}

		// get our char
		char_tmp, char_len := re.get_char(in_txt, i)
		ch := u8(char_tmp)

		// println("CC #${i:3d} ch: ${ch:c}")

		// negation
		if status == .start && ch == `^` {
			cc_type = u32(regex.ist_char_class_neg)
			i += char_len
			continue
		}

		// minus symbol
		if status == .start && ch == `-` {
			re.cc[tmp_index].cc_type = regex.cc_char
			re.cc[tmp_index].ch0 = char_tmp
			re.cc[tmp_index].ch1 = char_tmp
			i += char_len
			tmp_index++
			continue
		}

		// bsls
		if (status == .start || status == .in_char) && ch == `\\` {
			// println("CC bsls.")
			status = .in_bsls
			i += char_len
			continue
		}

		if status == .in_bsls {
			// println("CC bsls validation.")
			for c, x in regex.bsls_validator_array {
				if x.ch == ch {
					// println("CC bsls found [${ch:c}]")
					re.cc[tmp_index].cc_type = regex.cc_bsls
					re.cc[tmp_index].ch0 = regex.bsls_validator_array[c].ch
					re.cc[tmp_index].ch1 = regex.bsls_validator_array[c].ch
					re.cc[tmp_index].validator = regex.bsls_validator_array[c].validator
					i += char_len
					tmp_index++
					status = .in_char
					break
				}
			}
			if status == .in_bsls {
				// manage as a simple char
				// println("CC bsls not found [${ch:c}]")
				re.cc[tmp_index].cc_type = regex.cc_char
				re.cc[tmp_index].ch0 = char_tmp
				re.cc[tmp_index].ch1 = char_tmp
				i += char_len
				tmp_index++
				status = .in_char
				continue
			} else {
				continue
			}
		}

		// simple char
		if (status == .start || status == .in_char) && ch != `-` && ch != `]` {
			status = .in_char

			re.cc[tmp_index].cc_type = regex.cc_char
			re.cc[tmp_index].ch0 = char_tmp
			re.cc[tmp_index].ch1 = char_tmp

			i += char_len
			tmp_index++
			continue
		}

		// check range separator
		if status == .in_char && ch == `-` {
			status = .separator
			i += char_len
			continue
		}

		// check range end
		if status == .separator && ch != `]` && ch != `-` {
			status = .in_char
			re.cc[tmp_index - 1].cc_type = regex.cc_int
			re.cc[tmp_index - 1].ch1 = char_tmp
			i += char_len
			continue
		}

		// char class end
		if status == .in_char && ch == `]` {
			re.cc[tmp_index].cc_type = regex.cc_end
			re.cc[tmp_index].ch0 = 0
			re.cc[tmp_index].ch1 = 0
			re.cc_index = tmp_index + 1

			return res_index, i - in_i + 2, cc_type
		}

		i++
	}
	return regex.err_syntax_error, 0, u32(0)
}

/******************************************************************************
*
* Re Compiler
*
******************************************************************************/
//
// Quantifier
//
enum Quant_parse_state {
	start
	min_parse
	comma_checked
	max_parse
	greedy
	gredy_parse
	finish
}

// parse_quantifier return (min, max, str_len, greedy_flag) of a {min,max}? quantifier starting after the { char
fn (re RE) parse_quantifier(in_txt string, in_i int) (int, int, int, bool) {
	mut status := Quant_parse_state.start
	mut i := in_i

	mut q_min := 0 // default min in a {} quantifier is 1
	mut q_max := 0 // deafult max in a {} quantifier is max_quantifier

	mut ch := u8(0)

	for i < in_txt.len {
		unsafe {
			ch = in_txt.str[i]
		}
		// println("${ch:c} status: $status")

		// exit on no compatible char with {} quantifier
		if utf8util_char_len(ch) != 1 {
			return regex.err_syntax_error, i, 0, false
		}

		// min parsing skip if comma present
		if status == .start && ch == `,` {
			q_min = 0 // default min in a {} quantifier is 0
			status = .comma_checked
			i++
			continue
		}

		if status == .start && is_digit(ch) {
			status = .min_parse
			q_min *= 10
			q_min += int(ch - `0`)
			i++
			continue
		}

		if status == .min_parse && is_digit(ch) {
			q_min *= 10
			q_min += int(ch - `0`)
			i++
			continue
		}

		// we have parsed the min, now check the max
		if status == .min_parse && ch == `,` {
			status = .comma_checked
			i++
			continue
		}

		// single value {4}
		if status == .min_parse && ch == `}` {
			q_max = q_min
			status = .greedy
			continue
		}

		// end without max
		if status == .comma_checked && ch == `}` {
			q_max = regex.max_quantifier
			status = .greedy
			continue
		}

		// start max parsing
		if status == .comma_checked && is_digit(ch) {
			status = .max_parse
			q_max *= 10
			q_max += int(ch - `0`)
			i++
			continue
		}

		// parse the max
		if status == .max_parse && is_digit(ch) {
			q_max *= 10
			q_max += int(ch - `0`)
			i++
			continue
		}

		// finished the quantifier
		if status == .max_parse && ch == `}` {
			status = .greedy
			continue
		}

		// check if greedy flag char ? is present
		if status == .greedy {
			if i + 1 < in_txt.len {
				i++
				status = .gredy_parse
				continue
			}
			return q_min, q_max, i - in_i + 2, false
		}

		// check the greedy flag
		if status == .gredy_parse {
			if ch == `?` {
				return q_min, q_max, i - in_i + 2, true
			} else {
				i--
				return q_min, q_max, i - in_i + 2, false
			}
		}

		// not  a {} quantifier, exit
		return regex.err_syntax_error, i, 0, false
	}

	// not a conform {} quantifier
	return regex.err_syntax_error, i, 0, false
}

//
// Groups
//
enum Group_parse_state {
	start
	q_mark // (?
	q_mark1 // (?:|P  checking
	p_status // (?P
	p_start // (?P<
	p_end // (?P<...>
	p_in_name // (?P<...
	finish
}

// parse_groups parse a group for ? (question mark) syntax, if found, return (error, capture_flag, negate_flag, name_of_the_group, next_index)
fn (re RE) parse_groups(in_txt string, in_i int) (int, bool, bool, string, int) {
	mut status := Group_parse_state.start
	mut i := in_i
	mut name := ''

	for i < in_txt.len && status != .finish {
		// get our char
		char_tmp, char_len := re.get_char(in_txt, i)
		ch := u8(char_tmp)

		// start
		if status == .start && ch == `(` {
			status = .q_mark
			i += char_len
			continue
		}

		// check for question marks
		if status == .q_mark && ch == `?` {
			status = .q_mark1
			i += char_len
			continue
		}

		// negate group
		if status == .q_mark1 && ch == `!` {
			i += char_len
			return 0, false, true, name, i
		}

		// non capturing group
		if status == .q_mark1 && ch == `:` {
			i += char_len
			return 0, false, false, name, i
		}

		// enter in P section
		if status == .q_mark1 && ch == `P` {
			status = .p_status
			i += char_len
			continue
		}

		// not a valid q mark found
		if status == .q_mark1 {
			// println("NO VALID Q MARK")
			return -2, true, false, name, i
		}

		if status == .p_status && ch == `<` {
			status = .p_start
			i += char_len
			continue
		}

		if status == .p_start && ch != `>` {
			status = .p_in_name
			name += '${ch:1c}' // TODO: manage utf8 chars
			i += char_len
			continue
		}

		// colect name
		if status == .p_in_name && ch != `>` && is_alnum(ch) {
			name += '${ch:1c}' // TODO: manage utf8 chars
			i += char_len
			continue
		}

		// end name
		if status == .p_in_name && ch == `>` {
			i += char_len
			return 0, true, false, name, i
		}

		// error on name group
		if status == .p_in_name {
			return -2, true, false, name, i
		}

		// normal group, nothig to do, exit
		return 0, true, false, name, i
	}
	// UNREACHABLE
	// println("ERROR!! NOT MEANT TO BE HERE!!1")
	return -2, true, false, name, i
}

const (
	quntifier_chars = [rune(`+`), `*`, `?`, `{`]
)

//
// main compiler
//
// compile return (return code, index) where index is the index of the error in the query string if return code is an error code
fn (mut re RE) impl_compile(in_txt string) (int, int) {
	mut i := 0 // input string index
	mut pc := 0 // program counter

	// group management variables
	mut group_count := -1
	mut group_stack := []int{len: re.group_max_nested, init: 0}
	mut group_stack_txt_index := []int{len: re.group_max_nested, init: -1}
	mut group_stack_index := -1

	re.query = in_txt // save the query string

	i = 0
	for i < in_txt.len {
		mut char_tmp := u32(0)
		mut char_len := 0
		// println("i: ${i:3d} ch: ${in_txt.str[i]:c}")

		char_tmp, char_len = re.get_char(in_txt, i)

		//
		// check special cases: $ ^
		//
		if char_len == 1 && i == 0 && u8(char_tmp) == `^` {
			re.flag = regex.f_ms
			i = i + char_len
			continue
		}
		if char_len == 1 && i == (in_txt.len - 1) && u8(char_tmp) == `$` {
			re.flag = regex.f_me
			i = i + char_len
			continue
		}

		// ist_group_start
		if char_len == 1 && pc >= 0 && u8(char_tmp) == `(` {
			// check max groups allowed
			if group_count > re.group_max {
				return regex.err_groups_overflow, i + 1
			}
			group_stack_index++

			// check max nested groups allowed
			if group_stack_index > re.group_max_nested {
				return regex.err_groups_max_nested, i + 1
			}

			tmp_res, cgroup_flag, negate_flag, cgroup_name, next_i := re.parse_groups(in_txt,
				i)

			// manage question mark format error
			if tmp_res < -1 {
				return regex.err_group_qm_notation, next_i
			}

			// println("Parse group: [$tmp_res, $cgroup_flag, ($i,$next_i), '${in_txt[i..next_i]}' ]")
			i = next_i

			if cgroup_flag == true {
				group_count++
			}

			// calculate the group id
			// if it is a named group, recycle the group id
			// NOTE: **** the group index is +1 because map return 0 when not found!! ****
			mut group_id := group_count
			if cgroup_name.len > 0 {
				// println("GROUP NAME: ${cgroup_name}")
				if cgroup_name in re.group_map {
					group_id = re.group_map[cgroup_name] - 1
					group_count--
				} else {
					re.group_map[cgroup_name] = group_id + 1
				}
			}

			group_stack_txt_index[group_stack_index] = i
			group_stack[group_stack_index] = pc

			re.prog[pc].ist = u32(0) | regex.ist_group_start
			re.prog[pc].rep_min = 1
			re.prog[pc].rep_max = 1

			// manage negation groups
			if negate_flag == true {
				re.prog[pc].group_neg = true
				re.prog[pc].rep_min = 0 // may be not catched, but it is ok
			}

			// set the group id
			if cgroup_flag == false {
				// println("NO CAPTURE GROUP")
				re.prog[pc].group_id = -1
			} else {
				re.prog[pc].group_id = group_id
			}

			pc = pc + 1
			continue
		}

		// ist_group_end
		if char_len == 1 && pc > 0 && u8(char_tmp) == `)` {
			if group_stack_index < 0 {
				return regex.err_group_not_balanced, i + 1
			}

			goto_pc := group_stack[group_stack_index]
			group_stack_index--

			re.prog[pc].ist = u32(0) | regex.ist_group_end
			re.prog[pc].rep_min = 1
			re.prog[pc].rep_max = 1

			re.prog[pc].goto_pc = goto_pc // PC where to jump if a group need
			re.prog[pc].group_id = re.prog[goto_pc].group_id // id of this group, used for storing data

			re.prog[goto_pc].goto_pc = pc // start goto point to the end group pc
			// re.prog[goto_pc].group_id = group_count         // id of this group, used for storing data

			// duplicate the negation group info and settings
			if re.prog[goto_pc].group_neg == true {
				re.prog[pc].group_neg = re.prog[goto_pc].group_neg
				re.prog[pc].rep_min = re.prog[goto_pc].rep_min
			}

			pc = pc + 1
			i = i + char_len
			continue
		}

		// ist_dot_char match any char except the following token
		if char_len == 1 && pc >= 0 && u8(char_tmp) == `.` {
			// consecutive ist_dot_char is a syntax error
			if pc > 0 && re.prog[pc - 1].ist == regex.ist_dot_char {
				return regex.err_consecutive_dots, i
			}

			re.prog[pc].ist = u32(0) | regex.ist_dot_char
			re.prog[pc].rep_min = 1
			re.prog[pc].rep_max = 1
			pc = pc + 1
			i = i + char_len
			continue
		}

		// OR branch
		if char_len == 1 && pc > 0 && u8(char_tmp) == `|` {
			if pc > 0 && re.prog[pc - 1].ist == regex.ist_or_branch {
				return regex.err_syntax_error, i
			}
			re.prog[pc].ist = u32(0) | regex.ist_or_branch
			re.prog[pc].source_index = i
			pc = pc + 1
			i = i + char_len
			continue
		}

		// Quantifiers
		if char_len == 1 && pc > 0 {
			mut char_next := rune(0)
			mut char_next_len := 0
			if (char_len + i) < in_txt.len {
				char_next, char_next_len = re.get_char(in_txt, i + char_len)
			}
			mut quant_flag := true

			// negation groups can not have quantifiers
			if re.prog[pc - 1].group_neg == true && char_tmp in [`?`, `+`, `*`, `{`] {
				return regex.err_neg_group_quantifier, i
			}

			match u8(char_tmp) {
				`?` {
					// println("q: ${char_tmp:c}")
					// check illegal quantifier sequences
					if char_next_len == 1 && char_next in regex.quntifier_chars {
						return regex.err_syntax_error, i
					}
					re.prog[pc - 1].rep_min = 0
					re.prog[pc - 1].rep_max = 1
				}
				`+` {
					// println("q: ${char_tmp:c}")
					// check illegal quantifier sequences
					if char_next_len == 1 && char_next in regex.quntifier_chars {
						return regex.err_syntax_error, i
					}
					re.prog[pc - 1].rep_min = 1
					re.prog[pc - 1].rep_max = regex.max_quantifier
				}
				`*` {
					// println("q: ${char_tmp:c}")
					// check illegal quantifier sequences
					if char_next_len == 1 && char_next in regex.quntifier_chars {
						return regex.err_syntax_error, i
					}
					re.prog[pc - 1].rep_min = 0
					re.prog[pc - 1].rep_max = regex.max_quantifier
				}
				`{` {
					min, max, tmp, greedy := re.parse_quantifier(in_txt, i + 1)
					// it is a quantifier
					if min >= 0 {
						// println("{$min,$max}\n str:[${in_txt[i..i+tmp]}] greedy:$greedy")
						i = i + tmp
						re.prog[pc - 1].rep_min = min
						re.prog[pc - 1].rep_max = max
						re.prog[pc - 1].greedy = greedy
						// check illegal quantifier sequences
						if i <= in_txt.len {
							char_next, char_next_len = re.get_char(in_txt, i)
							if char_next_len == 1 && char_next in regex.quntifier_chars {
								return regex.err_syntax_error, i
							}
						}
						continue
					} else {
						return min, i
					}

					// TODO: decide if the open bracket can be conform without the close bracket
					/*
					// no conform, parse as normal char
					else {
						quant_flag = false
					}
					*/
				}
				else {
					quant_flag = false
				}
			}

			if quant_flag {
				i = i + char_len
				continue
			}
		}

		// IST_CHAR_CLASS_*
		if char_len == 1 && pc >= 0 {
			if u8(char_tmp) == `[` {
				cc_index, tmp, cc_type := re.parse_char_class(in_txt, i + 1)
				if cc_index >= 0 {
					// println("index: $cc_index str:${in_txt[i..i+tmp]}")
					i = i + tmp
					re.prog[pc].ist = u32(0) | cc_type
					re.prog[pc].cc_index = cc_index
					re.prog[pc].rep_min = 1
					re.prog[pc].rep_max = 1
					pc = pc + 1
					continue
				}
				// cc_class vector memory full
				else if cc_index < 0 {
					return cc_index, i
				}
			}
		}

		// ist_bsls_char
		if char_len == 1 && pc >= 0 {
			if u8(char_tmp) == `\\` {
				bsls_index, tmp := re.parse_bsls(in_txt, i)
				// println("index: $bsls_index str:${in_txt[i..i+tmp]}")
				if bsls_index >= 0 {
					i = i + tmp
					re.prog[pc].ist = u32(0) | regex.ist_bsls_char
					re.prog[pc].rep_min = 1
					re.prog[pc].rep_max = 1
					re.prog[pc].validator = regex.bsls_validator_array[bsls_index].validator
					re.prog[pc].ch = regex.bsls_validator_array[bsls_index].ch
					pc = pc + 1
					continue
				}
				// this is an escape char, skip the bsls and continue as a normal char
				else if bsls_index == regex.no_match_found {
					i += char_len
					char_tmp, char_len = re.get_char(in_txt, i)
					// continue as simple char
				}
				// if not an escape or a bsls char then it is an error (at least for now!)
				else {
					return bsls_index, i + tmp
				}
			}
		}

		// ist_simple_char
		re.prog[pc].ist = regex.ist_simple_char
		re.prog[pc].ch = char_tmp
		re.prog[pc].ch_len = u8(char_len)
		re.prog[pc].rep_min = 1
		re.prog[pc].rep_max = 1
		// println("char: ${char_tmp:c}")
		pc = pc + 1

		i += char_len
	}

	// add end of the program
	re.prog[pc].ist = regex.ist_prog_end
	re.prog_len = pc

	// check for unbalanced groups
	if group_stack_index != -1 {
		return regex.err_group_not_balanced, group_stack_txt_index[group_stack_index] + 1
	}

	// check for OR at the end of the program
	if pc > 0 && re.prog[pc - 1].ist == regex.ist_or_branch {
		return regex.err_syntax_error, in_txt.len - 1
	}

	// store the number of groups in the query
	re.group_count = group_count + 1

	//******************************************
	// Post processing
	//******************************************

	//
	// manage ist_dot_char
	//

	// find the checks for dot chars, if any...
	mut pc1 := 0
	mut dot_char_count := 0
	mut last_dot_char_pc := -1
	for pc1 < pc {
		if re.prog[pc1].ist == regex.ist_dot_char {
			// println("Dot_char pc: $pc1")
			last_dot_char_pc = pc1
			dot_char_count++
			mut pc2 := pc1 + 1
			for pc2 < pc {
				// consecutive dot chars is an error
				if re.prog[pc2].ist == regex.ist_dot_char {
					return regex.err_syntax_error, 0
				}
				if re.prog[pc2].ist !in [rune(regex.ist_prog_end), regex.ist_group_end,
					regex.ist_group_start] {
					// println("Next dot char check is PC: ${pc2}")
					re.prog[pc1].dot_check_pc = pc2
					break
				}
				pc2++
			}
		}
		pc1++
	}

	// println("last_dot_char_pc: ${last_dot_char_pc}")
	if last_dot_char_pc >= 0 {
		pc1 = last_dot_char_pc + 1
		mut is_last_dot := true
		for pc1 < pc {
			if re.prog[pc1].ist !in [rune(regex.ist_prog_end), regex.ist_group_end] {
				is_last_dot = false
				break
			}
			pc1++
		}
		if is_last_dot {
			re.prog[last_dot_char_pc].last_dot_flag = true
		}
	}

	//
	// manage bsls_char
	//

	// find the checks for bsls, if any...
	pc1 = 0
	mut bsls_char_count := 0
	mut last_bsls_char_pc := -1
	for pc1 < pc {
		if re.prog[pc1].ist == regex.ist_bsls_char {
			// println("bsls_char pc: $pc1")
			last_bsls_char_pc = pc1
			bsls_char_count++
			mut pc2 := pc1 + 1
			for pc2 < pc {
				if re.prog[pc2].ist !in [rune(regex.ist_prog_end), regex.ist_group_end,
					regex.ist_group_start] {
					// println("Next bsls check is PC: ${pc2}")
					re.prog[pc1].bsls_check_pc = pc2
					break
				}
				pc2++
			}
		}
		pc1++
	}

	// println('last_bsls_char_pc: ${last_bsls_char_pc}')
	if last_bsls_char_pc >= 0 {
		pc1 = last_bsls_char_pc + 1
		mut is_last_bsls := true
		for pc1 < pc {
			if re.prog[pc1].ist !in [rune(regex.ist_prog_end), regex.ist_group_end] {
				is_last_bsls = false
				break
			}
			pc1++
		}
		if is_last_bsls {
			re.prog[last_bsls_char_pc].last_dot_flag = true
		}
	}

	//
	// manage CC
	//
	pc1 = 0
	mut cc_char_count := 0
	mut last_cc_char_pc := -1
	for pc1 < pc {
		if re.prog[pc1].ist in [rune(regex.ist_char_class_pos), regex.ist_char_class_neg] {
			last_cc_char_pc = pc1
			cc_char_count++
			mut pc2 := pc1 + 1
			for pc2 < pc {
				if re.prog[pc2].ist !in [rune(regex.ist_prog_end), regex.ist_group_end,
					regex.ist_group_start] {
					// println("Next CC check is PC: ${pc2}")
					re.prog[pc1].cc_check_pc = pc2
					break
				}
				pc2++
			}
		}
		pc1++
	}

	// println('last_cc_char_pc: ${last_cc_char_pc}')
	if last_cc_char_pc >= 0 {
		pc1 = last_cc_char_pc + 1
		mut is_last_cc := true
		for pc1 < pc {
			if re.prog[pc1].ist !in [rune(regex.ist_prog_end), regex.ist_group_end] {
				is_last_cc = false
				break
			}
			pc1++
		}
		if is_last_cc {
			re.prog[last_cc_char_pc].last_dot_flag = true
		}
	}

	//******************************************

	// OR branch
	// a|b|cd
	// d exit point
	// a,b,c branches
	// set the jump in the right places
	pc1 = 0
	for pc1 < pc - 2 {
		// println("Here $pc1 ${pc-2}")
		// println("source index: ${pc1 + 1} => ${re.prog[pc1+1].source_index}")
		if re.prog[pc1 + 1].ist == regex.ist_or_branch {
			// two consecutive OR are a syntax error
			if re.prog[pc1 + 2].ist == regex.ist_or_branch {
				return regex.err_syntax_error, i
			}

			// check for []|[] errors
			if re.prog[pc1].ist == regex.ist_char_class_pos
				&& re.prog[pc1 + 2].ist == regex.ist_char_class_pos {
				return regex.err_invalid_or_with_cc, re.prog[pc1 + 1].source_index
			}
		}

		// manange a|b chains like a|(b)|c|d...
		// standard solution
		if re.prog[pc1].ist != regex.ist_or_branch && re.prog[pc1 + 1].ist == regex.ist_or_branch
			&& re.prog[pc1 + 2].ist != regex.ist_or_branch {
			re.prog[pc1].next_is_or = true // set that the next token is an  OR
			re.prog[pc1 + 1].rep_min = pc1 + 2 // failed match jump

			// match jump, if an OR chain the next token will be an OR token
			mut pc2 := pc1 + 2
			for pc2 < pc - 1 {
				ist := re.prog[pc2].ist
				if ist == regex.ist_group_start {
					re.prog[pc1 + 1].rep_max = re.prog[pc2].goto_pc + 1
					break
				}
				if ist != regex.ist_or_branch {
					re.prog[pc1 + 1].rep_max = pc2 + 1
					break
				}

				pc2++
			}
			// special case query of few chars, the true can't go on the first instruction
			if re.prog[pc1 + 1].rep_max == pc1 {
				re.prog[pc1 + 1].rep_max = 3
			}
			// println("Compile OR postproc. [$pc1,OR ${pc1+1},$pc2]")
			pc1 = pc2
			continue
		}

		pc1++
	}

	//******************************************
	// DEBUG PRINT REGEX GENERATED CODE
	//******************************************
	if re.debug > 0 {
		gc := re.get_code()
		re.log_func(gc)
	}
	//******************************************

	return regex.compile_ok, 0
}

// get_code return the compiled code as regex string, note: may be different from the source!
pub fn (re RE) get_code() string {
	mut pc1 := 0
	mut res := strings.new_builder(re.cc.len * 2 * re.prog.len)
	res.write_string('========================================\nv RegEx compiler v $regex.v_regex_version output:\n')

	mut stop_flag := false

	for pc1 <= re.prog.len {
		tk := re.prog[pc1]
		res.write_string('PC:${pc1:3d}')

		res.write_string(' ist: ')
		res.write_string('${tk.ist:8x}'.replace(' ', '0'))
		res.write_string(' ')
		ist := tk.ist
		if ist == regex.ist_bsls_char {
			res.write_string('[\\${tk.ch:1c}]     BSLS')
			if tk.last_dot_flag == true {
				res.write_string(' last!')
			}
		} else if ist == regex.ist_prog_end {
			res.write_string('PROG_END')
			stop_flag = true
		} else if ist == regex.ist_or_branch {
			res.write_string('OR      ')
		} else if ist == regex.ist_char_class_pos {
			res.write_string('[${re.get_char_class(pc1)}]     CHAR_CLASS_POS')
			if tk.last_dot_flag == true {
				res.write_string(' last!')
			}
		} else if ist == regex.ist_char_class_neg {
			res.write_string('[^${re.get_char_class(pc1)}]    CHAR_CLASS_NEG')
			if tk.last_dot_flag == true {
				res.write_string(' last!')
			}
		} else if ist == regex.ist_dot_char {
			res.write_string('.        DOT_CHAR nx chk: $tk.dot_check_pc')
			if tk.last_dot_flag == true {
				res.write_string(' last!')
			}
		} else if ist == regex.ist_group_start {
			res.write_string('(        GROUP_START #:$tk.group_id')
			if tk.group_id == -1 {
				res.write_string(' ?:')
			} else {
				for x in re.group_map.keys() {
					if re.group_map[x] == (tk.group_id + 1) {
						res.write_string(' ?P<$x>')
						break
					}
				}
			}
		} else if ist == regex.ist_group_end {
			res.write_string(')        GROUP_END   #:$tk.group_id')
		} else if ist == regex.ist_simple_char {
			res.write_string('[${tk.ch:1c}]      query_ch')
		}

		if tk.rep_max == regex.max_quantifier {
			res.write_string(' {${tk.rep_min:3d},MAX}')
		} else {
			if ist == regex.ist_or_branch {
				res.write_string(' if false go: ${tk.rep_min:3d} if true go: ${tk.rep_max:3d}')
			} else {
				res.write_string(' {${tk.rep_min:3d},${tk.rep_max:3d}}')
			}
			if tk.greedy == true {
				res.write_string('?')
			}
		}

		res.write_string('\n')
		if stop_flag {
			break
		}
		pc1++
	}

	res.write_string('========================================\n')
	return res.str()
}

// get_query return a string with a reconstruction of the query starting from the regex program code
pub fn (re RE) get_query() string {
	mut res := strings.new_builder(re.query.len * 2)

	if (re.flag & regex.f_ms) != 0 {
		res.write_string('^')
	}

	mut i := 0
	for i < re.prog.len && re.prog[i].ist != regex.ist_prog_end && re.prog[i].ist != 0 {
		tk := unsafe { &re.prog[i] }
		ch := tk.ist

		// GROUP start
		if ch == regex.ist_group_start {
			if re.debug > 0 {
				res.write_string('#$tk.group_id')
			}
			res.write_string('(')

			if tk.group_neg == true {
				res.write_string('?!') // negation group
			} else if tk.group_id == -1 {
				res.write_string('?:') // non capturing group
			}

			for x in re.group_map.keys() {
				if re.group_map[x] == (tk.group_id + 1) {
					res.write_string('?P<$x>')
					break
				}
			}

			i++
			continue
		}

		// GROUP end
		if ch == regex.ist_group_end {
			res.write_string(')')
		}

		// OR branch
		if ch == regex.ist_or_branch {
			res.write_string('|')
			if re.debug > 0 {
				res.write_string('{$tk.rep_min,$tk.rep_max}')
			}
			i++
			continue
		}

		// char class
		if ch == regex.ist_char_class_neg || ch == regex.ist_char_class_pos {
			res.write_string('[')
			if ch == regex.ist_char_class_neg {
				res.write_string('^')
			}
			res.write_string('${re.get_char_class(i)}')
			res.write_string(']')
		}

		// bsls char
		if ch == regex.ist_bsls_char {
			res.write_string('\\${tk.ch:1c}')
		}

		// ist_dot_char
		if ch == regex.ist_dot_char {
			res.write_string('.')
		}

		// char alone
		if ch == regex.ist_simple_char {
			if u8(ch) in regex.bsls_escape_list {
				res.write_string('\\')
			}
			res.write_string('${tk.ch:c}')
		}

		// quantifier
		if !(tk.rep_min == 1 && tk.rep_max == 1) && tk.group_neg == false {
			if tk.rep_min == 0 && tk.rep_max == 1 {
				res.write_string('?')
			} else if tk.rep_min == 1 && tk.rep_max == regex.max_quantifier {
				res.write_string('+')
			} else if tk.rep_min == 0 && tk.rep_max == regex.max_quantifier {
				res.write_string('*')
			} else {
				if tk.rep_max == regex.max_quantifier {
					res.write_string('{$tk.rep_min,MAX}')
				} else {
					res.write_string('{$tk.rep_min,$tk.rep_max}')
				}
				if tk.greedy == true {
					res.write_string('?')
				}
			}
		}
		i++
	}
	if (re.flag & regex.f_me) != 0 {
		res.write_string('$')
	}

	return res.str()
}

/******************************************************************************
*
* Groups saving utilities
*
******************************************************************************/
[direct_array_access]
fn (mut re RE) group_continuous_save(g_index int) {
	if re.group_csave_flag == true {
		// continuous save, save until we have space

		// init the first element as counter
		if re.group_csave.len == 0 {
			re.group_csave << 0
		}

		gi := g_index >> 1
		start := re.groups[g_index]
		end := re.groups[g_index + 1]

		// check if we are simply increasing the size ot the found group
		if re.group_csave.len >= 4 && gi == re.group_csave[re.group_csave.len - 3]
			&& start == re.group_csave[re.group_csave.len - 2] {
			re.group_csave[re.group_csave.len - 1] = end
			return
		}

		// otherwise append a new group to the list

		// increment counter
		re.group_csave[0]++
		// save the record
		re.group_csave << (g_index >> 1) // group id
		re.group_csave << re.groups[g_index] // start
		re.group_csave << re.groups[g_index + 1] // end
	}
}

/******************************************************************************
*
* Matching
*
******************************************************************************/
enum Match_state {
	start = 0
	stop
	end
	new_line
	ist_load // load and execute instruction
	ist_next // go to next instruction
	ist_next_ks // go to next instruction without clenaning the state
	ist_quant_p // match positive ,quantifier check
	ist_quant_n // match negative, quantifier check
	ist_quant_pg // match positive ,group quantifier check
	ist_quant_ng // match negative ,group quantifier check
}

fn state_str(s Match_state) string {
	match s {
		.start { return 'start' }
		.stop { return 'stop' }
		.end { return 'end' }
		.new_line { return 'new line' }
		.ist_load { return 'ist_load' }
		.ist_next { return 'ist_next' }
		.ist_next_ks { return 'ist_next_ks' }
		.ist_quant_p { return 'ist_quant_p' }
		.ist_quant_n { return 'ist_quant_n' }
		.ist_quant_pg { return 'ist_quant_pg' }
		.ist_quant_ng { return 'ist_quant_ng' }
	}
}

struct StateObj {
pub mut:
	group_index int = -1 // group id used to know how many groups are open
	match_flag  bool // indicate if we are in a match condition
	match_index int = -1 // index of the last match
	first_match int = -1 // index of the first match
	pc          int = -1 // program counter
	i           int = -1 // source string index
	char_len    int  // last char legth
	last_dot_pc int = -1 // last dot chat pc
}

[direct_array_access]
pub fn (mut re RE) match_base(in_txt &u8, in_txt_len int) (int, int) {
	// result status
	mut result := regex.no_match_found // function return

	mut ch := rune(0) // examinated char
	mut char_len := 0 // utf8 examinated char len
	mut m_state := Match_state.start // start point for the matcher FSM
	mut src_end := false
	mut last_fnd_pc := -1

	mut state := StateObj{} // actual state
	mut ist := rune(0) // actual instruction
	mut l_ist := rune(0) // last matched instruction

	mut step_count := 0 // stats for debug
	mut dbg_line := 0 // count debug line printed

	re.reset()

	if re.debug > 0 {
		// print header
		mut h_buf := strings.new_builder(32)
		h_buf.write_string('flags: ')
		h_buf.write_string('${re.flag:8x}'.replace(' ', '0'))
		h_buf.write_string('\n')
		sss := h_buf.str()
		re.log_func(sss)
	}

	for m_state != .end {
		if state.pc >= 0 && state.pc < re.prog.len {
			ist = re.prog[state.pc].ist
		} else if state.pc >= re.prog.len {
			// println("ERROR!! PC overflow!!")
			return regex.err_internal_error, state.i
		}

		//******************************************
		// DEBUG LOG
		//******************************************
		if re.debug > 0 {
			mut buf2 := strings.new_builder(re.cc.len + 128)

			// print all the instructions

			// end of the input text
			if state.i >= in_txt_len {
				buf2.write_string('# ${step_count:3d} END OF INPUT TEXT\n')
				sss := buf2.str()
				re.log_func(sss)
			} else {
				// print only the exe instruction
				if (re.debug == 1 && m_state == .ist_load) || re.debug == 2 {
					if ist == regex.ist_prog_end {
						buf2.write_string('# ${step_count:3d} PROG_END\n')
					} else if ist == 0 || m_state in [.start, .ist_next, .stop] {
						buf2.write_string('# ${step_count:3d} s: ${state_str(m_state):12s} PC: NA\n')
					} else {
						ch, char_len = re.get_charb(in_txt, state.i)

						buf2.write_string('# ${step_count:3d} s: ${state_str(m_state):12s} PC: ${state.pc:3d}=>')
						buf2.write_string('${ist:8x}'.replace(' ', '0'))
						buf2.write_string(" i,ch,len:[${state.i:3d},'${utf8_str(ch)}',$char_len] f.m:[${state.first_match:3d},${state.match_index:3d}] ")

						if ist == regex.ist_simple_char {
							buf2.write_string('query_ch: [${re.prog[state.pc].ch:1c}]')
						} else {
							if ist == regex.ist_bsls_char {
								buf2.write_string('BSLS [\\${re.prog[state.pc].ch:1c}]')
							} else if ist == regex.ist_prog_end {
								buf2.write_string('PROG_END')
							} else if ist == regex.ist_or_branch {
								buf2.write_string('OR')
							} else if ist == regex.ist_char_class_pos {
								buf2.write_string('CHAR_CLASS_POS[${re.get_char_class(state.pc)}]')
							} else if ist == regex.ist_char_class_neg {
								buf2.write_string('CHAR_CLASS_NEG[${re.get_char_class(state.pc)}]')
							} else if ist == regex.ist_dot_char {
								buf2.write_string('DOT_CHAR')
							} else if ist == regex.ist_group_start {
								tmp_gi := re.prog[state.pc].group_id
								tmp_gr := re.prog[re.prog[state.pc].goto_pc].group_rep
								buf2.write_string('GROUP_START #:$tmp_gi rep:$tmp_gr ')
							} else if ist == regex.ist_group_end {
								buf2.write_string('GROUP_END   #:${re.prog[state.pc].group_id} deep:$state.group_index')
							}
						}
						if re.prog[state.pc].rep_max == regex.max_quantifier {
							buf2.write_string('{${re.prog[state.pc].rep_min},MAX}:${re.prog[state.pc].rep}')
						} else {
							buf2.write_string('{${re.prog[state.pc].rep_min},${re.prog[state.pc].rep_max}}:${re.prog[state.pc].rep}')
						}
						if re.prog[state.pc].greedy == true {
							buf2.write_string('?')
						}
						buf2.write_string(' (#$state.group_index)')

						if ist == regex.ist_dot_char {
							buf2.write_string(' last!')
						}

						buf2.write_string('\n')
					}
					sss2 := buf2.str()
					re.log_func(sss2)
				}
			}
			step_count++
			dbg_line++
		}
		//******************************************

		if ist == regex.ist_prog_end {
			// println("HERE we end!")
			break
		}

		// we're out of text, manage it
		if state.i >= in_txt_len || m_state == .new_line {
			// println("Finished text!!")
			src_end = true

			// we have fished the text, we must manage out pf bound indexes
			if state.i >= in_txt_len {
				state.i = in_txt_len - 1
			}

			// manage groups
			if state.group_index >= 0 && state.match_index >= 0 {
				// println("End text with open groups!")
				// println("state.group_index: ${state.group_index}")
				// close the groups
				for state.group_index >= 0 {
					tmp_pc := re.group_data[state.group_index]
					re.prog[tmp_pc].group_rep++
					// println("Closing group $state.group_index {${re.prog[tmp_pc].rep_min},${re.prog[tmp_pc].rep_max}}:${re.prog[tmp_pc].group_rep}")

					if re.prog[tmp_pc].group_rep >= re.prog[tmp_pc].rep_min
						&& re.prog[tmp_pc].group_id >= 0 {
						start_i := re.group_stack[state.group_index]
						re.group_stack[state.group_index] = -1

						// save group results
						g_index := re.prog[tmp_pc].group_id * 2
						// println("group_id: ${re.prog[tmp_pc].group_id} g_index: ${g_index}")
						if start_i >= 0 {
							re.groups[g_index] = start_i
						} else {
							re.groups[g_index] = 0
						}

						re.groups[g_index + 1] = state.i

						if re.groups[g_index + 1] >= in_txt_len {
							// println("clamp group on stop!")
							re.groups[g_index + 1] = in_txt_len - 1
						}

						// continuous save, save until we have space
						re.group_continuous_save(g_index)
					}
					state.group_index--
				}
			}

			// println("re.groups: ${re.groups}")

			// the text is finished and the groups closed and we are the last group, ok exit
			if ist == regex.ist_group_end && re.prog[state.pc + 1].ist == regex.ist_prog_end {
				// println("Last group end")
				return state.first_match, state.i
			}

			if state.pc == -1 {
				state.pc = last_fnd_pc
			}

			// println("Finished text!!")
			// println("Instruction: ${ist:08x} pc: $state.pc")
			// println("min_rep: ${re.prog[state.pc].rep_min} max_rep: ${re.prog[state.pc].rep_max} rep: ${re.prog[state.pc].rep}")

			// program end
			if ist == regex.ist_prog_end {
				// println("Program end on end of text!")
				return state.first_match, state.i
			}

			if l_ist in [
				rune(regex.ist_char_class_neg),
				regex.ist_char_class_pos,
				regex.ist_bsls_char,
				regex.ist_dot_char,
			] {
				// println("***** We have a last special token")
				// println("PC: ${state.pc} last_dot_flag:${re.prog[state.pc].last_dot_flag}")
				// println("rep: ${re.prog[state.pc].group_rep} min: ${re.prog[state.pc].rep_min} max: ${re.prog[state.pc].rep_max}")
				// println("first match: ${state.first_match}")

				if re.prog[state.pc].last_dot_flag == true
					&& re.prog[state.pc].rep >= re.prog[state.pc].rep_min
					&& re.prog[state.pc].rep <= re.prog[state.pc].rep_max {
					return state.first_match, state.i
				}
				// println("Not fitted!!")
			}
			// no groups open, check the last token quantifier
			if ist != regex.ist_group_end && re.prog[state.pc + 1].ist == regex.ist_prog_end {
				if re.prog[state.pc].rep >= re.prog[state.pc].rep_min
					&& re.prog[state.pc].rep <= re.prog[state.pc].rep_max {
					// println("We are in good repetition")
					return state.first_match, state.i
				}
			}

			// println("No good exit!!")
			if re.prog[re.prog_len - 1].ist == regex.ist_group_end {
				// println("last ist is a group end!")
				if re.prog[re.prog_len - 1].group_rep >= re.prog[re.prog_len - 1].rep_min {
					return state.first_match, state.i
				}
			}
			return regex.no_match_found, state.i
		}

		// starting and init
		if m_state == .start {
			state.pc = -1
			state.i = 0
			m_state = .ist_next
			continue
		}
		// ist_next, next instruction reseting its state
		else if m_state == .ist_next {
			state.pc = state.pc + 1
			re.prog[state.pc].reset()
			// check if we are in the program bounds
			if state.pc < 0 || state.pc > re.prog.len {
				// println("ERROR!! PC overflow!!")
				return regex.err_internal_error, state.i
			}
			m_state = .ist_load
			continue
		}
		// ist_next_ks, next instruction keeping its state
		else if m_state == .ist_next_ks {
			state.pc = state.pc + 1
			// check if we are in the program bounds
			if state.pc < 0 || state.pc > re.prog.len {
				// println("ERROR!! PC overflow!!")
				return regex.err_internal_error, state.i
			}
			m_state = .ist_load
			continue
		}

		// load the char
		ch, char_len = re.get_charb(in_txt, state.i)

		// check new line if flag f_nl enabled
		if (re.flag & regex.f_nl) != 0 && char_len == 1 && u8(ch) in regex.new_line_list {
			m_state = .new_line
			continue
		}
		// check if stop
		else if m_state == .stop {
			// we are in search mode, don't exit until the end
			if ((re.flag & regex.f_src) != 0) && (ist != regex.ist_prog_end) {
				last_fnd_pc = state.pc
				state.pc = -1
				state.i += char_len

				m_state = .ist_next
				re.reset_src()
				state.match_index = -1
				state.first_match = -1

				// reset state list
				re.reset()

				continue
			}

			if ist == regex.ist_prog_end {
				return state.first_match, state.i
			}

			// manage here dot char

			if re.state_list.len > 0 {
				// println("Here we are, with stop: state buffer: [${re.state_list.len}]")
				state = re.state_list.pop()

				state.match_flag = true
				l_ist = u32(regex.ist_dot_char)

				if state.first_match < 0 {
					state.first_match = state.i
				}
				state.match_index = state.i
				re.prog[state.pc].rep++ // increase repetitions

				state.i += char_len
				m_state = .ist_quant_p
				continue
			}

			// exit on no match
			return result, state.i
		}
		// ist_load
		else if m_state == .ist_load {
			// program end
			if ist == regex.ist_prog_end {
				// if we are in match exit well

				if state.group_index >= 0 && state.match_index >= 0 {
					state.group_index = -1
				}

				m_state = .stop
				continue
			}
			// check GROUP start, no quantifier is checkd for this token!!
			else if ist == regex.ist_group_start {
				state.group_index++
				re.group_data[state.group_index] = re.prog[state.pc].goto_pc // save where is ist_group_end, we will use it for escape
				re.group_stack[state.group_index] = state.i // index where we start to manage
				// println("group_index $state.group_index rep ${re.prog[re.prog[state.pc].goto_pc].group_rep}")

				m_state = .ist_next
				continue
			}
			// check GROUP end
			else if ist == regex.ist_group_end {
				// we are in matching streak
				// println("Group END!! last ist: ${l_ist:08x}")
				if state.match_index >= 0 {
					// restore txt index stack and save the group data

					// println("g.id: ${re.prog[state.pc].group_id} group_index: ${state.group_index}")
					if state.group_index >= 0 && re.prog[state.pc].group_id >= 0 {
						start_i := re.group_stack[state.group_index]

						// save group results
						g_index := re.prog[state.pc].group_id * 2

						if start_i >= 0 {
							re.groups[g_index] = start_i
						} else {
							re.groups[g_index] = 0
						}

						re.groups[g_index + 1] = state.i

						if g_index > 0 && re.groups[g_index] <= re.groups[g_index - 1] {
							re.groups[g_index] = re.groups[g_index - 1]
						}

						if re.groups[g_index + 1] >= in_txt_len {
							// println("clamp group!")
							re.groups[g_index + 1] = in_txt_len - 1
						}

						// println("GROUP ${re.prog[state.pc].group_id} END [${re.groups[g_index]}, ${re.groups[g_index+1]}] i: $state.i in_txt_len: $in_txt_len")

						// continuous save, save until we have space
						re.group_continuous_save(g_index)
					}

					re.prog[state.pc].group_rep++ // increase repetitions
					// println("GROUP $group_index END ${re.prog[state.pc].group_rep}")
					if re.prog[state.pc].group_rep > in_txt_len - 1 {
						m_state = .ist_quant_ng
						continue
					}

					m_state = .ist_quant_pg
					continue
				}

				m_state = .ist_quant_ng
				continue
			}
			// check OR
			else if ist == regex.ist_or_branch {
				if state.match_index >= 0 {
					state.pc = re.prog[state.pc].rep_max
					// println("ist_or_branch True pc: $state.pc")
				} else {
					state.pc = re.prog[state.pc].rep_min
					// println("ist_or_branch False pc: $state.pc")
				}
				re.prog[state.pc].reset()
				m_state = .ist_load
				continue
			}
			// check ist_dot_char
			else if ist == regex.ist_dot_char {
				// println("ist_dot_char rep: ${re.prog[state.pc].rep}")

				// check next token to be false
				mut next_check_flag := false

				// if we are done with max go on dot char are dedicated case!!
				if re.prog[state.pc].rep >= re.prog[state.pc].rep_max {
					re.state_list.pop()
					m_state = .ist_next
					continue
				}

				if re.prog[state.pc].last_dot_flag == false && re.prog[state.pc].dot_check_pc >= 0
					&& re.prog[state.pc].rep >= re.prog[state.pc].rep_min {
					// load the char
					// ch_t, _ := re.get_charb(in_txt, state.i+char_len)
					ch_t := ch
					chk_pc := re.prog[state.pc].dot_check_pc

					// simple char
					if re.prog[chk_pc].ist == regex.ist_simple_char {
						if re.prog[chk_pc].ch == ch_t {
							next_check_flag = true
						}
						// println("Check [ist_simple_char] [${re.prog[chk_pc].ch}]==[${ch_t:c}] => $next_check_flag")
					}
					// char char_class
					else if re.prog[chk_pc].ist == regex.ist_char_class_pos
						|| re.prog[chk_pc].ist == regex.ist_char_class_neg {
						mut cc_neg := false
						if re.prog[chk_pc].ist == regex.ist_char_class_neg {
							cc_neg = true
						}
						mut cc_res := re.check_char_class(chk_pc, ch_t)

						if cc_neg {
							cc_res = !cc_res
						}
						next_check_flag = cc_res
						// println("Check [ist_char_class] => $next_check_flag")
					}
					// check bsls
					else if re.prog[chk_pc].ist == regex.ist_bsls_char {
						next_check_flag = re.prog[chk_pc].validator(u8(ch_t))
						// println("Check [ist_bsls_char] => $next_check_flag")
					}
				}

				// check if we must continue or pass to the next IST
				if next_check_flag == true && re.prog[state.pc + 1].ist != regex.ist_prog_end {
					// println("save the state!!")
					mut dot_state := StateObj{
						group_index: state.group_index
						match_flag: state.match_flag
						match_index: state.match_index
						first_match: state.first_match
						pc: state.pc
						i: state.i + char_len
						char_len: char_len
						last_dot_pc: state.pc
					}
					// if we are mananging a .* stay on the same char on return
					if re.prog[state.pc].rep_min == 0 {
						dot_state.i -= char_len
					}

					re.state_list << dot_state

					m_state = .ist_quant_n
					// println("dot_char stack len: ${re.state_list.len}")
					continue
				}

				state.match_flag = true
				l_ist = u32(regex.ist_dot_char)

				if state.first_match < 0 {
					state.first_match = state.i
				}
				state.match_index = state.i
				re.prog[state.pc].rep++ // increase repetitions

				state.i += char_len
				m_state = .ist_quant_p
				continue
			}
			// char class IST
			else if ist == regex.ist_char_class_pos || ist == regex.ist_char_class_neg {
				state.match_flag = false
				mut cc_neg := false

				if ist == regex.ist_char_class_neg {
					cc_neg = true
				}
				mut cc_res := re.check_char_class(state.pc, ch)

				if cc_neg {
					cc_res = !cc_res
				}

				if cc_res {
					state.match_flag = true
					l_ist = u32(regex.ist_char_class_pos)

					if state.first_match < 0 {
						state.first_match = state.i
					}

					state.match_index = state.i

					re.prog[state.pc].rep++ // increase repetitions
					state.i += char_len // next char
					m_state = .ist_quant_p
					continue
				}
				m_state = .ist_quant_n
				continue
			}
			// check bsls
			else if ist == regex.ist_bsls_char {
				// println("ist_bsls_char rep: ${re.prog[state.pc].rep}")

				// check next token to be false
				mut next_check_flag := false

				// if we are done with max go on dot char are dedicated case!!
				if re.prog[state.pc].rep >= re.prog[state.pc].rep_max {
					re.state_list.pop()
					m_state = .ist_next
					continue
				}

				if re.prog[state.pc].last_dot_flag == false && re.prog[state.pc].bsls_check_pc >= 0
					&& re.prog[state.pc].rep >= re.prog[state.pc].rep_min {
					// load the char
					// ch_t, _ := re.get_charb(in_txt, state.i+char_len)
					ch_t := ch
					chk_pc := re.prog[state.pc].bsls_check_pc

					// simple char
					if re.prog[chk_pc].ist == regex.ist_simple_char {
						if re.prog[chk_pc].ch == ch_t {
							next_check_flag = true
						}
						// println("Check [ist_simple_char] [${re.prog[chk_pc].ch}]==[${ch_t:c}] => $next_check_flag")
					}
					// char char_class
					else if re.prog[chk_pc].ist == regex.ist_char_class_pos
						|| re.prog[chk_pc].ist == regex.ist_char_class_neg {
						mut cc_neg := false
						if re.prog[chk_pc].ist == regex.ist_char_class_neg {
							cc_neg = true
						}
						mut cc_res := re.check_char_class(chk_pc, ch_t)

						if cc_neg {
							cc_res = !cc_res
						}
						next_check_flag = cc_res
						// println("Check [ist_char_class] => $next_check_flag")
					}
					// check bsls
					else if re.prog[chk_pc].ist == regex.ist_bsls_char {
						next_check_flag = re.prog[chk_pc].validator(u8(ch_t))
						// println("Check [ist_bsls_char] => $next_check_flag")
					}
				}

				// check if we must continue or pass to the next IST
				if next_check_flag == true && re.prog[state.pc + 1].ist != regex.ist_prog_end {
					// println("save the state!!")
					mut dot_state := StateObj{
						group_index: state.group_index
						match_flag: state.match_flag
						match_index: state.match_index
						first_match: state.first_match
						pc: state.pc
						i: state.i + char_len
						char_len: char_len
						last_dot_pc: state.pc
					}
					// if we are managing a \[something]* stay on the same char on return
					if re.prog[state.pc].rep_min == 0 {
						dot_state.i -= char_len
					}

					re.state_list << dot_state

					m_state = .ist_quant_n
					// println("dot_char stack len: ${re.state_list.len}")
					continue
				}

				tmp_res := re.prog[state.pc].validator(u8(ch))
				if tmp_res == false {
					m_state = .ist_quant_n
					continue
				}
				// println("${ch} => ${tmp_res}")

				state.match_flag = true
				l_ist = u32(regex.ist_dot_char)

				if state.first_match < 0 {
					state.first_match = state.i
				}
				state.match_index = state.i
				re.prog[state.pc].rep++ // increase repetitions

				state.i += char_len
				m_state = .ist_quant_p
				continue
			}
			// simple char IST
			else if ist == regex.ist_simple_char {
				// println("ist_simple_char")
				state.match_flag = false

				if re.prog[state.pc].ch == ch {
					state.match_flag = true
					l_ist = regex.ist_simple_char

					if state.first_match < 0 {
						state.first_match = state.i
					}
					// println("state.match_index: ${state.match_index}")
					state.match_index = state.i

					re.prog[state.pc].rep++ // increase repetitions
					state.i += char_len // next char
					m_state = .ist_quant_p
					continue
				}
				m_state = .ist_quant_n
				continue
			}
			// UNREACHABLE
			// println("PANIC2!! state: $m_state")
			return regex.err_internal_error, state.i
		}
		/***********************************
		* Quantifier management
		***********************************/
		// ist_quant_ng => quantifier negative test on group
		else if m_state == .ist_quant_ng {
			// we are finished here
			if state.group_index < 0 {
				// println("Early stop!")
				result = regex.no_match_found
				m_state = .stop
				continue
			}

			tmp_pc := re.group_data[state.group_index] // PC to the end of the group token
			rep := re.prog[tmp_pc].group_rep // use a temp variable
			re.prog[tmp_pc].group_rep = 0 // clear the repetitions

			// println(".ist_quant_ng group_pc_end: $tmp_pc rep: $rep")

			if rep >= re.prog[tmp_pc].rep_min {
				// println("ist_quant_ng GROUP CLOSED OK group_index: $state.group_index")

				state.i = re.group_stack[state.group_index]
				state.pc = tmp_pc
				state.group_index--
				m_state = .ist_next
				continue
			} else if re.prog[tmp_pc].next_is_or {
				// println("ist_quant_ng OR Negative branch")

				state.i = re.group_stack[state.group_index]
				state.pc = re.prog[tmp_pc + 1].rep_min - 1
				state.group_index--
				m_state = .ist_next
				continue
			} else if rep > 0 && rep < re.prog[tmp_pc].rep_min {
				// println("ist_quant_ng UNDER THE MINIMUM g.i: $state.group_index")

				// check if we are inside a group, if yes exit from the nested groups
				if state.group_index > 0 {
					state.group_index--
					state.pc = tmp_pc
					m_state = .ist_quant_ng //.ist_next
					continue
				}

				if state.group_index == 0 {
					state.group_index--
					state.pc = tmp_pc // TEST
					m_state = .ist_next
					continue
				}

				result = regex.no_match_found
				m_state = .stop
				continue
			} else if rep == 0 && rep < re.prog[tmp_pc].rep_min {
				// println("ist_quant_ng c_zero UNDER THE MINIMUM g.i: $state.group_index")

				if state.group_index > 0 {
					state.group_index--
					state.pc = tmp_pc
					m_state = .ist_quant_ng //.ist_next
					continue
				}

				result = regex.no_match_found
				m_state = .stop
				continue
			}

			// println("DO NOT STAY HERE!! {${re.prog[tmp_pc].rep_min},${re.prog[tmp_pc].rep_max}}:$rep")
			// UNREACHABLE
			return regex.err_internal_error, state.i
		}
		// ist_quant_pg => quantifier positive test on group
		else if m_state == .ist_quant_pg {
			// println(".ist_quant_pg")
			mut tmp_pc := state.pc
			if state.group_index >= 0 {
				tmp_pc = re.group_data[state.group_index]
			}

			if re.prog[tmp_pc].group_neg == true {
				// println("***** Negation of the group")
				result = regex.no_match_found
				m_state = .stop
				continue
			}

			rep := re.prog[tmp_pc].group_rep

			if rep < re.prog[tmp_pc].rep_min {
				// println("ist_quant_pg UNDER RANGE")
				state.pc = re.prog[tmp_pc].goto_pc
				m_state = .ist_next
				continue
			} else if rep == re.prog[tmp_pc].rep_max {
				// println("ist_quant_pg MAX RANGE")
				re.prog[tmp_pc].group_rep = 0 // clear the repetitions
				state.group_index--
				m_state = .ist_next

				continue
			} else if rep >= re.prog[tmp_pc].rep_min {
				// println("ist_quant_pg IN RANGE group_index:$state.group_index")

				// check greedy flag, if true exit on minimum
				if re.prog[tmp_pc].greedy == true {
					re.prog[tmp_pc].group_rep = 0 // clear the repetitions
					state.group_index--
					m_state = .ist_next
					continue
				}

				state.pc = re.prog[tmp_pc].goto_pc - 1
				state.group_index--
				m_state = .ist_next
				continue
			}

			// UNREACHABLE
			// println("PANIC3!! state: $m_state")
			return regex.err_internal_error, state.i
		}
		// ist_quant_n => quantifier negative test on token
		else if m_state == .ist_quant_n {
			rep := re.prog[state.pc].rep
			// println("Here!! PC $state.pc is_next_or: ${re.prog[state.pc].next_is_or}")

			// zero quantifier * or ?
			if rep == 0 && re.prog[state.pc].rep_min == 0 {
				// println("ist_quant_n c_zero RANGE MIN")
				m_state = .ist_next // go to next ist
				continue
			}
			// match + or *
			else if rep >= re.prog[state.pc].rep_min {
				// println("ist_quant_n MATCH RANGE")
				m_state = .ist_next
				continue
			}

			// check the OR if present
			if re.prog[state.pc].next_is_or {
				// println("OR present on failing")
				state.match_index = -1
				m_state = .ist_next
				continue
			}

			// we are in a group manage no match from here
			if state.group_index >= 0 {
				// println("ist_quant_n FAILED insied a GROUP group_index:$state.group_index")
				m_state = .ist_quant_ng
				continue
			}

			// no other options
			// println("ist_quant_n no_match_found")
			result = regex.no_match_found
			m_state = .stop
			continue
			// return no_match_found, 0
		}
		// ist_quant_p => quantifier positive test on token
		else if m_state == .ist_quant_p {
			// println("Here .ist_quant_p")
			// exit on first match
			if (re.flag & regex.f_efm) != 0 {
				return state.i, state.i + 1
			}

			rep := re.prog[state.pc].rep
			// println(rep)

			// under range
			if rep > 0 && rep < re.prog[state.pc].rep_min {
				// println("ist_quant_p UNDER RANGE")
				m_state = .ist_load // continue the loop
				continue
			}
			// range ok, continue loop
			else if rep >= re.prog[state.pc].rep_min && rep < re.prog[state.pc].rep_max {
				// println("ist_quant_p IN RANGE")

				// check greedy flag, if true exit on minimum
				if re.prog[state.pc].greedy == true {
					m_state = .ist_next
					continue
				}
				m_state = .ist_load
				continue
			}
			// max reached
			else if rep == re.prog[state.pc].rep_max {
				// println("ist_quant_p MAX RANGE")
				m_state = .ist_next
				continue
			}
		}
		// UNREACHABLE
		// println("PANIC4!! state: $m_state")
		return regex.err_internal_error, state.i
	}

	// println("Check end of text!")
	// Check the results
	if state.match_index >= 0 {
		if state.group_index < 0 {
			if re.prog[state.pc].ist == regex.ist_prog_end {
				// println("program ended!!")

				if (re.flag & regex.f_src) != 0 {
					// println("find return")
					return state.first_match, state.i
				} else {
					// println("Here!!")
					return 0, state.i
				}
			}

			// println("No Group here, natural end [$state.first_match,$state.i] state: ${state_str(m_state)} ist: $ist pgr_end: $re.prog.len")

			if re.prog[state.pc + 1].ist == regex.ist_prog_end
				|| re.prog[state.pc].ist == regex.ist_prog_end {
				rep := re.prog[state.pc].rep
				// println("rep: $rep re.prog[state.pc].rep_min: ${re.prog[state.pc].rep_min} re.prog[state.pc].rep_max: ${re.prog[state.pc].rep_max}")
				if rep >= re.prog[state.pc].rep_min && rep <= re.prog[state.pc].rep_max {
					return state.first_match, state.i
				}
				// println("Program not finished! ")
				return regex.no_match_found, state.i
			}
			if src_end {
				// println("program end")
				return state.first_match, state.i
			}
			// print("No match found!!")
			return regex.no_match_found, state.i
		} else {
			// println("Group match! OK")
			// println("first_match: $state.first_match, i: $state.i")

			// println("Skip last group")
			return state.first_match, state.i
			// return state.first_match,re.group_stack[state.group_index--]
		}
	}
	// println("no_match_found, natural end")
	return regex.no_match_found, state.i
}
