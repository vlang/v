/**********************************************************************
*
* regex 0.9d
*
* Copyright (c) 2019-2020 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* This file contains regex module
*
* Know limitation:
* - find is implemented in a trivial way
* - not full compliant PCRE
* - not compliant POSIX ERE
*
*
**********************************************************************/
module regex
import strings

pub const(
	V_REGEX_VERSION = "0.9d"      // regex module version

	MAX_CODE_LEN     = 256        // default small base code len for the regex programs
	MAX_QUANTIFIER   = 1073741824 // default max repetitions allowed for the quantifiers = 2^30

	// spaces chars (here only westerns!!) TODO: manage all the spaces from unicode
	SPACES = [` `, `\t`, `\n`, `\r`, `\v`, `\f`]
	// new line chars for now only '\n'
	NEW_LINE_LIST = [`\n`,`\r`]

	// Results
	NO_MATCH_FOUND          = -1
	
	// Errors
	COMPILE_OK              =  0   // the regex string compiled, all ok
	ERR_CHAR_UNKNOWN        = -2   // the char used is unknow to the system
	ERR_UNDEFINED           = -3   // the compiler symbol is undefined
	ERR_INTERNAL_ERROR      = -4   // Bug in the regex system!!
	ERR_CC_ALLOC_OVERFLOW   = -5   // memory for char class full!!
	ERR_SYNTAX_ERROR        = -6   // syntax error in regex compiling
	ERR_GROUPS_OVERFLOW     = -7   // max number of groups reached
	ERR_GROUPS_MAX_NESTED   = -8   // max number of nested group reached
	ERR_GROUP_NOT_BALANCED  = -9   // group not balanced
	ERR_GROUP_QM_NOTATION   = -10  // group invalid notation
)

const(
	//*************************************
	// regex program instructions
	//*************************************
	IST_SIMPLE_CHAR  = u32(0x7FFFFFFF)   // single char instruction, 31 bit available to char

	// char class 11 0100 AA xxxxxxxx
	// AA = 00  regular class
	// AA = 01  Negated class ^ char
	IST_CHAR_CLASS       = 0xD1000000   // MASK
	IST_CHAR_CLASS_POS   = 0xD0000000   // char class normal [abc]
	IST_CHAR_CLASS_NEG   = 0xD1000000   // char class negate [^abc]

	// dot char        10 0110 xx xxxxxxxx
	IST_DOT_CHAR         = 0x98000000   // match any char except \n

	// backslash chars 10 0100 xx xxxxxxxx
	IST_BSLS_CHAR        = 0x90000000   // backslash char

	// OR |            10 010Y xx xxxxxxxx
	IST_OR_BRANCH        = 0x91000000   // OR case

	// groups          10 010Y xx xxxxxxxx
	IST_GROUP_START      = 0x92000000   // group start (
	IST_GROUP_END        = 0x94000000   // group end   )

	// control instructions
	IST_PROG_END         = u32(0x88000000)      //10 0010 xx xxxxxxxx 
	//*************************************
)

/******************************************************************************
*
* General Utilities
*
******************************************************************************/
// utf8util_char_len calculate the length in bytes of a utf8 char
[inline]
fn utf8util_char_len(b byte) int {
	return (( 0xe5000000 >> (( b >> 3 ) & 0x1e )) & 3 ) + 1
}

// get_char get a char from position i and return an u32 with the unicode code
[inline]
fn (re RE) get_char(in_txt string, i int) (u32,int) {
	// ascii 8 bit
	if (re.flag & F_BIN) !=0 ||
		in_txt.str[i] & 0x80 == 0 
	{
		return u32(in_txt.str[i]), 1 
	}
	// unicode char
	char_len := utf8util_char_len(in_txt.str[i])
	mut tmp := 0
	mut ch := u32(0)
	for tmp < char_len {
		ch = (ch << 8) | in_txt.str[i+tmp]
		tmp++
	}
	return ch,char_len
}

// get_charb get a char from position i and return an u32 with the unicode code
[inline]
fn (re RE) get_charb(in_txt byteptr, i int) (u32,int) {
	// ascii 8 bit 
	if (re.flag & F_BIN) !=0 ||
		in_txt[i] & 0x80 == 0
	{
		return u32(in_txt[i]), 1 
	}
	// unicode char
	char_len := utf8util_char_len(in_txt[i])
	mut tmp := 0
	mut ch := u32(0)
	for tmp < char_len {
		ch = (ch << 8) | in_txt[i+tmp]
		tmp++
	}
	return ch,char_len
}

[inline]
fn is_alnum(in_char byte) bool {
	mut tmp := in_char - `A`
	if tmp >= 0x00 && tmp <= 25 { return true }
	tmp = in_char - `a`
	if tmp >= 0x00 && tmp <= 25 { return true }
	tmp = in_char - `0`
	if tmp >= 0x00 && tmp <= 9  { return true }
	if tmp == `_` { return true }
	return false
}

[inline]
fn is_not_alnum(in_char byte) bool {
	return !is_alnum(in_char)
}

[inline]
fn is_space(in_char byte) bool {
	return in_char in SPACES
}

[inline]
fn is_not_space(in_char byte) bool {
	return !is_space(in_char)
}

[inline]
fn is_digit(in_char byte) bool {
	tmp := in_char - `0`
	return tmp <= 0x09 && tmp >= 0
}

[inline]
fn is_not_digit(in_char byte) bool {
	return !is_digit(in_char)
}

[inline]
fn is_wordchar(in_char byte) bool {
	return is_alnum(in_char) || in_char == `_`
}

[inline]
fn is_not_wordchar(in_char byte) bool {
	return !is_alnum(in_char)
}

[inline]
fn is_lower(in_char byte) bool {
	tmp := in_char - `a`
	return  tmp >= 0x00 && tmp <= 25
}

[inline]
fn is_upper(in_char byte) bool {
	tmp := in_char - `A`
	return  tmp >= 0x00 && tmp <= 25
}

pub fn (re RE) get_parse_error_string(err int) string {
	match err {
		COMPILE_OK             { return "COMPILE_OK" }
		NO_MATCH_FOUND         { return "NO_MATCH_FOUND" }
		ERR_CHAR_UNKNOWN       { return "ERR_CHAR_UNKNOWN" }      
		ERR_UNDEFINED          { return "ERR_UNDEFINED" } 
		ERR_INTERNAL_ERROR     { return "ERR_INTERNAL_ERROR" }
		ERR_CC_ALLOC_OVERFLOW  { return "ERR_CC_ALLOC_OVERFLOW" }
		ERR_SYNTAX_ERROR       { return "ERR_SYNTAX_ERROR" }
		ERR_GROUPS_OVERFLOW    { return "ERR_GROUPS_OVERFLOW" }
		ERR_GROUPS_MAX_NESTED  { return "ERR_GROUPS_MAX_NESTED" }
		ERR_GROUP_NOT_BALANCED { return "ERR_GROUP_NOT_BALANCED" }
		ERR_GROUP_QM_NOTATION  { return "ERR_GROUP_QM_NOTATION" }
		else { return "ERR_UNKNOWN" }
	}
}

// utf8_str convert and utf8 sequence to a printable string
[inline]
fn utf8_str(ch u32) string {
	mut i := 4
	mut res := ""
	for i > 0 {
		v := byte((ch >> ((i-1)*8)) & 0xFF)
		if v != 0{
			res += "${v:1c}"
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
struct Token{
mut:
	ist u32 = u32(0)

	// char
	ch u32                 = u32(0)  // char of the token if any
	ch_len byte            = byte(0) // char len

	// Quantifiers / branch
	rep_min         int    = 0     // used also for jump next in the OR branch [no match] pc jump
	rep_max         int    = 0     // used also for jump next in the OR branch [   match] pc jump
	greedy          bool   = false // greedy quantifier flag

	// Char class
	cc_index        int    = -1

	// counters for quantifier check (repetitions)
	rep             int    = 0

	// validator function pointer
	validator fn (byte) bool

	// groups variables
	group_rep          int = 0     // repetition of the group
	group_id           int = -1    // id of the group
	goto_pc            int = -1    // jump to this PC if is needed

	// OR flag for the token 
	next_is_or bool        = false // true if the next token is an OR
}

[inline]
fn (tok mut Token) reset() {
	tok.rep = 0
}

/******************************************************************************
*
* Regex struct 
*
******************************************************************************/
pub const (
	F_NL  = 0x00000001  // end the match when find a new line symbol
	F_MS  = 0x00000002  // match true only if the match is at the start of the string
	F_ME  = 0x00000004  // match true only if the match is at the end of the string 

	F_EFM = 0x00000100  // exit on first token matched, used by search
	F_BIN = 0x00000200  // work only on bytes, ignore utf-8

	// behaviour modifier flags
	//F_OR  = 0x00010000  // the OR work with concatenation like PCRE
	F_SRC = 0x00020000  // search mode enabled
)

struct StateDotObj{
mut:
	i  int                = -1  // char index in the input buffer
	pc int                = -1  // program counter saved
	mi int                = -1  // match_index saved
	group_stack_index int = -1  // continuous save on capturing groups
}

pub
struct RE {
pub mut:
	prog []Token

	// char classes storage
	cc []CharClass             // char class list
	cc_index int         = 0   // index

	// state index
	state_stack_index int= -1
	state_stack []StateDotObj
	

	// groups
	group_count int      = 0   // number of groups in this regex struct
	groups []int               // groups index results
	group_max_nested int = 3   // max nested group
	group_max int        = 8   // max allowed number of different groups

	group_csave []int    = []int  // groups continuous save array
	group_csave_index int= -1     // groups continuous save index

	group_map map[string]int      // groups names map

	// flags
	flag int             = 0   // flag for optional parameters

	// Debug/log
	debug int            = 0   // enable in order to have the unroll of the code 0 = NO_DEBUG, 1 = LIGHT 2 = VERBOSE
	log_func fn (string) = simple_log  // log function, can be customized by the user
	query string         = ""  // query string
}

// Reset RE object
//[inline] 
fn (re mut RE) reset(){
	re.cc_index         = 0
	
	mut i := 0
	for i < re.prog.len {
		re.prog[i].group_rep          = 0 // clear repetition of the group
		re.prog[i].rep                = 0 // clear repetition of the token
		i++
	}
	re.groups = [-1].repeat(re.group_count*2)

	re.state_stack_index = -1

	// reset group_csave
	if re.group_csave.len > 0 {
		re.group_csave_index = 1
		re.group_csave[0] = 0     // reset the capture count
	}
}

// reset for search mode fail
// gcc bug, dont use [inline] or go 5 time slower
fn (re mut RE) reset_src(){
	mut i := 0
	for i < re.prog.len {
		re.prog[i].group_rep          = 0 // clear repetition of the group
		re.prog[i].rep                = 0 // clear repetition of the token
		i++
	}
	re.state_stack_index = -1
}

pub fn (re RE) get_group(group_name string) (int, int) {
	if group_name in re.group_map {
		tmp_index := re.group_map[group_name]-1
		start := re.groups[tmp_index*2]
		end := re.groups[tmp_index*2+1]
		return start,end
	}
	return -1, -1
}

/******************************************************************************
*
* Backslashes chars
*
******************************************************************************/
struct BslsStruct {
	ch u32                   // meta char
	validator fn (byte) bool // validator function pointer
}

const(
	BSLS_VALIDATOR_ARRAY = [
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
	BSLS_ESCAPE_LIST = [ `\\`,`|`,`.`,`*`,`+`,`{`,`}`,`[`,`]` ]
)

enum BSLS_parse_state {
		start,
		bsls_found,
		bsls_char,
		normal_char
}

// parse_bsls return (index, str_len) BSLS_VALIDATOR_ARRAY index, len of the backslash sequence if present
fn (re RE) parse_bsls(in_txt string, in_i int) (int,int){
	mut status := BSLS_parse_state.start
	mut i := in_i

	for i < in_txt.len {
		// get our char
		char_tmp,char_len := re.get_char(in_txt,i)
		ch := byte(char_tmp)

		if status == .start && ch == `\\` {
			status = .bsls_found
			i += char_len
			continue
		}

		// check if is our bsls char, for now only one length sequence
		if status == .bsls_found {
			for c,x in BSLS_VALIDATOR_ARRAY {
				if x.ch == ch {
					return c,i-in_i+1
				}
			}
			status = .normal_char
			continue
		}

		// no BSLS validator, manage as normal escape char char
		if status == .normal_char {
			if ch in BSLS_ESCAPE_LIST {
				return NO_MATCH_FOUND,i-in_i+1
			}
			return ERR_SYNTAX_ERROR,i-in_i+1
		}

		// at the present time we manage only one char after the \
		break

	}
	// not our bsls return KO
	return ERR_SYNTAX_ERROR, i
}

/******************************************************************************
*
* Char class
*
******************************************************************************/
const(
	CC_NULL = 0    // empty cc token
	CC_CHAR = 1    // simple char: a
	CC_INT  = 2    // char interval: a-z
	CC_BSLS = 3    // backslash char
	CC_END  = 4    // cc sequence terminator
)

struct CharClass {
mut:
	cc_type int = CC_NULL      // type of cc token
	ch0 u32     = u32(0)       // first char of the interval a-b  a in this case
	ch1 u32     = u32(0)	   // second char of the interval a-b b in this case
	validator fn (byte) bool   // validator function pointer
}

enum CharClass_parse_state {
	start,
	in_char,
	in_bsls,
	separator,
	finish,
}

fn (re RE) get_char_class(pc int) string {
	buf := [byte(0)].repeat(re.cc.len)
	mut buf_ptr := *byte(&buf)

	mut cc_i := re.prog[pc].cc_index
	mut i := 0
	mut tmp := 0
	for cc_i >= 0 && cc_i < re.cc.len && re.cc[cc_i].cc_type != CC_END {
				
		if re.cc[cc_i].cc_type == CC_BSLS {
			buf_ptr[i++] = `\\`
			buf_ptr[i++] = byte(re.cc[cc_i].ch0)
		}
		else if re.cc[cc_i].ch0 == re.cc[cc_i].ch1 {
			tmp = 3
			for tmp >= 0 {
				x := byte((re.cc[cc_i].ch0 >> (tmp*8)) & 0xFF)
				if x != 0 { 
					buf_ptr[i++] = x
				}
				tmp--
			}
		}
		else {
			tmp = 3
			for tmp >= 0 {
				x := byte((re.cc[cc_i].ch0 >> (tmp*8)) & 0xFF)
				if x != 0 { 
					buf_ptr[i++] = x
				}
				tmp--
			}
			buf_ptr[i++] = `-`
			tmp = 3
			for tmp >= 0 {
				x := byte((re.cc[cc_i].ch1 >> (tmp*8)) & 0xFF)
				if x != 0 { 
					buf_ptr[i++] = x
				}
				tmp--
			}
		}
		cc_i++
	}
	buf_ptr[i] = byte(0)
		
	return tos_clone( buf_ptr )
}

fn (re RE) check_char_class(pc int, ch u32) bool {
	mut cc_i := re.prog[pc].cc_index
	for cc_i >= 0 && cc_i < re.cc.len && re.cc[cc_i].cc_type != CC_END {
		if re.cc[cc_i].cc_type == CC_BSLS {
			if re.cc[cc_i].validator(byte(ch)) {
				return true
			}
		}
		else if ch >= re.cc[cc_i].ch0 && ch <= re.cc[cc_i].ch1 {
			return true
		}
		cc_i++
	}
	return false
}

// parse_char_class return (index, str_len, cc_type) of a char class [abcm-p], char class start after the [ char
fn (re mut RE) parse_char_class(in_txt string, in_i int) (int, int, u32) {
	mut status := CharClass_parse_state.start
	mut i := in_i

	mut tmp_index := re.cc_index
	res_index := re.cc_index

	mut cc_type := u32(IST_CHAR_CLASS_POS)

	for i < in_txt.len {

		// check if we are out of memory for char classes
		if tmp_index >= re.cc.len {
			return ERR_CC_ALLOC_OVERFLOW,0,u32(0) 
		}

		// get our char
		char_tmp,char_len := re.get_char(in_txt,i)
		ch := byte(char_tmp)

		//C.printf("CC #%3d ch: %c\n",i,ch)

		// negation
		if status == .start && ch == `^` {
			cc_type = u32(IST_CHAR_CLASS_NEG)
			i += char_len
			continue
		}

		// bsls
		if (status == .start || status == .in_char) && ch == `\\` {
			//C.printf("CC bsls.\n")
			status = .in_bsls
			i += char_len
			continue
		}

		if status == .in_bsls {
			//C.printf("CC bsls validation.\n")
			for c,x in BSLS_VALIDATOR_ARRAY {
				if x.ch == ch {
					//C.printf("CC bsls found \\%c.\n",ch)
					re.cc[tmp_index].cc_type   = CC_BSLS
					re.cc[tmp_index].ch0       = BSLS_VALIDATOR_ARRAY[c].ch
					re.cc[tmp_index].ch1       = BSLS_VALIDATOR_ARRAY[c].ch
					re.cc[tmp_index].validator = BSLS_VALIDATOR_ARRAY[c].validator
					i += char_len
					tmp_index++
					status = .in_char
					break
				}
			}
			if status == .in_bsls {
				//C.printf("CC bsls not found \\%c.\n",ch)
				status = .in_char
			}else {
				continue
			}
		}

		// simple char
		if (status == .start || status == .in_char) && 
			ch != `-` && ch != `]` 
		{
			status = .in_char
			
			re.cc[tmp_index].cc_type = CC_CHAR
			re.cc[tmp_index].ch0     = char_tmp
			re.cc[tmp_index].ch1     = char_tmp

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
			re.cc[tmp_index-1].cc_type = CC_INT
			re.cc[tmp_index-1].ch1     = char_tmp
			i += char_len
			continue
		}

		// char class end
		if status == .in_char && ch == `]` {
			re.cc[tmp_index].cc_type = CC_END
			re.cc[tmp_index].ch0     = 0
			re.cc[tmp_index].ch1     = 0
			re.cc_index = tmp_index+1
			
			return res_index, i-in_i+2, cc_type
		}

		i++
	}
	return ERR_SYNTAX_ERROR,0,u32(0)
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
	start,
	min_parse,
	comma_checked,
	max_parse,
	greedy,
	gredy_parse,
	finish
}

// parse_quantifier return (min, max, str_len, greedy_flag) of a {min,max}? quantifier starting after the { char
fn (re RE) parse_quantifier(in_txt string, in_i int) (int, int, int, bool) {
	mut status := Quant_parse_state.start
	mut i := in_i

	mut q_min := 0 // default min in a {} quantifier is 1
	mut q_max := 0 // deafult max in a {} quantifier is MAX_QUANTIFIER

	mut ch := byte(0)

	for i < in_txt.len {
		ch = in_txt.str[i]
		
		//C.printf("%c status: %d\n",ch,status)

		// exit on no compatible char with {} quantifier
		if utf8util_char_len(ch) != 1 {
			return ERR_SYNTAX_ERROR,i,0,false
		}

		// min parsing skip if comma present
		if status == .start && ch == `,` {
			q_min = 0 // default min in a {} quantifier is 0
			status = .comma_checked
			i++
			continue
		}

		if status == .start && is_digit( ch ) {
			status = .min_parse
			q_min *= 10
			q_min += int(ch - `0`)
			i++
			continue
		}

		if status == .min_parse && is_digit( ch ) {
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
			q_max = MAX_QUANTIFIER

			status = .greedy
			continue
		}

		// start max parsing
		if status == .comma_checked && is_digit( ch ) {
			status = .max_parse
			q_max *= 10
			q_max += int(ch - `0`)
			i++
			continue
		}

		// parse the max
		if status == .max_parse && is_digit( ch ) {
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
			if i+1 < in_txt.len {
				i++
				status = .gredy_parse
				continue
			}
			return q_min, q_max, i-in_i+2, false
		}

		// check the greedy flag
		if status == .gredy_parse {
			if ch == `?` {
				return q_min, q_max, i-in_i+2, true
			} else {
				i--
				return q_min, q_max, i-in_i+2, false
			}
		}

		// not  a {} quantifier, exit
		return ERR_SYNTAX_ERROR, i, 0, false
	}

	// not a conform {} quantifier
	return ERR_SYNTAX_ERROR, i, 0, false
}

//
// Groups
//
enum Group_parse_state {
	start,
	q_mark,      // (?
	q_mark1,     // (?:|P  checking
	p_status,    // (?P
	p_start,     // (?P<
	p_end,       // (?P<...>
	p_in_name,   // (?P<...	
	finish
}

// parse_groups parse a group for ? (question mark) syntax, if found, return (error, capture_flag, name_of_the_group, next_index)
fn (re RE) parse_groups(in_txt string, in_i int) (int, bool, string, int) {
	mut status := Group_parse_state.start
	mut i := in_i
	mut name := ''

	for i < in_txt.len && status != .finish {

		// get our char
		char_tmp,char_len := re.get_char(in_txt,i)
		ch := byte(char_tmp)

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

		// non capturing group
		if status == .q_mark1 && ch == `:` {
			i += char_len
			return 0, false, name, i
		}

		// enter in P section
		if status == .q_mark1 && ch == `P` {
			status = .p_status
			i += char_len
			continue
		}

		// not a valid q mark found
		if status == .q_mark1 {
			//println("NO VALID Q MARK")
			return -2 , true, name, i
		}

		if status == .p_status && ch == `<` {
			status = .p_start
			i += char_len
			continue
		}

		if status == .p_start && ch != `>` {
			status = .p_in_name
			name += "${ch:1c}" // TODO: manage utf8 chars
			i += char_len
			continue
		}

		// colect name
		if status == .p_in_name && ch != `>` && is_alnum(ch) {
			name += "${ch:1c}" // TODO: manage utf8 chars
			i += char_len
			continue
		}

		// end name
		if status == .p_in_name && ch == `>` {
			i += char_len
			return 0, true, name, i
		}

		// error on name group
		if status == .p_in_name {
			return -2 , true, name, i
		}

		// normal group, nothig to do, exit
		return  0 , true, name, i
	}
	/* UNREACHABLE */
	//println("ERROR!! NOT MEANT TO BE HERE!!1")
	return -2 , true, name, i
}

//
// main compiler
//
// compile return (return code, index) where index is the index of the error in the query string if return code is an error code
pub fn (re mut RE) compile(in_txt string) (int,int) {
	mut i        := 0      // input string index
	mut pc       := 0      // program counter
	mut tmp_code := u32(0)

	// group management variables
	mut group_count           := -1
	mut group_stack           := [0 ].repeat(re.group_max_nested)
	mut group_stack_txt_index := [-1].repeat(re.group_max_nested)
	mut group_stack_index     := -1

	re.query = in_txt      // save the query string

	i = 0
	for i < in_txt.len {
		tmp_code = u32(0)
		mut char_tmp := u32(0)
		mut char_len := 0
		//C.printf("i: %3d ch: %c\n", i, in_txt.str[i])

		char_tmp,char_len = re.get_char(in_txt,i)

		//
		// check special cases: $ ^
		//
		if char_len == 1 && i == 0 && byte(char_tmp) == `^` {
			re.flag = F_MS
			i = i + char_len
			continue
		}
		if char_len == 1 && i == (in_txt.len-1) && byte(char_tmp) == `$` {
			re.flag = F_ME
			i = i + char_len
			continue
		}

		// IST_GROUP_START
		if char_len == 1 && pc >= 0 && byte(char_tmp) == `(` {
			
			//check max groups allowed
			if group_count > re.group_max {
				return ERR_GROUPS_OVERFLOW,i+1
			}
			group_stack_index++

			// check max nested groups allowed
			if group_stack_index > re.group_max_nested {
				return ERR_GROUPS_MAX_NESTED,i+1
			}

			tmp_res, cgroup_flag, cgroup_name, next_i := re.parse_groups(in_txt,i)
			
			// manage question mark format error
			if tmp_res < -1 {
				return ERR_GROUP_QM_NOTATION,next_i
			}

			//println("Parse group: [$tmp_res, $cgroup_flag, ($i,$next_i), '${in_txt[i..next_i]}' ]")
			i = next_i

			if cgroup_flag == true {
				group_count++
			}

			// calculate the group id
			// if it is a named group, recycle the group id
			// NOTE: **** the group index is +1 because map return 0 when not found!! ****
			mut group_id := group_count
			if cgroup_name.len > 0 {
				//println("GROUP NAME: ${cgroup_name}")
				if cgroup_name in re.group_map{
					group_id = re.group_map[cgroup_name]-1
					group_count--
				} else {
					re.group_map[cgroup_name] = group_id+1
				}
			}

			group_stack_txt_index[group_stack_index] = i
			group_stack[group_stack_index] = pc

			re.prog[pc].ist = u32(0) | IST_GROUP_START
			re.prog[pc].rep_min = 1
			re.prog[pc].rep_max = 1
			
			// set the group id
			if cgroup_flag == false {
				//println("NO CAPTURE GROUP")
				re.prog[pc].group_id = -1 
			} else {
				re.prog[pc].group_id = group_id
			}

			pc = pc + 1
			continue

		}

		// IST_GROUP_END
		if char_len==1 && pc > 0 && byte(char_tmp) == `)` {
			if group_stack_index < 0 {
				return ERR_GROUP_NOT_BALANCED,i+1
			}

			goto_pc := group_stack[group_stack_index]
			group_stack_index--

			re.prog[pc].ist = u32(0) | IST_GROUP_END
			re.prog[pc].rep_min = 1
			re.prog[pc].rep_max = 1

			re.prog[pc].goto_pc = goto_pc			          // PC where to jump if a group need
			re.prog[pc].group_id = re.prog[goto_pc].group_id  // id of this group, used for storing data
			
			re.prog[goto_pc].goto_pc = pc                     // start goto point to the end group pc
			//re.prog[goto_pc].group_id = group_count         // id of this group, used for storing data

			pc = pc + 1
			i = i + char_len
			continue
		}

		// IST_DOT_CHAR match any char except the following token
		if char_len==1 && pc >= 0 && byte(char_tmp) == `.` {
			re.prog[pc].ist = u32(0) | IST_DOT_CHAR
			re.prog[pc].rep_min = 1
			re.prog[pc].rep_max = 1
			pc = pc + 1
			i = i + char_len
			continue
		}

		// OR branch
		if char_len==1 && pc > 0 && byte(char_tmp) == `|` {
			// two consecutive IST_DOT_CHAR are an error
			if pc > 0 && re.prog[pc-1].ist == IST_OR_BRANCH {
				return ERR_SYNTAX_ERROR,i
			}
			re.prog[pc].ist = u32(0) | IST_OR_BRANCH
			pc = pc + 1
			i = i + char_len
			continue
		}

		// Quantifiers
		if char_len==1 && pc > 0{
			mut quant_flag := true
			match byte(char_tmp) {
				`?` {
					//C.printf("q: %c\n",char_tmp)
					re.prog[pc-1].rep_min = 0
					re.prog[pc-1].rep_max = 1
				}

				`+` {
					//C.printf("q: %c\n",char_tmp)
					re.prog[pc-1].rep_min = 1
					re.prog[pc-1].rep_max = MAX_QUANTIFIER
				}

				`*` {
					//C.printf("q: %c\n",char_tmp)
					re.prog[pc-1].rep_min = 0
					re.prog[pc-1].rep_max = MAX_QUANTIFIER
				}

				`{` {
					min, max, tmp, greedy := re.parse_quantifier(in_txt, i+1)
					// it is a quantifier
					if min >= 0 {
						//C.printf("{%d,%d}\n str:[%s] greedy: %d\n", min, max, in_txt[i..i+tmp], greedy)
						i = i + tmp
						re.prog[pc-1].rep_min = min
						re.prog[pc-1].rep_max = max
						re.prog[pc-1].greedy  = greedy
						continue
					}
					else {
						return min,i
					}
					// TODO: decide if the open bracket can be conform without the close bracket
					/*
					// no conform, parse as normal char
					else {
						quant_flag = false
					}
					*/
				}
				else{
					quant_flag = false
				}
			}

			if quant_flag {
				i = i + char_len
				continue
			}
		}

		// IST_CHAR_CLASS_*
		if char_len==1 && pc >= 0{
			if byte(char_tmp) == `[` {
				cc_index,tmp,cc_type := re.parse_char_class(in_txt, i+1)
				if cc_index >= 0 {
					//C.printf("index: %d str:%s\n",cc_index,in_txt[i..i+tmp])
					i = i + tmp
					re.prog[pc].ist      = u32(0) | cc_type
					re.prog[pc].cc_index = cc_index
					re.prog[pc].rep_min  = 1
					re.prog[pc].rep_max  = 1
					pc = pc + 1
					continue
				}

				// cc_class vector memory full
				else if cc_index < 0 {
					return cc_index, i
				}
			}
		}
		
		// IST_BSLS_CHAR
		if char_len==1 && pc >= 0{
			if byte(char_tmp) == `\\` {
				bsls_index,tmp := re.parse_bsls(in_txt,i)
				//C.printf("index: %d str:%s\n",bsls_index,in_txt[i..i+tmp])
				if bsls_index >= 0 {
					i = i + tmp
					re.prog[pc].ist       = u32(0) | IST_BSLS_CHAR
					re.prog[pc].rep_min   = 1
					re.prog[pc].rep_max   = 1
					re.prog[pc].validator = BSLS_VALIDATOR_ARRAY[bsls_index].validator
					re.prog[pc].ch      = BSLS_VALIDATOR_ARRAY[bsls_index].ch
					pc = pc + 1
					continue
				} 
				// this is an escape char, skip the bsls and continue as a normal char
				else if bsls_index == NO_MATCH_FOUND {
					i += char_len
					char_tmp,char_len = re.get_char(in_txt,i)
					// continue as simple char
				}
				// if not an escape or a bsls char then it is an error (at least for now!)
				else {
					return bsls_index,i+tmp
				}
			}
		}

		// IST_SIMPLE_CHAR
		re.prog[pc].ist     = IST_SIMPLE_CHAR
		re.prog[pc].ch      = char_tmp
		re.prog[pc].ch_len  = char_len
		re.prog[pc].rep_min = 1
		re.prog[pc].rep_max = 1
		//C.printf("char: %c\n",char_tmp)
		pc = pc +1

		i+=char_len
	}

	// add end of the program
	re.prog[pc].ist = IST_PROG_END

	// check for unbalanced groups
	if group_stack_index != -1 {
		return ERR_GROUP_NOT_BALANCED, group_stack_txt_index[group_stack_index]+1
	}

	// check for OR at the end of the program
	if pc > 0 && re.prog[pc-1].ist == IST_OR_BRANCH {
		return ERR_SYNTAX_ERROR,in_txt.len
	}
	
	// store the number of groups in the query
	re.group_count = group_count+1

	//******************************************
	// Post processing
	//******************************************

	// count IST_DOT_CHAR to set the size of the state stack
	mut pc1 := 0
	mut tmp_count := 0
	for pc1 < pc {
		if re.prog[pc1].ist == IST_DOT_CHAR {
			tmp_count++
		}
		pc1++
	}

	// init the state stack
	re.state_stack = [StateDotObj{}].repeat(tmp_count+1)	
	
	// OR branch
	// a|b|cd
	// d exit point
	// a,b,c branches
	// set the jump in the right places
	pc1 = 0
	for pc1 < pc-2 {
		// two consecutive OR are a syntax error
		if re.prog[pc1+1].ist == IST_OR_BRANCH && re.prog[pc1+2].ist == IST_OR_BRANCH {
			return ERR_SYNTAX_ERROR, i
		}

		// manange a|b chains like a|(b)|c|d...
		// standard solution
		if re.prog[pc1].ist != IST_OR_BRANCH && 
			re.prog[pc1+1].ist == IST_OR_BRANCH &&
			re.prog[pc1+2].ist != IST_OR_BRANCH 
		{
			re.prog[pc1].next_is_or = true   // set that the next token is an  OR
			re.prog[pc1+1].rep_min = pc1+2   // failed match jump
			
			// match jump, if an OR chain the next token will be an OR token
			mut pc2 := pc1+2
			for pc2 < pc-1 {
				ist := re.prog[pc2].ist
				if  ist == IST_GROUP_START {
					re.prog[pc1+1].rep_max = re.prog[pc2].goto_pc + 1
					break
				}
				if ist != IST_OR_BRANCH {
					re.prog[pc1+1].rep_max = pc2 + 1
					break
				}
				pc2++
			}
			//C.printf("Compile OR postproc. [%d,OR %d,%d]\n",pc1,pc1+1,pc2)
			pc1 = pc2 
			continue
		}
		
		pc1++
	}

	//******************************************
	// DEBUG PRINT REGEX GENERATED CODE
	//******************************************
	if re.debug > 0 {
		re.log_func(re.get_code())
	}
	//******************************************

	return COMPILE_OK, 0
}

// get_code return the compiled code as regex string, note: may be different from the source!
pub fn (re RE) get_code() string {
		mut pc1 := 0
		mut res := strings.new_builder(re.cc.len*2*re.prog.len)
		res.write("========================================\nv RegEx compiler v $V_REGEX_VERSION output:\n")
		
		mut stop_flag := false

		for pc1 <= re.prog.len {
			tk := re.prog[pc1]
			res.write("PC:${pc1:3d}")
			
		    res.write(" ist: ")
		    res.write("${tk.ist:8x}".replace(" ","0") )
		    res.write(" ")
			ist :=tk.ist
			if ist == IST_BSLS_CHAR {
				res.write("[\\${tk.ch:1c}]     BSLS")
			} else if ist == IST_PROG_END {
				res.write("PROG_END")
				stop_flag = true
			} else if ist == IST_OR_BRANCH {
				res.write("OR      ")
			} else if ist == IST_CHAR_CLASS_POS {
				res.write("[${re.get_char_class(pc1)}]     CHAR_CLASS_POS")
			} else if ist == IST_CHAR_CLASS_NEG {
				res.write("[^${re.get_char_class(pc1)}]    CHAR_CLASS_NEG")
			} else if ist == IST_DOT_CHAR {
				res.write(".        DOT_CHAR")
			} else if ist == IST_GROUP_START {
				res.write("(        GROUP_START #:${tk.group_id}")
				if tk.group_id == -1 {
					res.write(" ?:")
				} else {
					for x in re.group_map.keys() {
						if re.group_map[x] == (tk.group_id+1) {
							res.write(" ?P<${x}>")
							break
						}
					}
				}
			} else if ist == IST_GROUP_END {
				res.write(")        GROUP_END   #:${tk.group_id}")
			} else if ist == IST_SIMPLE_CHAR {
				res.write("[${tk.ch:1c}]      query_ch")
			}

			if tk.rep_max == MAX_QUANTIFIER {
				res.write(" {${tk.rep_min:3d},MAX}")
			}else{
				if ist == IST_OR_BRANCH {
					res.write(" if false go: ${tk.rep_min:3d} if true go: ${tk.rep_max:3d}")
				} else {
					res.write(" {${tk.rep_min:3d},${tk.rep_max:3d}}")
				}
				if tk.greedy == true {
					res.write("?")
				}
			}
			res.write("\n")
			if stop_flag {
				break
			}
			pc1++
		}

		res.write("========================================\n")
		return res.str()
}

// get_query return a string with a reconstruction of the query starting from the regex program code
pub fn (re RE) get_query() string {
	mut res := strings.new_builder(re.query.len*2)

	if (re.flag & F_MS) != 0 {
		res.write("^")
	}

	mut i := 0
	for i < re.prog.len && re.prog[i].ist != IST_PROG_END && re.prog[i].ist != 0{
		tk := &re.prog[i]
		ch := tk.ist
		
		// GROUP start
		if ch == IST_GROUP_START {
			if re.debug == 0 {
				res.write("(")
			} else {
				if tk.group_id == -1 {
					res.write("(?:")   // non capturing group
				} else {
					res.write("#${tk.group_id}(")
				}
			}
			
			for x in re.group_map.keys() {
				if re.group_map[x] == (tk.group_id+1) {
					res.write("?P<${x}>")
					break
				}
			}

			i++
			continue
		}

		// GROUP end
		if ch == IST_GROUP_END {
			res.write(")")
		}

		// OR branch
		if ch == IST_OR_BRANCH {
			res.write("|")
			if re.debug > 0 {
				res.write("{${tk.rep_min},${tk.rep_max}}")
			}
			i++
			continue
		}

		// char class
		if ch == IST_CHAR_CLASS_NEG || ch == IST_CHAR_CLASS_POS {
			res.write("[")
			if ch == IST_CHAR_CLASS_NEG {
				res.write("^")
			}
			res.write("${re.get_char_class(i)}")
			res.write("]")
		}

		// bsls char
		if ch == IST_BSLS_CHAR {
			res.write("\\${tk.ch:1c}")
		}

		// IST_DOT_CHAR
		if ch == IST_DOT_CHAR {
			res.write(".")
		}

		// char alone
		if ch == IST_SIMPLE_CHAR {
			if byte(ch) in BSLS_ESCAPE_LIST {
				res.write("\\")
			}
			res.write("${tk.ch:c}")
		}

		// quantifier
		if !(tk.rep_min == 1 && tk.rep_max == 1) {
			if tk.rep_min == 0 && tk.rep_max == 1 {
				res.write("?")
			} else if tk.rep_min == 1 && tk.rep_max == MAX_QUANTIFIER {
				res.write("+")
			} else if tk.rep_min == 0 && tk.rep_max == MAX_QUANTIFIER {
				res.write("*")
			} else {
				if tk.rep_max == MAX_QUANTIFIER {
					res.write("{${tk.rep_min},MAX}")
				} else {
					res.write("{${tk.rep_min},${tk.rep_max}}")
				}
				if tk.greedy == true {
					res.write("?")
				}
			}
		}
		i++
	}
	if (re.flag & F_ME) != 0 {
		res.write("$")
	}

	return res.str()
}

/******************************************************************************
*
* Matching
*
******************************************************************************/
enum match_state{
	start = 0,
	stop,
	end,
	new_line,
	
	ist_load,     // load and execute instruction
	ist_next,     // go to next instruction
	ist_next_ks,  // go to next instruction without clenaning the state
	ist_quant_p,  // match positive ,quantifier check 
	ist_quant_n,  // match negative, quantifier check 
	ist_quant_pg, // match positive ,group quantifier check
	ist_quant_ng, // match negative ,group quantifier check
}

fn state_str(s match_state) string {
	match s{
		.start        { return "start" }
		.stop         { return "stop" }
		.end          { return "end" }
		.new_line     { return "new line" }

		.ist_load     { return "ist_load" }
		.ist_next     { return "ist_next" }
		.ist_next_ks  { return "ist_next_ks" }
		.ist_quant_p  { return "ist_quant_p" }
		.ist_quant_n  { return "ist_quant_n" }
		.ist_quant_pg { return "ist_quant_pg" }
		.ist_quant_ng { return "ist_quant_ng" }
		else { return "UNKN" }
	} 
}

struct StateObj {
pub mut:
	match_flag bool = false
	match_index int = -1
	match_first int = -1
}

pub fn (re mut RE) match_base(in_txt byteptr, in_txt_len int ) (int,int) {
	// result status
	mut result := NO_MATCH_FOUND     // function return
	mut first_match := -1             //index of the first match

	mut i := 0                       // source string index
	mut ch := u32(0)                 // examinated char 
	mut char_len := 0                // utf8 examinated char len
	mut m_state := match_state.start // start point for the matcher FSM

	mut pc := -1                     // program counter
	mut state := StateObj{}          // actual state
	mut ist := u32(0)                // actual instruction
	mut l_ist := u32(0)              // last matched instruction

	mut group_stack      := [-1].repeat(re.group_max)
	mut group_data       := [-1].repeat(re.group_max)

	mut group_index := -1            // group id used to know how many groups are open

	mut step_count := 0              // stats for debug
	mut dbg_line   := 0              // count debug line printed
	
	re.reset()

	if re.debug>0 {
		// print header
		mut h_buf := strings.new_builder(32)
		h_buf.write("flags: ")
		h_buf.write("${re.flag:8x}".replace(" ","0"))
		h_buf.write("\n")
		re.log_func(h_buf.str())
	}

	for m_state != .end {
		
		if pc >= 0 && pc < re.prog.len {
			ist = re.prog[pc].ist
		}else if pc >= re.prog.len {
			//C.printf("ERROR!! PC overflow!!\n")
			return ERR_INTERNAL_ERROR, i
		}

		//******************************************
		// DEBUG LOG
		//******************************************
		if re.debug>0 {
			mut buf2 := strings.new_builder(re.cc.len+128)

			// print all the instructions	

			// end of the input text
			if i >= in_txt_len {
				buf2.write("# ${step_count:3d} END OF INPUT TEXT\n")
				re.log_func(buf2.str())
			}else{

				// print only the exe instruction
				if (re.debug == 1 && m_state == .ist_load) ||
					re.debug == 2
				{		
					if ist == IST_PROG_END {
						buf2.write("# ${step_count:3d} PROG_END\n")
					}
					else if ist == 0 || m_state in [.start,.ist_next,.stop] {
						buf2.write("# ${step_count:3d} s: ${state_str(m_state):12s} PC: NA\n")
					}else{
						ch, char_len = re.get_charb(in_txt,i)
						
						buf2.write("# ${step_count:3d} s: ${state_str(m_state):12s} PC: ${pc:3d}=>")
						buf2.write("${ist:8x}".replace(" ","0"))
						buf2.write(" i,ch,len:[${i:3d},'${utf8_str(ch)}',${char_len}] f.m:[${first_match:3d},${state.match_index:3d}] ")

						if ist == IST_SIMPLE_CHAR {
							buf2.write("query_ch: [${re.prog[pc].ch:1c}]")
						} else {
							if ist == IST_BSLS_CHAR {
								buf2.write("BSLS [\\${re.prog[pc].ch:1c}]")
							} else if ist == IST_PROG_END {
								buf2.write("PROG_END")
							} else if ist == IST_OR_BRANCH {
								buf2.write("OR")
							} else if ist == IST_CHAR_CLASS_POS {
								buf2.write("CHAR_CLASS_POS[${re.get_char_class(pc)}]")
							} else if ist == IST_CHAR_CLASS_NEG {
								buf2.write("CHAR_CLASS_NEG[${re.get_char_class(pc)}]")
							} else if ist == IST_DOT_CHAR {
								buf2.write("DOT_CHAR")
							} else if ist == IST_GROUP_START {
								tmp_gi :=re.prog[pc].group_id
								tmp_gr := re.prog[re.prog[pc].goto_pc].group_rep
								buf2.write("GROUP_START #:${tmp_gi} rep:${tmp_gr} ")
							} else if ist == IST_GROUP_END {
								buf2.write("GROUP_END   #:${re.prog[pc].group_id} deep:${group_index}")
							}
						}
						if re.prog[pc].rep_max == MAX_QUANTIFIER {
							buf2.write("{${re.prog[pc].rep_min},MAX}:${re.prog[pc].rep}")
						} else {
							buf2.write("{${re.prog[pc].rep_min},${re.prog[pc].rep_max}}:${re.prog[pc].rep}")
						}
						if re.prog[pc].greedy == true {
							buf2.write("?")
						}
						buf2.write(" (#${group_index})\n")
					}
					re.log_func(buf2.str())
				}
			}
			step_count++
			dbg_line++
		}
		//******************************************

		// we're out of text, manage it
		if i >= in_txt_len || m_state == .new_line {
			
			// manage groups
			if group_index >= 0 && state.match_index >= 0 {
				//C.printf("End text with open groups!\n")
				// close the groups
				for group_index >= 0 {
					tmp_pc := group_data[group_index]
					re.prog[tmp_pc].group_rep++
					/*
					C.printf("Closing group %d {%d,%d}:%d\n",
						group_index,
						re.prog[tmp_pc].rep_min,
						re.prog[tmp_pc].rep_max,
						re.prog[tmp_pc].group_rep
					)
					*/
					if re.prog[tmp_pc].group_rep >= re.prog[tmp_pc].rep_min && re.prog[tmp_pc].group_id >= 0{
						start_i   := group_stack[group_index]
	 					group_stack[group_index]=-1

	 					// save group results
						g_index := re.prog[tmp_pc].group_id*2
						if start_i >= 0 {
							re.groups[g_index] = start_i
						} else {
							re.groups[g_index] = 0
						}
						re.groups[g_index+1] = i

						// continuous save, save until we have space
						if re.group_csave_index > 0 {
							// check if we have space to save the record
							if (re.group_csave_index + 3) < re.group_csave.len {
								// incrment counter
								re.group_csave[0]++
								// save the record  
								re.group_csave[re.group_csave_index++] = g_index >> 1          // group id
								re.group_csave[re.group_csave_index++] = re.groups[g_index]    // start
								re.group_csave[re.group_csave_index++] = re.groups[g_index+1]  // end
							}
						}

 					}

					group_index--
				}
			}

			// manage IST_DOT_CHAR

			m_state == .end
			break
			//return NO_MATCH_FOUND,0
		}

		// starting and init
		if m_state == .start {
			pc = -1
			i = 0
			m_state = .ist_next
			continue
		}

		// ist_next, next instruction reseting its state
		if m_state == .ist_next {
			pc = pc + 1
			re.prog[pc].reset()
			// check if we are in the program bounds
			if pc < 0 || pc > re.prog.len {
				//C.printf("ERROR!! PC overflow!!\n")
				return ERR_INTERNAL_ERROR, i
			}			
			m_state = .ist_load
			continue
		}

		// ist_next_ks, next instruction keeping its state
		if m_state == .ist_next_ks {
			pc = pc + 1
			// check if we are in the program bounds
			if pc < 0 || pc > re.prog.len {
				//C.printf("ERROR!! PC overflow!!\n")
				return ERR_INTERNAL_ERROR, i
			}		
			m_state = .ist_load
			continue
		}

		// load the char
		ch, char_len = re.get_charb(in_txt,i)

		// check new line if flag F_NL enabled
		if (re.flag & F_NL) != 0 && char_len == 1 && byte(ch) in NEW_LINE_LIST {
			m_state = .new_line
			continue
		}

		// check if stop 
		if m_state == .stop {
			
			// we are in search mode, don't exit until the end
			if re.flag & F_SRC != 0 && ist != IST_PROG_END {
				pc = -1
				i += char_len
				m_state = .ist_next
				re.reset_src()
				state.match_index = -1
				first_match = -1
				continue
			}

			// if we are in restore state ,do it and restart
			//C.printf("re.state_stack_index %d\n",re.state_stack_index )
			if re.state_stack_index >=0 && re.state_stack[re.state_stack_index].pc >= 0 {
				i = re.state_stack[re.state_stack_index].i
				pc = re.state_stack[re.state_stack_index].pc
				state.match_index =	re.state_stack[re.state_stack_index].mi
				group_index = re.state_stack[re.state_stack_index].group_stack_index

				m_state = .ist_load
				continue
			}

			if ist == IST_PROG_END { 
				return first_match,i
			}
			
			// exit on no match
			return result,0
		}

		// ist_load
		if m_state == .ist_load {
			
			// program end
			if ist == IST_PROG_END {
				// if we are in match exit well
				
				if group_index >= 0 && state.match_index >= 0 {
					group_index = -1
				}

				// we have a DOT MATCH on going
				//C.printf("IST_PROG_END l_ist: %08x\n", l_ist)
				if re.state_stack_index>=0 && l_ist == IST_DOT_CHAR {
					m_state = .stop
					continue
				}

				re.state_stack_index = -1
				m_state = .stop
				continue
				
			}

			// check GROUP start, no quantifier is checkd for this token!!
			else if ist == IST_GROUP_START {
				group_index++
				group_data[group_index] = re.prog[pc].goto_pc  // save where is IST_GROUP_END, we will use it for escape
				group_stack[group_index]=i                     // index where we start to manage
				//C.printf("group_index %d rep %d\n", group_index, re.prog[re.prog[pc].goto_pc].group_rep)
								
				m_state = .ist_next
				continue
			}

			// check GROUP end
			else if ist == IST_GROUP_END {
				// we are in matching streak
				if state.match_index >= 0 {
					// restore txt index stack and save the group data
					
					//C.printf("g.id: %d group_index: %d\n", re.prog[pc].group_id, group_index)
					if group_index >= 0 && re.prog[pc].group_id >= 0 {
	 					start_i   := group_stack[group_index]
	 					//group_stack[group_index]=-1

	 					// save group results
						g_index := re.prog[pc].group_id*2
						if start_i >= 0 {
							re.groups[g_index] = start_i
						} else {
							re.groups[g_index] = 0
						}
						re.groups[g_index+1] = i
						//C.printf("GROUP %d END [%d, %d]\n", re.prog[pc].group_id, re.groups[g_index], re.groups[g_index+1])

						// continuous save, save until we have space
						if re.group_csave_index > 0 {
							// check if we have space to save the record
							if (re.group_csave_index + 3) < re.group_csave.len {
								// incrment counter
								re.group_csave[0]++
								// save the record  
								re.group_csave[re.group_csave_index++] = g_index >> 1          // group id
								re.group_csave[re.group_csave_index++] = re.groups[g_index]    // start
								re.group_csave[re.group_csave_index++] = re.groups[g_index+1]  // end
							}
						}
					}
					
					re.prog[pc].group_rep++ // increase repetitions
					//C.printf("GROUP %d END %d\n", group_index, re.prog[pc].group_rep) 
					m_state = .ist_quant_pg
					continue
					
				}

				m_state = .ist_quant_ng
				continue			
			}

			// check OR
			else if ist == IST_OR_BRANCH {
				if state.match_index >= 0 {
					pc = re.prog[pc].rep_max
					//C.printf("IST_OR_BRANCH True pc: %d\n", pc)					
				}else{
					pc = re.prog[pc].rep_min
					//C.printf("IST_OR_BRANCH False pc: %d\n", pc)
				}
				re.prog[pc].reset()
				m_state == .ist_load
				continue
			}

			// check IST_DOT_CHAR
			else if ist == IST_DOT_CHAR {
				//C.printf("IST_DOT_CHAR rep: %d\n", re.prog[pc].rep)
				state.match_flag = true
				l_ist = u32(IST_DOT_CHAR)

				if first_match < 0 {
					first_match = i
				}
				state.match_index = i
				re.prog[pc].rep++	

				//if re.prog[pc].rep >= re.prog[pc].rep_min && re.prog[pc].rep <= re.prog[pc].rep_max {
				if re.prog[pc].rep >= 0 && re.prog[pc].rep <= re.prog[pc].rep_max {
					//C.printf("DOT CHAR save state : %d\n", re.state_stack_index)
					// save the state
					
					// manage first dot char
					if re.state_stack_index < 0 {
						re.state_stack_index++
					}

					re.state_stack[re.state_stack_index].pc = pc
					re.state_stack[re.state_stack_index].mi = state.match_index
					re.state_stack[re.state_stack_index].group_stack_index = group_index
				} else {
					re.state_stack[re.state_stack_index].pc = -1
					re.state_stack[re.state_stack_index].mi = -1
					re.state_stack[re.state_stack_index].group_stack_index = -1
				}

				if re.prog[pc].rep >= 1 && re.state_stack_index >= 0 {
					re.state_stack[re.state_stack_index].i  = i + char_len
				} 

				// manage * and {0,} quantifier
				if re.prog[pc].rep_min > 0 {
					i += char_len // next char
					l_ist = u32(IST_DOT_CHAR)
				}

				m_state = .ist_next
				continue

			}

			// char class IST
			else if ist == IST_CHAR_CLASS_POS || ist == IST_CHAR_CLASS_NEG {
				state.match_flag = false
				mut cc_neg := false
			
				if ist == IST_CHAR_CLASS_NEG {
					cc_neg = true
				}
				mut cc_res := re.check_char_class(pc,ch)
				
				if cc_neg {
					cc_res = !cc_res
				}

				if cc_res {
					state.match_flag = true
					l_ist = u32(IST_CHAR_CLASS_POS)
					
					if first_match < 0 {
						first_match = i
					}
					
					state.match_index = i

					re.prog[pc].rep++ // increase repetitions
					i += char_len // next char
					m_state = .ist_quant_p
					continue
				}
				m_state = .ist_quant_n
				continue
			}

			// check bsls
			else if ist == IST_BSLS_CHAR {
				state.match_flag = false
				tmp_res := re.prog[pc].validator(byte(ch))
				//C.printf("BSLS in_ch: %c res: %d\n", ch, tmp_res)
				if tmp_res {
					state.match_flag = true
					l_ist = u32(IST_BSLS_CHAR)
					
					if first_match < 0 {
						first_match = i
					}
					
					state.match_index = i

					re.prog[pc].rep++ // increase repetitions
					i += char_len // next char
					m_state = .ist_quant_p
					continue
				}
				m_state = .ist_quant_n
				continue
			}

			// simple char IST
			else if ist == IST_SIMPLE_CHAR {
				//C.printf("IST_SIMPLE_CHAR\n")
				state.match_flag = false

				if re.prog[pc].ch == ch
				{
					state.match_flag = true
					l_ist = IST_SIMPLE_CHAR
					
					if first_match < 0 {
						first_match = i
					}
					//C.printf("state.match_index: %d\n", state.match_index)
					state.match_index = i

					re.prog[pc].rep++ // increase repetitions
					i += char_len // next char
					m_state = .ist_quant_p
					continue
				}
				m_state = .ist_quant_n
				continue
			} 
			/* UNREACHABLE */
			//C.printf("PANIC2!! state: %d\n", m_state)
			return ERR_INTERNAL_ERROR, i
		
		}

		/***********************************
		* Quantifier management 
		***********************************/
		// ist_quant_ng
		if m_state == .ist_quant_ng {
			
			// we are finished here
			if group_index < 0 {
				//C.printf("Early stop!\n")
				result = NO_MATCH_FOUND
				m_state = .stop
				continue
			}

			tmp_pc := group_data[group_index]    // PC to the end of the group token
			rep    := re.prog[tmp_pc].group_rep  // use a temp variable 
			re.prog[tmp_pc].group_rep = 0        // clear the repetitions

			//C.printf(".ist_quant_ng group_pc_end: %d rep: %d\n", tmp_pc,rep)

			if rep >= re.prog[tmp_pc].rep_min {
				//C.printf("ist_quant_ng GROUP CLOSED OK group_index: %d\n", group_index)
				
				i = group_stack[group_index]
				pc = tmp_pc
				group_index--
				m_state = .ist_next
				continue
			}
			else if re.prog[tmp_pc].next_is_or {
				//C.printf("ist_quant_ng OR Negative branch\n")

				i = group_stack[group_index]
				pc = re.prog[tmp_pc+1].rep_min -1
				group_index--
				m_state = .ist_next
				continue
			}
			else if rep>0 && rep < re.prog[tmp_pc].rep_min {
				//C.printf("ist_quant_ng UNDER THE MINIMUM g.i: %d\n", group_index)
				
				// check if we are inside a group, if yes exit from the nested groups
				if group_index > 0{
					group_index--
					pc = tmp_pc
					m_state = .ist_quant_ng //.ist_next
					continue
				}

				if group_index == 0 {
					group_index--
					pc = tmp_pc // TEST
					m_state = .ist_next
					continue
				}

				result = NO_MATCH_FOUND
				m_state = .stop
				continue
			}
			else if rep==0 && rep < re.prog[tmp_pc].rep_min {
				//C.printf("ist_quant_ng ZERO UNDER THE MINIMUM g.i: %d\n", group_index)

				if group_index > 0{
					group_index--
					pc = tmp_pc
					m_state = .ist_quant_ng //.ist_next
					continue
				}

				result = NO_MATCH_FOUND
				m_state = .stop
				continue
			}

			//C.printf("DO NOT STAY HERE!! {%d,%d}:%d\n", re.prog[tmp_pc].rep_min, re.prog[tmp_pc].rep_max, rep)
			/* UNREACHABLE */
			return ERR_INTERNAL_ERROR, i

		}
		// ist_quant_pg
		else if m_state == .ist_quant_pg {
			//C.printf(".ist_quant_pg\n")
			mut tmp_pc := pc
			if group_index >= 0 {
				tmp_pc = group_data[group_index]			
			}

			rep := re.prog[tmp_pc].group_rep

			if rep < re.prog[tmp_pc].rep_min {
				//C.printf("ist_quant_pg UNDER RANGE\n")
				pc = re.prog[tmp_pc].goto_pc 
				m_state = .ist_next
				continue
			}
			else if rep == re.prog[tmp_pc].rep_max {
				//C.printf("ist_quant_pg MAX RANGE\n")
				re.prog[tmp_pc].group_rep = 0 // clear the repetitions
				group_index--
				m_state = .ist_next
				continue
			}
			else if rep >= re.prog[tmp_pc].rep_min {
				//C.printf("ist_quant_pg IN RANGE group_index:%d\n", group_index)

				// check greedy flag, if true exit on minimum
				if re.prog[tmp_pc].greedy == true {
					re.prog[tmp_pc].group_rep = 0 // clear the repetitions
					group_index--
					m_state = .ist_next
					continue
				}

				pc = re.prog[tmp_pc].goto_pc - 1
				group_index--
				m_state = .ist_next
				continue
			}
			
			/* UNREACHABLE */
			//C.printf("PANIC3!! state: %d\n", m_state)
			return ERR_INTERNAL_ERROR, i
		}
		
		// ist_quant_n
		else if m_state == .ist_quant_n {
			rep := re.prog[pc].rep
			//C.printf("Here!! PC %d is_next_or: %d \n", pc, re.prog[pc].next_is_or)

			// zero quantifier * or ?
			if rep == 0 && re.prog[pc].rep_min == 0 {
				//C.printf("ist_quant_n ZERO RANGE MIN\n")
				m_state = .ist_next // go to next ist
				continue
			}
			// match + or *
			else if rep >= re.prog[pc].rep_min {
				//C.printf("ist_quant_n MATCH RANGE\n")
				m_state = .ist_next
				continue
			}

			// check the OR if present
			if re.prog[pc].next_is_or {
				//C.printf("OR present on failing\n")
				state.match_index = -1
				m_state = .ist_next
				continue
			}

			// we are in a group manage no match from here
			if group_index >= 0 {
				//C.printf("ist_quant_n FAILED insied a GROUP group_index:%d\n", group_index)
				m_state = .ist_quant_ng
				continue
			}

			// no other options
			//C.printf("ist_quant_n NO_MATCH_FOUND\n")
			result = NO_MATCH_FOUND
			m_state = .stop
			continue
			//return NO_MATCH_FOUND, 0 
		}

		// ist_quant_p
		else if m_state == .ist_quant_p {
			// exit on first match
			if (re.flag & F_EFM) != 0 {
				return i,i+1
			}

			rep := re.prog[pc].rep
			
			// under range
			if rep > 0 && rep < re.prog[pc].rep_min {
				//C.printf("ist_quant_p UNDER RANGE\n")
				m_state = .ist_load // continue the loop
				continue
			}

			// range ok, continue loop
			else if rep >= re.prog[pc].rep_min && rep < re.prog[pc].rep_max {
				//C.printf("ist_quant_p IN RANGE\n")
				
				// check greedy flag, if true exit on minimum
				if re.prog[pc].greedy == true {
					m_state = .ist_next
					continue
				}
				m_state = .ist_load
				continue
			}

			// max reached
			else if rep == re.prog[pc].rep_max {
				//C.printf("ist_quant_p MAX RANGE\n")
				m_state = .ist_next
				continue
			}

		}
		/* UNREACHABLE */
		//C.printf("PANIC4!! state: %d\n", m_state)
		return ERR_INTERNAL_ERROR, i
	}

	// Check the results
	if state.match_index >= 0 {
		if group_index < 0 {
			//C.printf("OK match,natural end [%d,%d]\n", first_match, i)
			return first_match, i
		} else {
			//C.printf("Skip last group\n")
			return first_match,group_stack[group_index--]
		}
	}
	//C.printf("NO_MATCH_FOUND, natural end\n")
	return NO_MATCH_FOUND, 0
}

/******************************************************************************
*
* Public functions
*
******************************************************************************/

//
// Inits
//

// regex create a regex object from the query string
pub fn regex(in_query string) (RE,int,int){
	mut re := RE{}
	re.prog = [Token{}].repeat(in_query.len+1)
	re.cc = [CharClass{}].repeat(in_query.len+1)
	re.group_max_nested = 8

	re_err,err_pos := re.compile(in_query)
	return re, re_err, err_pos
}

// new_regex create a REgex of small size, usually sufficient for ordinary use
pub fn new_regex() RE {
	return new_regex_by_size(1)
}

// new_regex_by_size create a REgex of large size, mult specify the scale factor of the memory that will be allocated
pub fn new_regex_by_size(mult int) RE {
	mut re := RE{}
	re.prog = [Token{}].repeat(MAX_CODE_LEN*mult)       // max program length, default 256 istructions
	re.cc = [CharClass{}].repeat(MAX_CODE_LEN*mult)     // char class list
	re.group_max_nested = 3*mult                        // max nested group
	
	return re
}

//
// Matchers
//

pub fn (re mut RE) match_string(in_txt string) (int,int) {
	start, end := re.match_base(in_txt.str,in_txt.len)
	if start >= 0 && end > start {		
		if (re.flag & F_MS) != 0 && start > 0 {
			return NO_MATCH_FOUND, 0
		}
		if (re.flag & F_ME) != 0 && end < in_txt.len {
			if in_txt[end] in NEW_LINE_LIST {
				return start, end
			}
			return NO_MATCH_FOUND, 0
		}
		return start, end
	}
	return start, end
}

//
// Finders
//

// find try to find the first match in the input string
pub fn (re mut RE) find(in_txt string) (int,int) {
	old_flag := re.flag
	re.flag |= F_SRC  // enable search mode
	start, end := re.match_base(in_txt.str, in_txt.len)
	re.flag = old_flag
	if start >= 0 && end > start {
		return start,end
	}
	return NO_MATCH_FOUND, 0
}

// find all the non overlapping occurrences of the match pattern
pub fn (re mut RE) find_all(in_txt string) []int {
	mut i := 0
	mut res := []int
	mut ls := -1
	for i < in_txt.len {
		s,e := re.find(in_txt[i..])
		if s >= 0 && e > s && i+s > ls {
			//println("find match in: ${i+s},${i+e} [${in_txt[i+s..i+e]}] ls:$ls")
			res << i+s
			res << i+e
			ls = i+s
			i = i+e
			continue
		} else {
			i++
		}
		
	}
	return res
}

// replace return a string where the matches are replaced with the replace string
pub fn (re mut RE) replace(in_txt string, repl string) string {
	pos := re.find_all(in_txt)
	if pos.len > 0 {
		mut res := ""
		mut i := 0

		mut s1 := 0
		mut e1 := in_txt.len
		
		for i < pos.len {
			e1 = pos[i]
			res += in_txt[s1..e1] + repl
			s1 = pos[i+1]
			i += 2
		}

		res += in_txt[s1..]
		return res
	}
	return in_txt
}
