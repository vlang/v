# Description
`regex` is a small but powerful regular expression library,
written in pure V.

NB: `regex` is *not* PCRE compatible.

[TOC]

## Introduction

Here are the assumptions made during the writing of the implementation, that
are valid for all the `regex` module features:

1. The matching stops at the end of the string, *not* at newline characters.

2. The basic atomic elements of this regex engine are the tokens.
In a query string a simple character is a token.


## Differences with PCRE:

NB: We must point out that the **V-Regex module is not PCRE compliant** and thus
some behaviour will be different. This difference is due to the V philosophy,
to have one way and keep it simple.

The main differences can be summarized in the following points:

- The basic element **is the token not the sequence of symbols**, and the most
simple token, is a single character.

- `|` **the OR operator acts on tokens,** for example `abc|ebc` is not
`abc` OR `ebc`. Instead it is evaluated like `ab`, followed by `c OR e`,
followed by `bc`, because the **token is the base element**,
not the sequence of symbols.
Note: **Two char classes with an `OR` in the middle is a syntax error.**

- The **match operation stops at the end of the string**. It does *NOT* stop
at new line characters.


## Tokens

The tokens are the atomic units, used by this regex engine.
They can be one of the following:


### Simple char

This token is a simple single character like `a` or `b` etc.


### Match positional delimiters

`^` Matches the start of the string.

`$` Matches the end of the string.


### Char class (cc)

The character classes match all the chars specified inside. Use square
brackets `[ ]` to enclose them.

The sequence of the chars in the character class, is evaluated with an OR op.

For example, the cc `[abc]`, matches any character, that is `a` or `b` or `c`,
but it doesn't match `C` or `z`.

Inside a cc, it is possible to specify a "range" of characters, for example
`[ad-h]` is equivalent to writing `[adefgh]`.

A cc can have different ranges at the same time, for example `[a-zA-z0-9]`
matches all the latin lowercase, uppercase and numeric characters.

It is possible to negate the meaning of a cc, using the caret char at the
start of the cc like this: `[^abc]` . That matches every char that is NOT
`a` or `b` or `c`.

A cc can contain meta-chars like: `[a-z\d]`, that match all the lowercase
latin chars `a-z` and all the digits `\d`.

It is possible to mix all the properties of the char class together.

NB: In order to match the `-` (minus) char, it must be preceded by
    a backslash in the cc, for example `[\-_\d\a]` will match:
      `-` minus,
      `_` underscore,
      `\d` numeric chars,
	  `\a` lower case chars.

### Meta-chars

A meta-char is specified by a backslash, before a character.
For example `\w` is the meta-char `w`.

A meta-char can match different types of characters.

* `\w` matches a word char char `[a-zA-Z0-9_]`
* `\W` matches a non word char
* `\d` matches a digit `[0-9]`
* `\D` matches a non digit
* `\s` matches a space char, one of `[' ','\t','\n','\r','\v','\f']`
* `\S` matches a non space char
* `\a` matches only a lowercase char `[a-z]`
* `\A` matches only an uppercase char `[A-Z]`

### Quantifier

Each token can have a quantifier, that specifies how many times the character
must be matched.

#### **Short quantifiers**

- `?` matches 0 or 1 time, `a?b` matches both `ab` or `b`
- `+` matches *at least* 1 time, for example, `a+` matches both `aaa` or `a`
- `*` matches 0 or more times, for example, `a*b` matches `aaab`, `ab` or `b`

#### **Long quantifiers**

- `{x}` matches exactly x times, `a{2}` matches `aa`, but not `aaa` or `a`
- `{min,}` matches at least min times, `a{2,}` matches `aaa` or `aa`, not `a`
- `{,max}` matches at least 0 times and at maximum max times,
   for example, `a{,2}` matches `a` and `aa`, but doesn't match `aaa`
- `{min,max}` matches from min times, to max times, for example
    `a{2,3}` matches `aa` and `aaa`, but doesn't match `a` or `aaaa`

A long quantifier, may have a `greedy off` flag, that is the `?`
character after the brackets. `{2,4}?` means to match the minimum
number of possible tokens, in this case 2.

### Dot char

The dot is a particular meta-char, that matches "any char".

It is simpler to explain it with an example:

Suppose you have `abccc ddeef` as a source string, that you want to parse
with a regex. The following table show the query strings and the result of
parsing source string.

| query string |   result    |
|--------------|-------------|
| `.*c`        | `abc`       |
| `.*dd`	   | `abcc dd`   |
| `ab.*e`      | `abccc dde` |
| `ab.{3} .*e` | `abccc dde` |
The dot matches any character, until the next token match is satisfied.

**Important Note:** *Consecutive dots, for example `...`, are not allowed.*
*This will cause a syntax error. Use a quantifier instead.*

### OR token

The token `|`, means a logic OR operation between two consecutive tokens,
i.e. `a|b` matches a character that is `a` or `b`.

The OR token can work in a "chained way": `a|(b)|cd ` means test first `a`,
if the char is not `a`, then test the group `(b)`, and if the group doesn't
match too, finally test the token `c`.

NB: ** unlike in PCRE, the OR operation works at token level!**
It doesn't work at concatenation level!
NB2: **Two char classes with an `OR` in the middle is a syntax error.**

That also means, that a query string like `abc|bde` is not equal to
`(abc)|(bde)`, but instead to `ab(c|b)de.
The OR operation works only for `c|b`, not at char concatenation level.

### Groups

Groups are a method to create complex patterns with repetitions of blocks
of tokens. The groups are delimited by round brackets `( )`. Groups can be
nested. Like all other tokens, groups can have a quantifier too.

`c(pa)+z` match `cpapaz` or `cpaz` or `cpapapaz` .

`(c(pa)+z ?)+` matches `cpaz cpapaz cpapapaz` or `cpapaz`

Lets analyze this last case, first we have the group `#0`, that is the most
outer round brackets `(...)+`. This group has a quantifier `+`, that say to
match its content *at least one time*.

Then we have a simple char token `c`, and a second group `#1`: `(pa)+`.
This group also tries to match the sequence `pa`, *at least one time*,
as specified by the `+` quantifier.

Then, we have another simple token `z` and another simple token ` ?`,
i.e. the space char (ascii code 32) followed by the `?` quantifier,
which means that the preceding space should be matched 0 or 1 time.

This explains why the `(c(pa)+z ?)+` query string,
can match `cpaz cpapaz cpapapaz` .

In this implementation the groups are "capture groups". This means that the
last temporal result for each group, can be retrieved from the `RE` struct.

The "capture groups" are stored as indexes in the field `groups`,
that is an `[]int` inside the `RE` struct.

**example:**

```v oksyntax
text := 'cpaz cpapaz cpapapaz'
query := r'(c(pa)+z ?)+'
mut re := regex.regex_opt(query) or { panic(err) }
println(re.get_query())
// #0(c#1(pa)+z ?)+
// #0 and #1 are the ids of the groups, are shown if re.debug is 1 or 2
start, end := re.match_string(text)
// [start=0, end=20]  match => [cpaz cpapaz cpapapaz]
mut gi := 0
for gi < re.groups.len {
	if re.groups[gi] >= 0 {
		println('${gi / 2} :[${text[re.groups[gi]..re.groups[gi + 1]]}]')
	}
	gi += 2
}
// groups captured
// 0 :[cpapapaz]
// 1 :[pa]
```

**note:** *to show the `group id number` in the result of the `get_query()`*
*the flag `debug` of the RE object must be `1` or `2`*

In order to simplify the use of the captured groups, it possible to use the
utility function: `get_group_list`.

This function return a list of groups using this support struct:

```v oksyntax
pub struct Re_group {
pub:
	start int = -1
	end   int = -1
}
```

Here an example of use:

```v oksyntax
/*
This simple function converts an HTML RGB value with 3 or 6 hex digits to
an u32 value, this function is not optimized and it is only for didatical
purpose. Example: #A0B0CC #A9F
*/
fn convert_html_rgb(in_col string) u32 {
	mut n_digit := if in_col.len == 4 { 1 } else { 2 }
	mut col_mul := if in_col.len == 4 { 4 } else { 0 }
	// this is the regex query, it use the V string interpolation to customize the regex query
	// NOTE: if you want use escaped code you must use the r"" (raw) strings,
	// *** please remember that the V interpoaltion doesn't work on raw strings. ***
	query := '#([a-fA-F0-9]{$n_digit})([a-fA-F0-9]{$n_digit})([a-fA-F0-9]{$n_digit})'
	mut re := regex.regex_opt(query) or { panic(err) }
	start, end := re.match_string(in_col)
	println('start: $start, end: $end')
	mut res := u32(0)
	if start >= 0 {
		group_list := re.get_group_list() // this is the utility function
		r := ('0x' + in_col[group_list[0].start..group_list[0].end]).int() << col_mul
		g := ('0x' + in_col[group_list[1].start..group_list[1].end]).int() << col_mul
		b := ('0x' + in_col[group_list[2].start..group_list[2].end]).int() << col_mul
		println('r: $r g: $g b: $b')
		res = u32(r) << 16 | u32(g) << 8 | u32(b)
	}
	return res
}
```

Others utility functions are `get_group_by_id` and `get_group_bounds_by_id`
that get  directly the string of a group using its `id`:

```v ignore
txt := "my used string...."
for g_index := 0; g_index < re.group_count ; g_index++ {
	println("#${g_index} [${re.get_group_by_id(txt, g_index)}] \
    	bounds: ${re.get_group_bounds_by_id(g_index)}")
}
```

More helper functions are listed in the **Groups query functions** section.

### Groups Continuous saving

In particular situations, it is useful to have a continuous group saving.
This is possible by initializing the `group_csave` field in the `RE` struct.

This feature allows you to collect data in a continuous/streaming way.

In the example, we can pass a text, followed by an integer list,
that we wish to collect. To achieve this task, we can use the continuous
group saving, by enabling the right flag: `re.group_csave_flag = true`.

The `.group_csave` array will be filled then, following this logic:

`re.group_csave[0]` - number of total saved records
`re.group_csave[1+n*3]` - id of the saved group
`re.group_csave[1+n*3]` - start index in the source string of the saved group
`re.group_csave[1+n*3]` - end index in the source string of the saved group

The regex will save groups, until it finishes, or finds that the array has no
more space. If the space ends, no error is raised, and further records will
not be saved.

```v ignore
import regex
fn main(){
    txt   := "http://www.ciao.mondo/hello/pippo12_/pera.html"
    query := r"(?P<format>https?)|(?P<format>ftps?)://(?P<token>[\w_]+.)+"

    mut re := regex.regex_opt(query) or { panic(err) }
    //println(re.get_code())   // uncomment to see the print of the regex execution code
    re.debug=2  // enable maximum log
    println("String: ${txt}")
    println("Query : ${re.get_query()}")
    re.debug=0  // disable log
    re.group_csave_flag = true
    start, end := re.match_string(txt)
    if start >= 0 {
        println("Match ($start, $end) => [${txt[start..end]}]")
    } else {
        println("No Match")
    }

    if re.group_csave_flag == true && start >= 0 && re.group_csave.len > 0{
        println("cg: $re.group_csave")
        mut cs_i := 1
        for cs_i < re.group_csave[0]*3 {
            g_id := re.group_csave[cs_i]
            st   := re.group_csave[cs_i+1]
            en   := re.group_csave[cs_i+2]
            println("cg[$g_id] $st $en:[${txt[st..en]}]")
            cs_i += 3
        }
    }
}
```

The output will be:

```
String: http://www.ciao.mondo/hello/pippo12_/pera.html
Query : #0(?P<format>https?)|{8,14}#0(?P<format>ftps?)://#1(?P<token>[\w_]+.)+
Match (0, 46) => [http://www.ciao.mondo/hello/pippo12_/pera.html]
cg: [8, 0, 0, 4, 1, 7, 11, 1, 11, 16, 1, 16, 22, 1, 22, 28, 1, 28, 37, 1, 37, 42, 1, 42, 46]
cg[0] 0 4:[http]
cg[1] 7 11:[www.]
cg[1] 11 16:[ciao.]
cg[1] 16 22:[mondo/]
cg[1] 22 28:[hello/]
cg[1] 28 37:[pippo12_/]
cg[1] 37 42:[pera.]
cg[1] 42 46:[html]
```

### Named capturing groups

This regex module supports partially the question mark `?` PCRE syntax for groups.

`(?:abcd)` **non capturing group**:  the content of the group will not be saved.

`(?P<mygroup>abcdef)` **named group:** the group content is saved and labeled
as `mygroup`.

The label of the groups is saved in the `group_map` of the `RE` struct,
that is a map from `string` to `int`, where the value is the index in
`group_csave` list of indexes.

Here is an example for how to use them:
```v ignore
import regex
fn main(){
    txt   := "http://www.ciao.mondo/hello/pippo12_/pera.html"
    query := r"(?P<format>https?)|(?P<format>ftps?)://(?P<token>[\w_]+.)+"

    mut re := regex.regex_opt(query) or { panic(err) }
    //println(re.get_code())   // uncomment to see the print of the regex execution code
    re.debug=2  // enable maximum log
    println("String: ${txt}")
    println("Query : ${re.get_query()}")
    re.debug=0  // disable log
    start, end := re.match_string(txt)
    if start >= 0 {
        println("Match ($start, $end) => [${txt[start..end]}]")
    } else {
        println("No Match")
    }

    for name in re.group_map.keys() {
        println("group:'$name' \t=> [${re.get_group_by_name(txt, name)}] \
        bounds: ${re.get_group_bounds_by_name(name)}")
    }
}
```

Output:

```
String: http://www.ciao.mondo/hello/pippo12_/pera.html
Query : #0(?P<format>https?)|{8,14}#0(?P<format>ftps?)://#1(?P<token>[\w_]+.)+
Match (0, 46) => [http://www.ciao.mondo/hello/pippo12_/pera.html]
group:'format' 	=> [http] bounds: (0, 4)
group:'token' 	=> [html] bounds: (42, 46)
```

In order to simplify the use of the named groups, it is possible to
use a name map in the `re` struct, using the function `re.get_group_by_name`.

Here is a more complex example of using them:
```v oksyntax
// This function demostrate the use of the named groups
fn convert_html_rgb_n(in_col string) u32 {
	mut n_digit := if in_col.len == 4 { 1 } else { 2 }
	mut col_mul := if in_col.len == 4 { 4 } else { 0 }
	query := '#(?P<red>[a-fA-F0-9]{$n_digit})' + '(?P<green>[a-fA-F0-9]{$n_digit})' +
		'(?P<blue>[a-fA-F0-9]{$n_digit})'
	mut re := regex.regex_opt(query) or { panic(err) }
	start, end := re.match_string(in_col)
	println('start: $start, end: $end')
	mut res := u32(0)
	if start >= 0 {
		red_s, red_e := re.get_group_by_name('red')
		r := ('0x' + in_col[red_s..red_e]).int() << col_mul
		green_s, green_e := re.get_group_by_name('green')
		g := ('0x' + in_col[green_s..green_e]).int() << col_mul
		blue_s, blue_e := re.get_group_by_name('blue')
		b := ('0x' + in_col[blue_s..blue_e]).int() << col_mul
		println('r: $r g: $g b: $b')
		res = u32(r) << 16 | u32(g) << 8 | u32(b)
	}
	return res
}
```

Other utilities are `get_group_by_name` and `get_group_bounds_by_name`,
that return the string of a group using its `name`:

```v ignore
txt := "my used string...."
for name in re.group_map.keys() {
	println("group:'$name' \t=> [${re.get_group_by_name(txt, name)}] \
    bounds: ${re.get_group_bounds_by_name(name)}")
}
```



### Groups query functions

These functions are helpers to query the captured groups

```v ignore
// get_group_bounds_by_name get a group boundaries by its name
pub fn (re RE) get_group_bounds_by_name(group_name string) (int, int)

// get_group_by_name get a group string by its name
pub fn (re RE) get_group_by_name(group_name string) string

// get_group_by_id get a group boundaries by its id
pub fn (re RE) get_group_bounds_by_id(group_id int) (int,int)

// get_group_by_id get a group string by its id
pub fn (re RE) get_group_by_id(in_txt string, group_id int) string

struct Re_group {
pub:
	start int = -1
	end   int = -1
}

// get_group_list return a list of Re_group for the found groups
pub fn (re RE) get_group_list() []Re_group
```

## Flags

It is possible to set some flags in the regex parser, that change
the behavior of the parser itself.

```v ignore
// example of flag settings
mut re := regex.new()
re.flag = regex.f_bin
```

- `f_bin`: parse a string as bytes, utf-8 management disabled.

- `f_efm`: exit on the first char matches in the query, used by the
           find function.

- `f_ms`:  matches only if the index of the start match is 0,
           same as `^` at the start of the query string.

- `f_me`:  matches only if the end index of the match is the last char
           of the input string, same as `$` end of query string.

- `f_nl`:  stop the matching if found a new line char `\n` or `\r`

## Functions

### Initializer

These functions are helper that create the `RE` struct,
a `RE` struct can be created manually if you needed.

#### **Simplified initializer**

```v ignore
// regex create a regex object from the query string and compile it
pub fn regex_opt(in_query string) ?RE
```

#### **Base initializer**

```v ignore
// new_regex create a REgex of small size, usually sufficient for ordinary use
pub fn new() RE

```
#### **Custom initialization**
For some particular needs, it is possible to initialize a fully customized regex:
```v ignore
pattern = r"ab(.*)(ac)"
// init custom regex
mut re := regex.RE{}
// max program length, can not be longer then the pattern
re.prog = []Token    {len: pattern.len + 1}
// can not be more char class the the length of the pattern
re.cc   = []CharClass{len: pattern.len}

re.group_csave_flag = false          // true enable continuos group saving if needed
re.group_max_nested = 128            // set max 128 group nested possible
re.group_max        = pattern.len>>1 // we can't have more groups than the half of the pattern legth
re.group_stack = []int{len: re.group_max, init: -1}
re.group_data  = []int{len: re.group_max, init: -1}
```
### Compiling

After an initializer is used, the regex expression must be compiled with:

```v ignore
// compile compiles the REgex returning an error if the compilation fails
pub fn (re mut RE) compile_opt(in_txt string)?
```

### Matching Functions

These are the matching functions

```v ignore
// match_string try to match the input string, return start and end index if found else start is -1
pub fn (re mut RE) match_string(in_txt string) (int,int)

```

## Find and Replace

There are the following find  and replace functions:

#### Find functions

```v ignore
// find try to find the first match in the input string
// return start and end index if found else start is -1
pub fn (re mut RE) find(in_txt string) (int,int)

// find_all find all the "non overlapping" occurrences of the matching pattern
// return a list of start end indexes like: [3,4,6,8]
// the matches are [3,4] and [6,8]
pub fn (re mut RE) find_all(in_txt string) []int

// find_all find all the "non overlapping" occurrences of the matching pattern
// return a list of strings
// the result is like ["first match","secon match"]
pub fn (mut re RE) find_all_str(in_txt string) []string
```

#### Replace functions

```v ignore
// replace return a string where the matches are replaced with the repl_str string,
// this function support groups in the replace string
pub fn (re mut RE) replace(in_txt string, repl string) string
```

replace string can include groups references:

```v ignore
txt   := "Today it is a good day."
query := r'(a\w)[ ,.]'
mut re := regex.regex_opt(query)?
res := re.replace(txt, r"__[\0]__")
```

in this example we used the group `0` in the replace string: `\0`, the result will be:

```
Today it is a good day. => Tod__[ay]__it is a good d__[ay]__
```

**Note:** in the replace strings can be used only groups from `0` to `9`.

If the usage of `groups` in the replace process, is not needed, it is possible
to use a quick function:

```v ignore
// replace_simple return a string where the matches are replaced with the replace string
pub fn (mut re RE) replace_simple(in_txt string, repl string) string
```

If it is needed to replace N instances of the found strings it is possible to use:
```v ignore
// replace_n return a string where the firts `count` matches are replaced with the repl_str string
// `count` indicate the number of max replacements that will be done.
// if count is > 0 the replace began from the start of the string toward the end
// if count is < 0 the replace began from the end of the string toward the start
// if count is 0 do nothing
pub fn (mut re RE) replace_n(in_txt string, repl_str string, count int) string
```

#### Custom replace function

For complex find and replace operations, you can use `replace_by_fn` .
The `replace_by_fn`, uses a custom replace callback function, thus
allowing customizations. The custom callback function is called for
every non overlapped find.

The custom callback function must be of the type:

```v ignore
// type of function used for custom replace
// in_txt  source text
// start   index of the start of the match in in_txt
// end     index of the end   of the match in in_txt
// --- the match is in in_txt[start..end] ---
fn (re RE, in_txt string, start int, end int) string
```

The following example will clarify its usage:

```v ignore
import regex
// customized replace functions
// it will be called on each non overlapped find
fn my_repl(re regex.RE, in_txt string, start int, end int) string {
    g0 := re.get_group_by_id(in_txt, 0)
    g1 := re.get_group_by_id(in_txt, 1)
    g2 := re.get_group_by_id(in_txt, 2)
    return "*$g0*$g1*$g2*"
}

fn main(){
    txt   := "today [John] is gone to his house with (Jack) and [Marie]."
    query := r"(.)(\A\w+)(.)"

    mut re := regex.regex_opt(query) or { panic(err) }

    result := re.replace_by_fn(txt, my_repl)
    println(result)
}
```

Output:

```
today *[*John*]* is gone to his house with *(*Jack*)* and *[*Marie*]*.
```



## Debugging

This module has few small utilities to you write regex patterns.

### **Syntax errors highlight**

The next example code shows how to visualize regex pattern syntax errors
in the compilation phase:

```v oksyntax
query := r'ciao da ab[ab-]'
// there is an error, a range not closed!!
mut re := new()
re.compile_opt(query) or { println(err) }
// output!!
// query: ciao da ab[ab-]
// err  : ----------^
// ERROR: ERR_SYNTAX_ERROR
```

### **Compiled code**

It is possible to view the compiled code calling the function `get_query()`.
The result will be something like this:

```
========================================
v RegEx compiler v 1.0 alpha output:
PC:  0 ist: 92000000 (        GROUP_START #:0 {  1,  1}
PC:  1 ist: 98000000 .        DOT_CHAR nx chk: 4 {  1,  1}
PC:  2 ist: 94000000 )        GROUP_END   #:0 {  1,  1}
PC:  3 ist: 92000000 (        GROUP_START #:1 {  1,  1}
PC:  4 ist: 90000000 [\A]     BSLS {  1,  1}
PC:  5 ist: 90000000 [\w]     BSLS {  1,MAX}
PC:  6 ist: 94000000 )        GROUP_END   #:1 {  1,  1}
PC:  7 ist: 92000000 (        GROUP_START #:2 {  1,  1}
PC:  8 ist: 98000000 .        DOT_CHAR nx chk: -1 last! {  1,  1}
PC:  9 ist: 94000000 )        GROUP_END   #:2 {  1,  1}
PC: 10 ist: 88000000 PROG_END {  0,  0}
========================================

```

`PC`:`int` is the program counter or step of execution, each single step is a token.

`ist`:`hex` is the token instruction id.

`[a]` is the char used by the token.

`query_ch` is the type of token.

`{m,n}` is the quantifier, the greedy off flag  `?`  will be showed if present in the token

### **Log debug**

The log debugger allow to print the status of the regex parser when the
parser is running. It is possible to have two different levels of
debug information: 1 is normal, while 2 is verbose.

Here is an example:

*normal* - list only the token instruction with their values

```ignore
// re.flag = 1 // log level normal
flags: 00000000
#   2 s:     ist_load PC:   i,ch,len:[  0,'a',1] f.m:[ -1, -1] query_ch: [a]{1,1}:0 (#-1)
#   5 s:     ist_load PC:   i,ch,len:[  1,'b',1] f.m:[  0,  0] query_ch: [b]{2,3}:0? (#-1)
#   7 s:     ist_load PC:   i,ch,len:[  2,'b',1] f.m:[  0,  1] query_ch: [b]{2,3}:1? (#-1)
#  10 PROG_END
```

*verbose* - list all the instructions and states of the parser

```ignore
flags: 00000000
#   0 s:        start PC: NA
#   1 s:     ist_next PC: NA
#   2 s:     ist_load PC:   i,ch,len:[  0,'a',1] f.m:[ -1, -1] query_ch: [a]{1,1}:0 (#-1)
#   3 s:  ist_quant_p PC:   i,ch,len:[  1,'b',1] f.m:[  0,  0] query_ch: [a]{1,1}:1 (#-1)
#   4 s:     ist_next PC: NA
#   5 s:     ist_load PC:   i,ch,len:[  1,'b',1] f.m:[  0,  0] query_ch: [b]{2,3}:0? (#-1)
#   6 s:  ist_quant_p PC:   i,ch,len:[  2,'b',1] f.m:[  0,  1] query_ch: [b]{2,3}:1? (#-1)
#   7 s:     ist_load PC:   i,ch,len:[  2,'b',1] f.m:[  0,  1] query_ch: [b]{2,3}:1? (#-1)
#   8 s:  ist_quant_p PC:   i,ch,len:[  3,'b',1] f.m:[  0,  2] query_ch: [b]{2,3}:2? (#-1)
#   9 s:     ist_next PC: NA
#  10 PROG_END
#  11 PROG_END
```

the columns have the following meaning:

`#   2` number of actual steps from the start of parsing

`s:     ist_next` state of the present step

`PC:   1` program counter of the step

`=>7fffffff ` hex code of the instruction

`i,ch,len:[  0,'a',1]` `i` index in the source string, `ch` the char parsed,
`len` the length in byte of the char parsed

`f.m:[  0,  1]` `f` index of the first match in the source string, `m` index that is actual matching

`query_ch: [b]` token in use and its char

`{2,3}:1?` quantifier `{min,max}`, `:1` is the actual counter of repetition,
`?` is the greedy off flag if present.

### **Custom Logger output**

The debug functions output uses the `stdout` as default,
it is possible to provide an alternative output, by setting a custom
output function:

```v oksyntax
// custom print function, the input will be the regex debug string
fn custom_print(txt string) {
	println('my log: $txt')
}

mut re := new()
re.log_func = custom_print
// every debug output from now will call this function
```

## Example code

Here an example that perform some basically match of strings

```v ignore
import regex

fn main(){
    txt   := "http://www.ciao.mondo/hello/pippo12_/pera.html"
    query := r"(?P<format>https?)|(?P<format>ftps?)://(?P<token>[\w_]+.)+"

    mut re := regex.regex_opt(query) or { panic(err) }

    start, end := re.match_string(txt)
    if start >= 0 {
        println("Match ($start, $end) => [${txt[start..end]}]")
        for g_index := 0; g_index < re.group_count ; g_index++ {
            println("#${g_index} [${re.get_group_by_id(txt, g_index)}] \
            bounds: ${re.get_group_bounds_by_id(g_index)}")
        }
        for name in re.group_map.keys() {
            println("group:'$name' \t=> [${re.get_group_by_name(txt, name)}] \
            bounds: ${re.get_group_bounds_by_name(name)}")
        }
    } else {
        println("No Match")
    }
}
```
Here an example of total customization of the regex environment creation:
```v ignore
import regex

fn main(){
    txt   := "today John is gone to his house with Jack and Marie."
    query := r"(?:(?P<word>\A\w+)|(?:\a\w+)[\s.]?)+"

    // init regex
    mut re := regex.RE{}
	// max program length, can not be longer then the query
    re.prog = []regex.Token    {len: query.len + 1}
	// can not be more char class the the length of the query
    re.cc   = []regex.CharClass{len: query.len}
    re.prog = []regex.Token    {len: query.len+1}
	// enable continuos group saving
    re.group_csave_flag = true
	// set max 128 group nested
    re.group_max_nested = 128
	// we can't have more groups than the half of the query legth
    re.group_max        = query.len>>1

    // compile the query
    re.compile_opt(query) or { panic(err) }

    start, end := re.match_string(txt)
    if start >= 0 {
        println("Match ($start, $end) => [${txt[start..end]}]")
    } else {
        println("No Match")
    }

    // show results for continuos group saving
    if re.group_csave_flag == true && start >= 0 && re.group_csave.len > 0{
        println("cg: $re.group_csave")
        mut cs_i := 1
        for cs_i < re.group_csave[0]*3 {
            g_id := re.group_csave[cs_i]
            st   := re.group_csave[cs_i+1]
            en   := re.group_csave[cs_i+2]
            println("cg[$g_id] $st $en:[${txt[st..en]}]")
            cs_i += 3
        }
    }

    // show results for captured groups
    if start >= 0 {
        println("Match ($start, $end) => [${txt[start..end]}]")
        for g_index := 0; g_index < re.group_count ; g_index++ {
            println("#${g_index} [${re.get_group_by_id(txt, g_index)}] \
            bounds: ${re.get_group_bounds_by_id(g_index)}")
        }
        for name in re.group_map.keys() {
            println("group:'$name' \t=> [${re.get_group_by_name(txt, name)}] \
            bounds: ${re.get_group_bounds_by_name(name)}")
        }
    } else {
        println("No Match")
    }
}
```

More examples are available in the test code for the `regex` module,
see `vlib/regex/regex_test.v`.
