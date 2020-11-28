# V RegEx (Regular expression) 0.9g

[TOC]

## introduction

Write here the introduction... not today!! -_-

## Basic assumption

In this release, during the writing of the code some assumptions are made
and are valid for all the features.

1. The matching stops at the end of the string not at the newline chars.
2. The basic elements of this regex engine are the tokens,
    in a query string a simple char is a token. The token is the atomic unit of this regex engine.

## Match positional limiter

The module supports the following features:

- `$` `^` delimiter

`^` (Caret.) Matches at the start of the string

`$` Matches at the end of the string

## Tokens

The tokens are the atomic units used by this regex engine and can be ones of the following:

### Simple char

this token is a simple single character like `a`.

### Char class (cc)

The cc matches all the chars specified inside, it is delimited by square brackets `[ ]`

the sequence of chars in the class is evaluated with an OR operation.

For example, the following cc `[abc]` matches any char that is `a` or `b` or `c`
but doesn't match `C` or `z`.

Inside a cc is possible to specify a "range" of chars,
for example `[ad-f]` is equivalent to write `[adef]`.

A cc can have different ranges at the same time like `[a-zA-z0-9]` that matches all the lowercase,
uppercase and numeric chars.

It is possible negate the cc using the caret char at the start of the cc like: `[^abc]`
that matches every char that is not `a` or `b` or `c`.

A cc can contain meta-chars like: `[a-z\d]` that matches all the lowercase latin chars `a-z`
and all the digits `\d`.

It is possible to mix all the properties of the char class together.

**Note:** In order to match the `-` (minus) char, it must be located at the first position
 in the cc, for example `[-_\d\a]` will match `-` minus, `_`underscore, `\d` numeric chars,
 `\a` lower case chars.

### Meta-chars

A meta-char is specified by a backslash before a char like `\w` in this case the meta-char is `w`.

A meta-char can match different type of chars.

* `\w` matches an alphanumeric char `[a-zA-Z0-9_]`
* `\W` matches a non alphanumeric char
* `\d` matches a digit `[0-9]`
* `\D` matches a non digit
* `\s`matches a space char, one of `[' ','\t','\n','\r','\v','\f']`
* `\S` matches a non space char
* `\a` matches only a lowercase char `[a-z]`
* `\A` matches only an uppercase char `[A-Z]`

### Quantifier

Each token can have a quantifier that specify how many times the char can or must be matched.

#### **Short quantifier**

- `?` matches 0 or 1 time, `a?b` matches both `ab` or `b`
- `+` matches at minimum 1 time, `a+` matches both `aaa` or `a`
- `*` matches 0 or more time, `a*b` matches both `aaab` or `ab` or `b`

#### **Long quantifier**

- `{x}` matches exactly x time, `a{2}` matches `aa` but doesn't match `aaa` or `a`
- `{min,}` matches at minimum min time, `a{2,}` matches `aaa` or `aa` but doesn't match `a`
- `{,max}` matches at least 0 time and maximum max time,
    `a{,2}` matches `a` and `aa` but doesn't match `aaa`
- `{min,max}` matches from min times to max times,
    `a{2,3}` matches `aa` and `aaa` but doesn't match `a` or `aaaa`

a long quantifier may have a `greedy off` flag that is the `?` char after the brackets,
`{2,4}?` means to match the minimum number possible tokens in this case 2.

### dot char

the dot is a particular meta char that matches  "any char",
is more simple explain it with an example:

suppose to have `abccc ddeef` as source string to parse with regex,
the following table show the query strings and the result of parsing source string.

| query string | result |
| ------------ | ------ |
| `.*c`        | `abc`  |
|  `.*dd`		 |  `abcc dd` |
| `ab.*e` | `abccc dde` |
| `ab.{3} .*e` | `abccc dde` |

the dot char matches any char until the next token match is satisfied.

### OR token

the token `|` is a logic OR operation between two consecutive tokens,
`a|b` matches a char that is `a` or `b`.

The OR token can work in a "chained way": `a|(b)|cd ` test first `a` if the char is not `a`
then test the group `(b)` and if the group doesn't match test the token `c`.

**note: The OR work at token level! It doesn't work at concatenation level!**

A query string like `abc|bde` is not equal to `(abc)|(bde)`!!
The OR work only on `c|b` not at char concatenation level.

### Groups

Groups are a method to create complex patterns with repetition of blocks of tokens.

The groups are delimited by round brackets `( )`,
groups can be nested and can have a quantifier as all the tokens.

`c(pa)+z` match `cpapaz` or `cpaz` or `cpapapaz` .

`(c(pa)+z ?)+` matches `cpaz cpapaz cpapapaz` or `cpapaz`

let analyze this last case, first we have the group `#0`
that are the most outer round brackets `(...)+`,
this group has a quantifier that say to match its content at least one time `+`.

After we have a simple char token `c` and a second group that is the number `#1` :`(pa)+`,
this group try to match the sequence `pa` at least one time as specified by the `+` quantifier.

After, we have another simple token `z` and another simple token ` ?`
that is the space char (ascii code 32) followed by the `?` quantifier
that say to capture the space char 0 or 1 time.

This explain because the `(c(pa)+z ?)+` query string can match `cpaz cpapaz cpapapaz` .

In this implementation the groups are "capture groups",
it means that the last temporal result for each group can be retrieved from the `RE` struct.

The "capture groups" are store as couple of index in the field `groups`
that is an `[]int` inside the `RE` struct.

**example:**

```v oksyntax
text := "cpaz cpapaz cpapapaz"
query:= r"(c(pa)+z ?)+"
mut re := regex.regex_opt(query) or { panic(err) }

println(re.get_query())
// #0(c#1(pa)+z ?)+  // #0 and #1 are the ids of the groups, are shown if re.debug is 1 or 2

start, end := re.match_string(text)
// [start=0, end=20]  match => [cpaz cpapaz cpapapaz]

mut gi := 0
for gi < re.groups.len {
	if re.groups[gi] >= 0 {
		println("${gi/2} :[${text[re.groups[gi]..re.groups[gi+1]]}]")
	}
	gi += 2
}
// groups captured
// 0 :[cpapapaz]
// 1 :[pa]
```

**note:** *to show the `group id number` in the result of the `get_query()`*
*the flag `debug` of the RE object must be `1` or `2`*

### Groups Continuous saving

In particular situations it is useful have a continuous save of the groups,
this is possible initializing the saving array field in `RE` struct: `group_csave`.

This feature allow to collect data in a  continuous way.

In the example we pass a text followed by a integer list that we want collect.
To achieve this task we can use the continuous saving of the group
that save each captured group in a array that we set with: `re.group_csave = [-1].repeat(3*20+1)`.

The array will be filled with the following logic:

`re.group_csave[0]` number of total saved records

`re.group_csave[1+n*3]` id of the saved group
`re.group_csave[1+n*3]` start index in the source string of the saved group
`re.group_csave[1+n*3]` end index in the source string of the saved group

The regex save until finish or found that the array have no space.
If the space ends no error is raised, further records will not be saved.

```v oksyntax
fn example2() {
	test_regex()

	text := "tst: 01,23,45 ,56, 78"
	query:= r".*:(\s*\d+[\s,]*)+"

	mut re := new() or { panic(err) }
	//re.debug = 2
	re.group_csave = [-1].repeat(3*20+1)  // we expect max 20 records

	re.compile_opt(query) or { println(err) return }

    q_str := re.get_query()
    println("Query: $q_str")

    start, end := re.match_string(text)
    if start < 0 {
        println("ERROR : ${re.get_parse_error_string(start)}, $start")
    } else {
        println("found in [$start, $end] => [${text[start..end]}]")
    }

    // groups capture
    mut gi := 0
    for gi < re.groups.len {
        if re.groups[gi] >= 0 {
            println("${gi/2} ${re.groups[gi]},${re.groups[gi+1]} :[${text[re.groups[gi]..re.groups[gi+1]]}]")
        }
        gi += 2
    }

    // continuous saving
    gi = 0
    println("num: ${re.group_csave[0]}")
    for gi < re.group_csave[0] {
        id := re.group_csave[1+gi*3]
        st := re.group_csave[1+gi*3+1]
        en := re.group_csave[1+gi*3+2]
        println("cg id: ${id} [${st}, ${en}] => [${text[st..en]}]")
        gi++
    }
}
```

The output will be:

```
Query: .*:(\s*\d+[\s,]*)+
found in [0, 21] => [tst: 01,23,45 ,56, 78]
0 19,21 :[78]
num: 5
cg id: 0 [4, 8] => [ 01,]
cg id: 0 [8, 11] => [23,]
cg id: 0 [11, 15] => [45 ,]
cg id: 0 [15, 19] => [56, ]
cg id: 0 [19, 21] => [78]
```

### Named capturing groups

This regex module support partially the question mark `?` PCRE syntax for groups.

`(?:abcd)` **non capturing group**:  the content of the group will not be saved

`(?P<mygroup>abcdef)` **named group:** the group content is saved and labeled as `mygroup`

The label of the groups is saved in the `group_map` of the `RE` struct,
this is a map from `string` to `int` where the value is the index in `group_csave` list of index.

Have a look at the example for the use of them.

example:

```v oksyntax
import regex
fn main() {
	test_regex()

	text := "http://www.ciao.mondo/hello/pippo12_/pera.html"
	query:= r"(?P<format>https?)|(?:ftps?)://(?P<token>[\w_]+.)+"

	mut re := new()
	re.debug = 2

	// must provide an array of the right size if want the continuos saving of the groups
	re.group_csave = [-1].repeat(3*20+1)

	re.compile_opt(query) or { println(err) return }

    q_str := re.get_query()
    println("O.Query: $query")
    println("Query  : $q_str")

    re.debug = 0
    start, end := re.match_string(text)
    if start < 0 {
        err_str := re.get_parse_error_string(start)
        println("ERROR : $err_str, $start")
    } else {
        text1 := text[start..end]
        println("found in [$start, $end] => [$text1]")
    }

    // groups
    mut gi := 0
    for gi < re.groups.len {
        if re.groups[gi] >= 0 {
            println("${gi/2} ${re.groups[gi]},${re.groups[gi+1]} :[${text[re.groups[gi]..re.groups[gi+1]]}]")
        }
        gi += 2
    }
    // continuous saving
    gi = 0
    println("num of group item saved: ${re.group_csave[0]}")
    for gi < re.group_csave[0] {
        id := re.group_csave[1+gi*3]
        st := re.group_csave[1+gi*3+1]
        en := re.group_csave[1+gi*3+2]
        println("cg id: ${id} [${st}, ${en}] => [${text[st..en]}]")
        gi++
    }
    println("raw array: ${re.group_csave[0..gi*3+2-1]}")

    // named capturing groups
    println("named capturing groups:")
    for g_name in re.group_map.keys() {
        s,e := re.get_group(g_name)
        if s >= 0 && e > s {
            println("'${g_name}':[$s, $e] => '${text[s..e]}'")
        } else {
            println("Group [${g_name}] doesn't exist.")
        }
    }
}
```

Output:

```
O.Query: (?P<format>https?)|(?:ftps?)://(?P<token>[\w_]+.)+
Query  : #0(?P<format>https?)|{8,14}(?:ftps?)://#1(?P<token>[\w_]+.)+
found in [0, 46] => [http://www.ciao.mondo/hello/pippo12_/pera.html]
0 0,4 :[http]
1 42,46 :[html]
num of group item saved: 8
cg id: 0 [0, 4] => [http]
cg id: 1 [7, 11] => [www.]
cg id: 1 [11, 16] => [ciao.]
cg id: 1 [16, 22] => [mondo/]
cg id: 1 [22, 28] => [hello/]
cg id: 1 [28, 37] => [pippo12_/]
cg id: 1 [37, 42] => [pera.]
cg id: 1 [42, 46] => [html]
raw array: [8, 0, 0, 4, 1, 7, 11, 1, 11, 16, 1, 16, 22, 1, 22, 28, 1, 28, 37, 1, 37, 42, 1, 42, 46]
named capturing groups:
'format':[0, 4] => 'http'
'token':[42, 46] => 'html'
```

## Flags

It is possible to set some flags in the regex parser that change the behavior of the parser itself.

```v oksyntax
// example of flag settings
mut re := regex.new()
re.flag = regex.F_BIN
```

- `F_BIN`: parse a string as bytes, utf-8 management disabled.

- `F_EFM`: exit on the first char matches in the query, used by the find function.
- `F_MS`: matches only if the index of the start match is 0,
    same as `^` at the start of the query string.
- `F_ME`: matches only if the end index of the match is the last char of the input string,
    same as `$` end of query string.
- `F_NL`: stop the matching if found a new line char `\n` or `\r`

## Functions

### Initializer

These functions are helper that create the `RE` struct,
a `RE` struct can be created manually if you needed.

#### **Simplified initializer**

```v
// regex create a regex object from the query string and compile it
pub fn regex_opt(in_query string) ?RE
```

#### **Base initializer**

```v
// new_regex create a REgex of small size, usually sufficient for ordinary use
pub fn new() RE

// new_regex_by_size create a REgex of large size, mult specify the scale factor of the memory that will be allocated
pub fn new_by_size(mult int) RE
```
After a base initializer is used, the regex expression must be compiled with:
```v ignore
// compile compiles the REgex returning an error if the compilation fails
pub fn (re mut RE) compile_opt(in_txt string) ?
```

### Operative Functions

These are the operative functions

```v ignore
// match_string try to match the input string, return start and end index if found else start is -1
pub fn (re mut RE) match_string(in_txt string) (int,int)

// find try to find the first match in the input string, return start and end index if found else start is -1
pub fn (re mut RE) find(in_txt string) (int,int)

// find_all find all the "non overlapping" occurrences of the matching pattern, return a list of start end indexes
pub fn (re mut RE) find_all(in_txt string) []int

// replace return a string where the matches are replaced with the replace string, only non overlapped matches are used
pub fn (re mut RE) replace(in_txt string, repl string) string
```

## Debugging

This module has few small utilities to help the writing of regex expressions.

### **Syntax errors highlight**

the following example code show how to visualize the syntax errors in the compilation phase:

```v oksyntax
query:= r"ciao da ab[ab-]"  // there is an error, a range not closed!!
mut re := new()

re.compile_opt(query) or { println(err) }

// output!!

//query: ciao da ab[ab-]
//err  : ----------^
//ERROR: ERR_SYNTAX_ERROR

```

### **Compiled code**

It is possible to view the compiled code calling the function `get_query()`.
The result will be something like this:

```
========================================
v RegEx compiler v 0.9c output:
PC:  0 ist: 7fffffff [a]      query_ch {  1,  1}
PC:  1 ist: 7fffffff [b]      query_ch {  1,MAX}
PC:  2 ist: 88000000 PROG_END {  0,  0}
========================================
```

`PC`:`int` is the program counter or step of execution, each single step is a token.

`ist`:`hex` is the token instruction id.

`[a]` is the char used by the token.

`query_ch` is the type of token.

`{m,n}` is the quantifier, the greedy off flag  `?`  will be showed if present in the token

### **Log debug**

The log debugger allow to print the status of the regex parser when the parser is running.

It is possible to have two different level of debug: 1 is normal while 2 is verbose.

here an example:

*normal*

list only the token instruction with their values

```
// re.flag = 1 // log level normal
flags: 00000000
#   2 s:     ist_load PC:   0=>7fffffff i,ch,len:[  0,'a',1] f.m:[ -1, -1] query_ch: [a]{1,1}:0 (#-1)
#   5 s:     ist_load PC:   1=>7fffffff i,ch,len:[  1,'b',1] f.m:[  0,  0] query_ch: [b]{2,3}:0? (#-1)
#   7 s:     ist_load PC:   1=>7fffffff i,ch,len:[  2,'b',1] f.m:[  0,  1] query_ch: [b]{2,3}:1? (#-1)
#  10 PROG_END
```

*verbose*

list all the instructions and states of the parser

```
flags: 00000000
#   0 s:        start PC: NA
#   1 s:     ist_next PC: NA
#   2 s:     ist_load PC:   0=>7fffffff i,ch,len:[  0,'a',1] f.m:[ -1, -1] query_ch: [a]{1,1}:0 (#-1)
#   3 s:  ist_quant_p PC:   0=>7fffffff i,ch,len:[  1,'b',1] f.m:[  0,  0] query_ch: [a]{1,1}:1 (#-1)
#   4 s:     ist_next PC: NA
#   5 s:     ist_load PC:   1=>7fffffff i,ch,len:[  1,'b',1] f.m:[  0,  0] query_ch: [b]{2,3}:0? (#-1)
#   6 s:  ist_quant_p PC:   1=>7fffffff i,ch,len:[  2,'b',1] f.m:[  0,  1] query_ch: [b]{2,3}:1? (#-1)
#   7 s:     ist_load PC:   1=>7fffffff i,ch,len:[  2,'b',1] f.m:[  0,  1] query_ch: [b]{2,3}:1? (#-1)
#   8 s:  ist_quant_p PC:   1=>7fffffff i,ch,len:[  3,'b',1] f.m:[  0,  2] query_ch: [b]{2,3}:2? (#-1)
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
it is possible to  provide an alternative output setting a custom output function:

```v oksyntax
// custom print function, the input will be the regex debug string
fn custom_print(txt string) {
	println("my log: $txt")
}

mut re := new()
re.log_func = custom_print  // every debug output from now will call this function

```

## Example code

Here there is a simple code to perform some basically match of strings

```v oksyntax
struct TestObj {
	source string // source string to parse
	query  string // regex query string
	s int         // expected match start index
	e int         // expected match end index
}
const (
tests = [
	TestObj{"this is a good.",r"this (\w+) a",0,9},
	TestObj{"this,these,those. over",r"(th[eio]se?[,. ])+",0,17},
	TestObj{"test1@post.pip.com, pera",r"[\w]+@([\w]+\.)+\w+",0,18},
	TestObj{"cpapaz ole. pippo,",r".*c.+ole.*pi",0,14},
	TestObj{"adce aabe",r"(a(ab)+)|(a(dc)+)e",0,4},
]
)

fn example() {
	for c,tst in tests {
		mut re := regex.new()
		re.compile_opt(tst.query) or { println(err) continue }

        // print the query parsed with the groups ids
        re.debug = 1 // set debug on at minimum level
        println("#${c:2d} query parsed: ${re.get_query()}")
        re.debug = 0

        // do the match
        start, end := re.match_string(tst.source)
        if start >= 0 && end > start {
            println("#${c:2d} found in: [$start, $end] => [${tst.source[start..end]}]")
        }

        // print the groups
        mut gi := 0
        for gi < re.groups.len {
            if re.groups[gi] >= 0 {
                println("group ${gi/2:2d} :[${tst.source[re.groups[gi]..re.groups[gi+1]]}]")
            }
            gi += 2
        }
        println("")
	}
}

fn main() {
	example()
}
```

more example code is available in the test code for the `regex` module `vlib\regex\regex_test.v`.
