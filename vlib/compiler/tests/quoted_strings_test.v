
// first comment

/* multi
   line 
   comment
   with // embedded internal line comments


*/

fn test_simple_quoted_strings(){
	s := 'testing'
	raw_s_a       := r'testing'
	raw_s_q       := r"testing"
	quoted_s      :=  "testing"
	///////////////////////////////////////
	assert raw_s_a == raw_s_q
	assert s == quoted_s
	assert s == raw_s_a
	assert s == raw_s_q
}

fn test_quoted_apostrophes(){
	s         := r"this 'should' be quoted"
	quoted_s  :=  "this 'should' be quoted"
	assert s == quoted_s
}

fn test_simple_interpolated_variables(){
	s := 'abc'
	quoted_s      := "$s"
	apostrophed_s := '$s'
	assert s == quoted_s
	assert s == apostrophed_s
}

fn test_interpolated_variables_in_curly_braces_with_format_params(){
	s := 'abc'
	preformatted_string := '|       abc|'
	quoted_s      := " ${s:10s} "
	apostrophed_s := ' ${s:10s} '
	assert preformatted_string == quoted_s
	assert preformatted_string == apostrophed_s
	assert true
	assert s > 'x'
	assert false
	assert is_matching('a','b') > 10
}

fn is_matching(s1 string, s2 string) int {
	return s1.len - s2.len
}
///////////////////////////////////////////////////////////////////////////////

/* TODO: V error: C error
fn test_apostrophed_quotes(){
	s             := r'this "should" be quoted'
	apostrophed_s :=  'this "should" be quoted'
	assert s == apostrophed_s
}
*/


/* TODO: compiler error: expected `|` but got `:`
fn test_interpolated_variables_in_curly_braces(){
	s := 'abc'
	quoted_s      := "${s}"
	apostrophed_s := '$s'
	assert s == quoted_s
	assert s == apostrophed_s
}
*/
