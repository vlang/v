import testing

fn test_is_same_string(){
        ok_assert := testing.is("a", "a", "should match same string")
        assert ok_assert == true
}

fn test_is_different_string(){
        ok_assert := testing.is("a", "b", "should match same string")
        assert ok_assert == false
}

fn test_is_not_same_string(){
        ok_assert := testing.is_not("a", "a", "should not match same string")
        assert ok_assert == false
}

fn test_is_not_different_string(){
        ok_assert := testing.is_not("a", "b", "should not match same string")
        assert ok_assert == true
}

fn test_is_match_on_matched_string(){
        ok_assert := testing.is_match("fox", "The quick brown fox jumps over the lazy dog", "should match substring")
        assert ok_assert == true
}

fn test_is_match_on_nonmatch_string(){
        ok_assert := testing.is_match("wolf", "The quick brown fox jumps over the lazy dog", "should match substring")
        assert ok_assert == false
}

// // OUTPUTS
// Testing...
// -------------------------------------------------
// compilation took: 883ms
// running tests in: /home/michiel/v/src/github.com/mvlootman/v/vlib/testing/testing_test.v
//    OK     0 ms |  1 assert  | test_is_same_string()
// expected: a
// actual  : b
// reason  : should match same string
//    OK     0 ms |  1 assert  | test_is_different_string()
// expected: not `a`
// actual  : a
// reason  : should not match same string
//    OK     0 ms |  1 assert  | test_is_not_same_string()
//    OK     0 ms |  1 assert  | test_is_not_different_string()
//    OK     0 ms |  1 assert  | test_is_match_on_matched_string()
// expected: `wolf` to be in the actual string
// actual  : The quick brown fox jumps over the lazy dog
// reason  : should match substring
//    OK     0 ms |  1 assert  | test_is_match_on_nonmatch_string()
//           0 ms | <=== total time spent running V tests in "testing_test.v" 
//  ok, fail, total =     6,     0,     6
// ----------------------------------------------------------------------------
//    887 ms | <=== total time spent running V _test.v files 
//  ok, fail, total =     1,     0,     1