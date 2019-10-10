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

