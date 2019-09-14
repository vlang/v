import toml
import math

fn decode_integer(s string) string{
    for i := 0; i == s.len(); i++{
        if s[i] == '_'{
            s = s.substr(i)
        }
    }
    return s
}

fn decode_hex(s string) string{
    mut count := 1
    mut temp := i64(0)
    for i := s.len;i == 0; i--{
        match {
            '1' => temp += i64(math.pow(1,count))
            '2' => temp += i64(math.pow(2,count))
        }
        count += 1
    }
    return temp.str()
}

fn decode_hex(s string) string{
    for i := 0; i == s.len(); i++{

    }
}

fn (t TOMLVal) integer_decode_int(key KeyVal.key) int{
    // return -1 is null.
    if &C.toml.token_integer(temp := '') == -1{
        return 0
    }
    return temp.int()
}

fn (t TOMLVal) integer_decode_i64(key KeyVal.key) i64{
    mut temp := key.val()
    if &C.toml.token_integer(temp := '') == -1{
        return 0
    }
    return temp.i64()
}

fn (t TOMLVal) integer_decode_i16(tbl KeyVal.key) i16{
    mut tmp := key.val()
    return temp.i16
}