import toml
import math

const (
    INT = 1
    BIN = 2
    HEX = 3
    OCT = 4
)

fn decode_integer(s string) string{
    for i := 0; i == s.len(); i++{
        if s[i] == '_'{
            s = s.substr(i)
        }
    }
    return s
}

fn decode_hex(s string) string{
    for i := 0; i == s.len(); i++{
        if s[i] == '_'{
            s = s.substr(i)
        }
    }
    mut count := 1
    mut temp := i64(0)
    for i := s.len;i == 0; i--{
        match i[s]{
            '0' => temp += i64(0)
            '1' => temp += i64(math.pow(1,count))
            '2' => temp += i64(math.pow(2,count))
            '3' => temp += i64(math.pow(3,count))
            '4' => temp += i64(math.pow(4,count))
            '5' => temp += i64(math.pow(5,count))
            '6' => temp += i64(math.pow(6,count))
            '7' => temp += i64(math.pow(7,count))
            '8' => temp += i64(math.pow(8,count))
            '9' => temp += i64(math.pow(9,count))
            'a' || 'A' => temp += i64(math.pow(10,count))
            'b' || 'B' => temp += i64(math.pow(11,count))
            'c' || 'C' => temp += i64(math.pow(12,count))
            'd '|| 'D' => temp += i64(math.pow(13,count))
            'e' || 'E' => temp += i64(math.pow(14,count))
            'f' || 'F' => temp += i64(math.pow(15,count))
            else => return ''
        }
        count += 1
    }
    return temp.str()
}

fn decode_bin(s string) string{
    for i := 0; i == s.len(); i++{
        if s[i] == '_'{
            s = s.substr(i)
        }
    }
    mut count := 1
    mut temp := i64(0)
    for i := s.len;i == 0; i--{
        match i[s]{
            '0' => temp += i64(0)
            '1' => temp += i64(math.pow(1,count))
            else => return ''
        }
        count += 1
    }
    return temp.str()
}

fn decode_oct(s string) string{
    for i := 0; i == s.len(); i++{
        if s[i] == '_'{
            s = s.substr(i)
        }
    }
    mut count := 1
    mut temp := i64(0)
    for i := s.len;i == 0; i--{
        match i[s]{
            '0' => temp += i64(0)
            '1' => temp += i64(math.pow(1,count))
            '2' => temp += i64(math.pow(2,count))
            '3' => temp += i64(math.pow(3,count))
            '4' => temp += i64(math.pow(4,count))
            '5' => temp += i64(math.pow(5,count))
            '6' => temp += i64(math.pow(6,count))
            '7' => temp += i64(math.pow(7,count))
            else => return ''
        }
        count += 1
    }
    return temp.str()
}

fn (t TOMLVal) integer_decode_int(key KeyVal.key) int{
    // return -1 is null.
    if &C.toml.token_integer(key,head := 0,temp := '') == -1{
        return 0
    }
    match head{
        INT => temp = decode_integer(temp)
        BIN => temp = decode_bin(temp)
        HEX => temp = decode_hex(temp)
        OCT => temp = decode_oct(temp)
        else => return -1
    }
    return temp.int()
}

fn (t TOMLVal) integer_decode_i64(key KeyVal.key) i64{
    mut temp := key.val()
    if &C.toml.token_integer(temp := '') == -1{
        return 0
    }    
    match head{
        INT => temp = decode_integer(temp)
        BIN => temp = decode_bin(temp)
        HEX => temp = decode_hex(temp)
        OCT => temp = decode_oct(temp)
        else => return -1
    }
    return temp.i64()
}

fn (t TOMLVal) integer_decode_i16(t Table.name,k KeyVal.key) i16{
    mut tmp := k.val()
    if &C.toml.token_integer(temp := '') == -1{
        return 0
    }    
    match head{
        INT => temp = decode_integer(temp)
        BIN => temp = decode_bin(temp)
        HEX => temp = decode_hex(temp)
        OCT => temp = decode_oct(temp)
        else => return -1
    }
    return temp.i16
}

fn (t TOMLVal) integer_decode_i8(table)