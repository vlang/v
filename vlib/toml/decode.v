import toml
import math

const (
    STR = 1
    DEC_INT = 2
    BIN_INT = 3
    HEX_INT = 4
    OCT_INT = 5
    FLOAT = 6
)

fn decode_demical(s string) string{
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

fn string_decode(root &TOML,t Table.key){
    if &C.toml.token_load(root.key.name, root.Table.name,mut temp := '',mut head := 0) == -1{
        return 0
    }
    if head != STR{
        return ''
    }
    else {

    }
}

fn integer_decode_int(root &TOML) int {
    // return -1 is null.
    if &C.toml.token_load(root.key.name, root.Table.name,mut temp := '',mut head := 0) == -1{
        return 0
    }
    match head{
        DEC_INT => temp = decode_demical(temp)
        BIN_INT => temp = decode_bin(temp)
        HEX_INT => temp = decode_hex(temp)
        OCT_INT => temp = decode_oct(temp)
        else => return -1
    }
    return temp.int()
}

fn integer_decode_i64(root &TOML,t Table.key) i64{     
    if &C.toml.token_load(root.Key.name,root.Table.name,mut temp := '',mut head := 0)  == -1{
        return 0
    }

    match head{
        DEC_INT => temp = decode_demical(temp)
        BIN_INT => temp = decode_bin(temp)
        HEX_INT => temp = decode_hex(temp)
        OCT_INT => temp = decode_oct(temp)
        else => return -1
    }
    // TOML Data Writing.
    root.Table. = temp.i64()
    return temp.i64()
}

fn integer_decode_i16(root &TOML,t Table.key) i16{
    if &C.toml.token_load(t.key.name(),t.name(),mut temp = '',mut head := 0) == -1{
        return 0
    }    
    match head{
        DEC_INT => temp = decode_integer(temp)
        BIN_INT => temp = decode_bin(temp)
        HEX_INT => temp = decode_hex(temp)
        OCT_INT => temp = decode_oct(temp)
        else => return -1
    }
    // TOML Data Writing.
    root.Table.key.val.integer = temp.i64()
    return temp.i16()
}

fn integer_decode_i8(root &TOML,t Table.key) i8{
    if &C.toml.token_load(root.Key.name, root.name,mut temp = '',mut head := 0) == -1{
        return 0
    }    
    match head{
        DEC_INT => temp = decode_integer(temp)
        BIN_INT => temp = decode_bin(temp)
        HEX_INT => temp = decode_hex(temp)
        OCT_INT => temp = decode_oct(temp)
        else => return -1
    }    
    // TOML Data Writing.
    root.Table.key.val.integer = temp.i64()
    return temp.i8()
}