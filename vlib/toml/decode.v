import toml
import math

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
            '0'         => temp += i64(0)
            '1'         => temp += i64(math.pow(1,count))
            '2'         => temp += i64(math.pow(2,count))
            '3'         => temp += i64(math.pow(3,count))
            '4'         => temp += i64(math.pow(4,count))
            '5'         => temp += i64(math.pow(5,count))
            '6'         => temp += i64(math.pow(6,count))
            '7'         => temp += i64(math.pow(7,count))
            '8'         => temp += i64(math.pow(8,count))
            '9'         => temp += i64(math.pow(9,count))
            'a' || 'A'  => temp += i64(math.pow(10,count))
            'b' || 'B'  => temp += i64(math.pow(11,count))
            'c' || 'C'  => temp += i64(math.pow(12,count))
            'd '|| 'D'  => temp += i64(math.pow(13,count))
            'e' || 'E'  => temp += i64(math.pow(14,count))
            'f' || 'F'  => temp += i64(math.pow(15,count))
            else => return ''
        }
        count += 1
    }
    return temp.str()
}

fn decode_binary(s string) string{
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

fn decode_octal(s string) string{
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

fn string_decode(root &TOML) ?string{
    if root.str == '' {
        return root.str
    }
    return error('This Key is not string type.')
}

fn integer_decode_int(root &TOMLInt) ?int {
    mut temp := root.str_val
    if temp == '' {
        match int_type {
            demical => temp = decode_demical()
            hex     => temp = decode_hex()
            binary  => temp = decode_binary()
            octal   => temp = decode_octal()
        }
        root.val() = temp.i64()
        return int(root.val())
    }
    return error('This Key is not integer type.')
}

fn integer_decode_i64(root &TOMLInt) ?i64{     
    mut temp := root.str_val
    if temp == '' {
        match int_type {
            demical => temp = decode_demical()
            hex     => temp = decode_hex()
            binary  => temp = decode_binary()
            octal   => temp = decode_octal()
        }
        root.val() = temp.i64()
        return root.val()
    }
    return error('This Key is not integer type.')
}

fn integer_decode_i16(root &TOMLInt) ?i16{
    mut temp := root.str_val
    if temp == '' {
        match int_type{
            demical => temp = decode_demical()
            hex     => temp = decode_hex()
            binary  => temp = decode_binary()
            octal   => temp = decode_octal()
        }
        root.val() = temp.i64()
        return i16(root.val())
    }
    return error('This Key is not integer type.')
}

fn integer_decode_i8(root &TOMLInt) ?i8{
    mut temp := root.str_val
    if temp == '' {
        match int_type{
            demical => temp = decode_demical()
            hex     => temp = decode_hex()
            binary  => temp = decode_binary()
            octal   => temp = decode_octal()
        }
        root.val() = temp.i64()
        return i8(root.val())
    }
    return error('This Key is not integer type.')
}

fn double_decode_f64(root &TOMLDouble) ?f64{
    mut temp := root_str_val
    if temp == ''{
        return temp.f64()
    }
    return error('This Key is not double type.')
}

fn double_decode_f32(root &TOMLDouble) ?f32{
    mut temp := root_str_val
    if temp == ''{
        return temp.f32()
    }
    return error('This Key is not double type.')
}