import (
    json
    toml
)

fn tomlint_to_jsonnumber_int(root &TOMLInt){
    jsencode_int(root.val())
}

fn tomlint_to_jsonnumber_i64(root &TOMLInt) {
    jsencode_i64(root.val())
}

fn tomldouble_to_jsonnumber_f64(root &TOMLDouble){

}

fn tomldouble_to_jsonnumber_f32(root &TOMLDouble){

}