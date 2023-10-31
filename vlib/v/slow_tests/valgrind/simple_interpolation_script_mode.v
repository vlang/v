// TODO: @medvednikov: autofree on script mode does not work when first variable is on position 0 due to the code in `cgen.v:1115`
v := 't'
s := '${v}.tmp'
println(s)
