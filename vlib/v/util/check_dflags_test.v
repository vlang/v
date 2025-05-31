module main

#flag -DCNUMBER1=$d('N',1231)
#flag -DCNUMBER2=$d('N', 1232)
#flag -DCNUMBER3=$d('N',1233 )
#flag -DCNUMBER4=$d('N', 1234 )
#flag -DCNUMBER5=$d('N',  1235  )
#flag -DCNUMBER6=$d('N',1236) ##

fn test_if_compilation_with_custom_cflags_works_numbers_simple() {
	assert C.CNUMBER1 == 1231
	assert C.CNUMBER2 == 1232
	assert C.CNUMBER3 == 1233
	assert C.CNUMBER4 == 1234
	assert C.CNUMBER5 == 1235
	assert C.CNUMBER6 == 1236
}

#flag -DCNUMBERS1=$d('N',123)+$d('N',123)
#flag -DCNUMBERS2=$d('N', 123)-$d('N', 123)
#flag -DCNUMBERS3=$d('N',123 )*$d('N',123 )
#flag -DCNUMBERS4=$d('N', 123 )/$d('N', 123 )
#flag -DCNUMBERS5=$d('N',  123  )+2*$d('N',  123  )
#flag -DCNUMBERS6=$d('N',123)+1000*$d('N',123) ##

fn test_if_compilation_with_custom_cflags_works_numbers_composed_arithmetic() {
	assert C.CNUMBERS1 == 246
	assert C.CNUMBERS2 == 0
	assert C.CNUMBERS3 == 15129
	assert C.CNUMBERS4 == 1
	assert C.CNUMBERS5 == 369
	assert C.CNUMBERS6 == 123123
}

#flag -DFNAME0=$d('A1','"printf"')
#flag -DFNAME1=$d('A1','"print')$d('A2','f"')
#flag -DFNAME2=$d('A1', 'print')$d('A2','f')
#flag -DFNAME3=$d('A1','prin' )$d('A2','tf')
#flag -DFNAME4=$d('A1', 'pri' )$d('A2','ntf')
#flag -DFNAME5=$d('A1', 'pr' )$d('A2','intf') ##

fn test_custom_flags_with_composed_strings() {
	assert voidptr(C.FNAME0) == voidptr(C.printf)
	assert voidptr(C.FNAME1) == voidptr(C.printf)
	assert voidptr(C.FNAME2) == voidptr(C.printf)
	assert voidptr(C.FNAME3) == voidptr(C.printf)
	assert voidptr(C.FNAME4) == voidptr(C.printf)
	assert voidptr(C.FNAME5) == voidptr(C.printf)
}

#flag -DCMIXED1=$d('A1','mixed')_$d('A2',1)
#flag -DCMIXED2=$d('A1', 'mixed')_$d('A2',2 )
#flag -DCMIXED3=$d('A1','mixed' )_$d('A2', 3)
#flag -DCMIXED4=$d('A1', 'mixed' )_$d('A2', 4 )
#flag -DCMIXED55=$d('A1', 'mixed' )_$d('A2',55) ##

@[export: 'mixed_1']
pub fn f1() {}

@[export: 'mixed_2']
pub fn f2() {}

@[export: 'mixed_3']
pub fn f3() {}

@[export: 'mixed_4']
pub fn f4() {}

@[export: 'mixed_55']
pub fn f55() {}

fn test_custom_flags_that_are_a_mix() {
	assert voidptr(C.CMIXED1) == voidptr(C.mixed_1)
	assert voidptr(C.CMIXED2) == voidptr(C.mixed_2)
	assert voidptr(C.CMIXED3) == voidptr(C.mixed_3)
	assert voidptr(C.CMIXED4) == voidptr(C.mixed_4)
	assert voidptr(C.CMIXED55) == voidptr(C.mixed_55)
}
