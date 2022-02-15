module edwards25519

fn test_proj_lookup_table() {
	mut table := ProjLookupTable{}
	b := new_generator_point()
	table.from_p3(b)

	mut tmp1 := ProjectiveCached{}
	mut tmp2 := ProjectiveCached{}
	mut tmp3 := ProjectiveCached{}

	table.select_into(mut tmp1, 6)
	table.select_into(mut tmp2, -2)
	table.select_into(mut tmp3, -4)

	// Expect T1 + T2 + T3 = identity
	mut acc_p1 := ProjectiveP1{}
	mut acc_p3 := new_identity_point()

	acc_p1.add(acc_p3, tmp1)
	acc_p3.from_p1(acc_p1)
	acc_p1.add(acc_p3, tmp2)
	acc_p3.from_p1(acc_p1)
	acc_p1.add(acc_p3, tmp3)
	acc_p3.from_p1(acc_p1)

	assert acc_p3.equal(id_point) == 1
}

fn test_affine_lookup_table() {
	mut table := AffineLookupTable{}
	b := new_generator_point()
	table.from_p3(b)

	mut tmp1 := AffineCached{}
	mut tmp2 := AffineCached{}
	mut tmp3 := AffineCached{}

	table.select_into(mut tmp1, 3)
	table.select_into(mut tmp2, -7)
	table.select_into(mut tmp3, 4)
	// Expect T1 + T2 + T3 = identity

	mut acc_p1 := ProjectiveP1{}
	mut acc_p3 := new_identity_point()

	acc_p1.add_affine(acc_p3, tmp1)
	acc_p3.from_p1(acc_p1)
	acc_p1.add_affine(acc_p3, tmp2)
	acc_p3.from_p1(acc_p1)
	acc_p1.add_affine(acc_p3, tmp3)
	acc_p3.from_p1(acc_p1)

	assert acc_p3.equal(id_point) == 1
}

fn test_naf_lookup_table5() {
	mut table := NafLookupTable5{}
	b := new_generator_point()
	table.from_p3(b)

	mut tmp1 := ProjectiveCached{}
	mut tmp2 := ProjectiveCached{}
	mut tmp3 := ProjectiveCached{}
	mut tmp4 := ProjectiveCached{}

	table.select_into(mut tmp1, 9)
	table.select_into(mut tmp2, 11)
	table.select_into(mut tmp3, 7)
	table.select_into(mut tmp4, 13)
	// Expect T1 + T2 = T3 + T4

	mut acc_p1 := ProjectiveP1{}
	mut lhs := new_identity_point()
	mut rhs := new_identity_point()

	acc_p1.add(lhs, tmp1)
	lhs.from_p1(acc_p1)
	acc_p1.add(lhs, tmp2)
	lhs.from_p1(acc_p1)

	acc_p1.add(rhs, tmp3)
	rhs.from_p1(acc_p1)
	acc_p1.add(rhs, tmp4)
	rhs.from_p1(acc_p1)

	assert lhs.equal(rhs) == 1
}

fn test_naf_lookup_table8() {
	mut table := NafLookupTable8{}
	b := new_generator_point()
	table.from_p3(b)

	mut tmp1 := AffineCached{}
	mut tmp2 := AffineCached{}
	mut tmp3 := AffineCached{}
	mut tmp4 := AffineCached{}

	table.select_into(mut tmp1, 49)
	table.select_into(mut tmp2, 11)
	table.select_into(mut tmp3, 35)
	table.select_into(mut tmp4, 25)
	// Expect T1 + T2 = T3 + T4

	mut acc_p1 := ProjectiveP1{}
	mut lhs := new_identity_point()
	mut rhs := new_identity_point()

	acc_p1.add_affine(lhs, tmp1)
	lhs.from_p1(acc_p1)
	acc_p1.add_affine(lhs, tmp2)
	lhs.from_p1(acc_p1)

	acc_p1.add_affine(rhs, tmp3)
	rhs.from_p1(acc_p1)
	acc_p1.add_affine(rhs, tmp4)
	rhs.from_p1(acc_p1)

	assert lhs.equal(rhs) == 1
}
