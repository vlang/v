fn test_raw_string_literal_method_call() {
	value := r'$2a$07$MRniCPEgEQnrJmmgN.maM'.bytes()
	assert value.bytestr() == r'$2a$07$MRniCPEgEQnrJmmgN.maM'
}
