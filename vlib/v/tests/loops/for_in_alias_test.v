enum Nucleotide {
	a
	c
	g
	t
}

type Codon = []Nucleotide
type Gene = []Codon

fn test_for_in_alias() {
	mut gene := Gene([
		Codon([Nucleotide.a, Nucleotide.c, Nucleotide.g]),
		Codon([Nucleotide.g, Nucleotide.a, Nucleotide.t]),
	])

	mut ret := []string{}
	for cdn in gene {
		println(cdn)
		ret << '${cdn}'
	}

	assert ret.len == 2
	assert ret[0] == 'Codon([a, c, g])'
	assert ret[1] == 'Codon([g, a, t])'
}
