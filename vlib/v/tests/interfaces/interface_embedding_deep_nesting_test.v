interface I99 {
	I98
}

interface I1 {
	I0
}

interface I2 {
	I1
}

interface I3 {
	I2
}

interface I4 {
	I3
}

interface I5 {
	I4
}

interface I6 {
	I5
}

interface I7 {
	I6
}

interface I8 {
	I7
}

interface I9 {
	I8
}

interface I10 {
	I9
}

interface I11 {
	I10
}

interface I12 {
	I11
}

interface I13 {
	I12
}

interface I14 {
	I13
}

interface I15 {
	I14
}

interface I16 {
	I15
}

interface I17 {
	I16
}

interface I18 {
	I17
}

interface I19 {
	I18
}

interface I20 {
	I19
}

interface I21 {
	I20
}

interface I22 {
	I21
}

interface I23 {
	I22
}

interface I24 {
	I23
}

interface I25 {
	I24
}

interface I26 {
	I25
}

interface I27 {
	I26
}

interface I28 {
	I27
}

interface I29 {
	I28
}

interface I30 {
	I29
}

interface I31 {
	I30
}

interface I32 {
	I31
}

interface I33 {
	I32
}

interface I34 {
	I33
}

interface I35 {
	I34
}

interface I36 {
	I35
}

interface I37 {
	I36
}

interface I38 {
	I37
}

interface I39 {
	I38
}

interface I40 {
	I39
}

interface I41 {
	I40
}

interface I42 {
	I41
}

interface I43 {
	I42
}

interface I44 {
	I43
}

interface I45 {
	I44
}

interface I46 {
	I45
}

interface I47 {
	I46
}

interface I48 {
	I47
}

interface I49 {
	I48
}

interface I50 {
	I49
}

interface I51 {
	I50
}

interface I52 {
	I51
}

interface I53 {
	I52
}

interface I54 {
	I53
}

interface I55 {
	I54
}

interface I56 {
	I55
}

interface I57 {
	I56
}

interface I58 {
	I57
}

interface I59 {
	I58
}

interface I60 {
	I59
}

interface I61 {
	I60
}

interface I62 {
	I61
}

interface I63 {
	I62
}

interface I64 {
	I63
}

interface I65 {
	I64
}

interface I66 {
	I65
}

interface I67 {
	I66
}

interface I68 {
	I67
}

interface I69 {
	I68
}

interface I70 {
	I69
}

interface I71 {
	I70
}

interface I72 {
	I71
}

interface I73 {
	I72
}

interface I74 {
	I73
}

interface I75 {
	I74
}

interface I76 {
	I75
}

interface I77 {
	I76
}

interface I78 {
	I77
}

interface I79 {
	I78
}

interface I80 {
	I79
}

interface I81 {
	I80
}

interface I82 {
	I81
}

interface I83 {
	I82
}

interface I84 {
	I83
}

interface I85 {
	I84
}

interface I86 {
	I85
}

interface I87 {
	I86
}

interface I88 {
	I87
}

interface I89 {
	I88
}

interface I90 {
	I89
}

interface I91 {
	I90
}

interface I92 {
	I91
}

interface I93 {
	I92
}

interface I94 {
	I93
}

interface I95 {
	I94
}

interface I96 {
	I95
}

interface I97 {
	I96
}

interface I98 {
	I97
}

interface I0 {
	m999() int
}

struct Abc {
	x int = 123
}

fn (s Abc) m999() int {
	return 999
}

fn test_deep_nested_interface_embeddings() {
	a := Abc{}
	dump(a)
	i := I99(a)
	dump(i)
	assert i.m999() == 999
}
