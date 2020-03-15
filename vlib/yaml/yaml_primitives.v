module yaml

import (
	time
)

const (
	NULL = 0
)
struct Data{
	squences 	&[]Squence
	maps		&[]Mapping
}

struct YAMLVal{
	mut:
	integer YAMLInt
	str string
	boolean bool
	squence Squence
}

struct YAMLInt{

}

struct Squence{
	val YAMLVal
}

struct Mapping{
	val YAMLVal
}

struct TimeStamp{
	
}