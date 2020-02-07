module yaml

import (
	time
)

const (
	NULL = 0
)
struct Yaml{
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

}

struct Mapping{

}

struct TimeStamp{
	
}