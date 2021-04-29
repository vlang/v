interface WalkerTalker {
	// Abc
	Walker // adsas
	// zxczxc
	Talker // xyzdef
	// asdasdas
}

interface Talker {
	talk(msg string)
}

interface Walker {
	walk(newx int, newy int)
}

struct Abc {
mut:
	x       int
	y       int
	phrases []string
}

fn (mut s Abc) talk(msg string) {
	s.phrases << msg
}

fn (mut s Abc) walk(x int, y int) {
	s.x = x
	s.y = y
}

fn test_walker_talker() {
	mut wt := WalkerTalker(Abc{1, 1, ['hi']})
	wt.talk('my name is Wally')
	wt.walk(100, 100)
	if mut wt is Abc {
		dump(wt)
		assert wt.x == 100
		assert wt.y == 100
		assert wt.phrases.last().ends_with('Wally')
	}
}
