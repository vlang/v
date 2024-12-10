interface WalkerTalker {
	// Abc
	Walker // adsas
	// zxczxc
	Talker // xyzdef
	// asdasdas
	nspeeches int
}

interface Talker {
	nspeeches int
mut:
	talk(msg string)
}

interface Walker {
	nsteps int
mut:
	walk(newx int, newy int)
}

struct Abc {
mut:
	x         int
	y         int
	phrases   []string
	nsteps    int = 1000
	nspeeches int = 1000
}

fn (mut s Abc) talk(msg string) {
	s.phrases << msg
	s.nspeeches++
}

fn (mut s Abc) walk(x int, y int) {
	s.x = x
	s.y = y
	s.nsteps++
}

fn test_walker_talker() {
	mut wt := WalkerTalker(Abc{
		x:       1
		y:       1
		phrases: ['hi']
	})
	wt.talk('my name is Wally')
	wt.walk(100, 100)
	if mut wt is Abc {
		dump(wt)
		assert wt.x == 100
		assert wt.y == 100
		assert wt.phrases.last().ends_with('Wally')
		assert wt.nspeeches == 1001
		assert wt.nsteps == 1001
	}
}
