
interface SetI{
	has( e int ) bool
	get( e int ) int
	len() int
	mut:
		set(e int) bool
}

struct BitSet {
	mut:
		data SetI
}
fn (bs BitSet) len() int { return bs.data.len() }
fn (bs BitSet) has( e int ) bool { return bs.data.has( e ) }
fn (bs BitSet) get( e int ) int { return bs.get( e ) }
fn (mut bs BitSet) set(e int) bool {
	if bs.data.has( e ) {return false }
	else {
		bs.data.set( e )
		return true
	}
}

struct Set<T> {
mut:
	data  []T
}
pub fn (se Set) len<T>() int { return se.data.len }
pub fn (se Set) has<T>( e T ) bool { return e in se.data }
pub fn (se Set) get<T>( e T ) T { return se.data[se.data.index(e)] }
pub fn (mut se Set) set<T>( e T ) bool {
	if e in se.data { return false }
	se.data << e
	return true
}

fn main() {
 mut u := BitSet{ data: Set<int>{[1,2,3]} }
 println(u)
 println( u.has(1) )
 println( u.set(4) )
 println( u.len() )

}

