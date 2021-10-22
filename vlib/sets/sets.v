import math.big

type Idx = int | u32 | big.Integer

interface StoreI{
	has( e Idx ) bool
	get( e Idx ) bool
	len() Idx
	mut:
		set(e Idx) bool
}

struct BitSet<I> {
	mut:
		data StoreI
}
fn (		bs BitSet<I>)      len() 			 Idx 		{ return bs.data.len() }
//fn (		bs BitSet<I>) 	has( e Idx ) bool 	{ return bs.data.has( e ) }
fn (		bs BitSet<I>)   get<I>( e I ) bool 	{ return bs.data.get( e ) }
fn (		bs BitSet<I>) clear<I>( e I ) bool 	{ return true }
fn (mut bs BitSet<I>)   set<I>( e I ) bool 	{
	println( 'from ' + @STRUCT + '.' + @FN )
	if bs.data.has( e ) { return false }
	else {
		bs.data.set( e )
		return true
	}
}

struct Store<I> {
mut:
	data  []I
}
pub fn (		s Store) len<I>() 			 Idx 	{ return Idx(s.data.len) }
pub fn (		s Store) has<I>( e Idx ) bool {
	println( '\tfrom ' + @STRUCT + '.' + @FN + ' ret ${ e in s.data }' )
	return e in s.data
}
pub fn (		s Store) get<I>( e Idx ) bool {
	$if I is int {
		println( @STRUCT + '.get I is Int')
		return e in s.data
	} $else $if I is u32 {
			println( @STRUCT + '.get I is u32')
			return int(e) in s.data
	} $else $if I is big.Integer {
		idx := e.int()
		println( @STRUCT + '.get I is big.Intger $idx')
		return idx in s.data
	} $else $if I is Idx {
		println( @STRUCT + '.get I is Idx, e is $e returning ${e in s.data}')
		return e in s.data
	} $else {
		eprintln(@STRUCT + '.get unknown I, e is $e')
		panic('cannot handle typeof I')
	}
}

pub fn (mut s Store) set<I>( e Idx ) bool {
	if s.get<I>( e ) { return false }
	println( 'in ' + @STRUCT + '.' + @FN + ' storing $e' )
	$if I is int {
		println( @STRUCT + '.set I is Int')
		s.data << int(e)
		return true
	} $else $if I is u32 {
			println( @STRUCT + '.set I is u32')
			s.data << e
			return true
	} $else $if I is big.Integer {
		idx := e.int()
		println( @STRUCT + '.set I is big.Intger $idx')
		s.data << idx
		return true
	} $else $if I is Idx {
		println( @STRUCT + '.set I is Idx, e is $e returning ${e in s.data}')
		s.data << e
	} $else {
		eprintln(@STRUCT + '.set unknown I, e is $e')
		//panic('cannot handle typeof I')
		return true
	}
	//panic('unhandled type, e was $e')
	return true
}

fn main() {
 mut u := BitSet<Idx>{ data: Store<Idx>{ [ Idx(1), Idx(2), Idx(3)] } }
 println(u)
 println( u.get( Idx(1) ))
 println( 'setting 4 ${u.set( Idx(4) )}' )
 println( u.len() )
 println( 'setting 4 again ${u.set( Idx(4) )}' )
 println(u)

mut v := BitSet<Idx>{ data: Store<int>{ [ 1, 2, 3] } }
println( v )
}

