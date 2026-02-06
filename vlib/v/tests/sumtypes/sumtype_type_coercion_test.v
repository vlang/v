struct Empty {}

struct Node[T] {
	value T
	next  Chain[T]
}

type Chain[T] = Empty | Node[T]

fn get[T](chain Chain[T]) T {
	return match chain {
		Empty { 0 }
		Node[T] { chain.value }
	}
}

fn test_main() {
	chain := Node{0.2, Empty{}}
	assert get(chain) == 0.2
}
