struct Empty {}

struct Node[T] {
	value T
	next  &Chain[T]
}

type Chain[T] = Empty | Node[T]

fn get[T](chain Chain[T]) T {
	return match chain {
		Empty { 0 }
		Node[T] { chain.value }
	}
}

fn test_main() {
	end := Chain[f64](Empty{})
	chain := Node{0.2, &end}
	assert get(chain) == 0.2
}
