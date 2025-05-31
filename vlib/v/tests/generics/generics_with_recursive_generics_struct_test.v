pub struct Node[T] {
	value     T
	points_to []&Node[T]
}

fn test_generics_with_recursive_generics_struct() {
	mid := &Node[string]{
		value: 'Middle'
	}
	finish := &Node[string]{
		value: 'Finish'
	}

	graph := &Node[string]{
		value:     'Start'
		points_to: [
			&Node[string]{
				value:     'TopLeft'
				points_to: [
					finish,
					mid,
				]
			},
		]
	}

	println(graph.points_to[0].value)
	assert graph.points_to[0].value == 'TopLeft'
}
