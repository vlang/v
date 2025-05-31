struct Context {
}

struct Input {
}

struct Block {
	x int
	y int
mut:
	children []Element
}

type Element = Block | Input

fn (input Input) draw(mut ctx Context, x int, y int) {}

fn (block Block) draw(mut ctx Context, x int, y int) {
	for child in block.children {
		child.draw(mut ctx, block.x + x, block.y + y)
	}
}

fn (elt Element) draw(mut ctx Context, x int, y int) {
	$for T in Element.variants {
		if elt is T {
			elt.draw(mut ctx, x, y)
			return
		}
	}
}

fn test_main() {
	assert true
}
