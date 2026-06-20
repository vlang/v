module transform

import v3.flat

fn (mut t Transformer) transform_live_fn(id flat.NodeId, _node flat.Node) flat.NodeId {
	return id
}
