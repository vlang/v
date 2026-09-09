module transform

import v3.flat

// transform_live_fn transforms transform live fn data for transform.
fn (mut t Transformer) transform_live_fn(id flat.NodeId, _node flat.Node) flat.NodeId {
	return id
}
