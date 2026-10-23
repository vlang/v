struct ConstAggregateEmpty {}

struct ConstAggregatePayload {
	value int
}

type ConstAggregate = ConstAggregateEmpty | ConstAggregatePayload | int

type ConstAggregateAlias = ConstAggregate

const const_aggregate_empty = ConstAggregate(ConstAggregateEmpty{})
const const_aggregate_payload = ConstAggregate(ConstAggregatePayload{42})
const const_aggregate_alias = ConstAggregateAlias(ConstAggregatePayload{17})

fn test_sumtype_aggregate_constants() {
	assert const_aggregate_empty is ConstAggregateEmpty
	assert const_aggregate_payload is ConstAggregatePayload
	assert (const_aggregate_payload as ConstAggregatePayload).value == 42
	assert (const_aggregate_alias as ConstAggregatePayload).value == 17
}
