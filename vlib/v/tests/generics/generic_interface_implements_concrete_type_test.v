// Regression test for #24060.
// Explicit `implements Interface[Concrete]` should not force generic inference.

struct System[T] {
mut:
	data T
}

interface Transaction[T] {
mut:
	run(mut system System[T])
}

struct Data {
mut:
	dummy string
}

struct CreateDocumentTransaction implements Transaction[Data] {}

fn (mut txn CreateDocumentTransaction) run(mut system System[Data]) {
	system.data.dummy = 'updated'
}

fn apply_transaction(mut system System[Data], mut transaction Transaction[Data]) {
	transaction.run(mut system)
}

fn test_generic_interface_implemented_with_concrete_type() {
	mut system := System[Data]{
		data: Data{
			dummy: 'initial'
		}
	}
	mut transaction := CreateDocumentTransaction{}
	apply_transaction(mut system, mut transaction)
	assert system.data.dummy == 'updated'
}
