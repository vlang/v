// vtest build: !windows && !musl?
module pg

fn test_orm_rollback_of_a_finished_tx_is_a_no_op() {
	mut tx := Tx{
		done: true
	}
	tx.orm_rollback()!
	// a manual rollback still reports the misuse
	if _ := tx.rollback() {
		assert false, 'rollback of a finished Tx should fail'
	} else {
		assert err.msg() == 'pg: transaction is already finished'
	}
}
