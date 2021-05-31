module orm

const (
	num64 = [8, 12]
	nums = [5, 6, 7, 9, 10, 11, 16]
	float = [13, 14]
	string = 18
)

pub enum OperationKind {
	neq // !=
	eq  // ==
	gt  // >
	lt  // <
	ge  // >=
	le  // <=
}

fn (kind OperationKind) to_str() string {
	str := match kind {
		.neq { '!=' }
		.eq { '==' }
		.gt { '>' }
		.lt { '<' }
		.ge { '>=' }
		.le { '<=' }
	}
	return str
}

pub enum StmtKind {
	insert
	update
	delete
}

pub struct OrmQueryData {
pub:
	fields []string
	data []voidptr
	types []int
	kinds []OperationKind
}

pub interface OrmConnection {
	//@select(table string, data OrmQueryData, where OrmQueryData) ?[][]string
	insert(table string, data OrmQueryData) ?int
	update(table string, data OrmQueryData, where OrmQueryData) ?int
	delete(table string, data OrmQueryData, where OrmQueryData) ?int
}

pub fn orm_stmt_gen(table string, para string, kind StmtKind, nums bool, qm string, data OrmQueryData, where OrmQueryData) string {
	mut str := ''

	mut c := 0

	match kind {
		.insert {
			str += 'INSERT INTO $para$table$para ('
			for i, field in data.fields {
				str += '$para$field$para'
				if i < data.fields.len -1 {
					str += ', '
				}
			}
			str += ') VALUES ('
			for i, _ in data.fields {
				str += qm
				if nums {
					str += '$c'
					c++
				}
				if i < data.fields.len - 1 {
					str += ', '
				}
			}
			str += ')'
		}
		.update {
			str += 'UPDATE $para$table$para SET '
			for i, field in data.fields {
				str += '$para$field$para = $qm'
				if nums {
					str += '$c'
					c++
				}
				if i < data.fields.len - 1 {
					str += ', '
				}
			}
			str += ' WHERE '
		}
		.delete {
			str += 'DELETE FROM $para$table$para WHERE '
		}
	}
	if kind == .update || kind == .delete {
		for i, field in where.fields {
			str += '$para$field$para ${where.kinds[i].to_str()} $qm'
			if nums {
				str += '$c'
				c++
			}
			if i < where.fields.len - 1 {
				str += ' AND '
			}
		}
	}
	str += ';'
	return str
}