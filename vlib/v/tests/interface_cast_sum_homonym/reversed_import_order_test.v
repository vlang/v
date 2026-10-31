import sumpkt
import ifacepkt

fn test_interface_cast_with_sum_homonym_imported_first() {
	assert ifacepkt.from_closure().id() == 1
	assert ifacepkt.from_value(9).id() == 9
	assert ifacepkt.from_local() == 1
	s := sumpkt.report(4)
	assert s is sumpkt.Report
	if s is sumpkt.Report {
		assert s.count == 4
	}
}
