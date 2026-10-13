module build

fn test_task_keeps_repeatable_flag() {
	mut runs := &[]string{}
	mut ctx := context()
	ctx.task(
		name:    'a'
		depends: ['dep']
		run:     fn [mut runs] (t Task) ! {
			runs << 'a'
		}
	)
	ctx.task(
		name:    'b'
		depends: ['dep']
		run:     fn [mut runs] (t Task) ! {
			runs << 'b'
		}
	)
	ctx.task(
		name:       'dep'
		repeatable: true
		run:        fn [mut runs] (t Task) ! {
			runs << 'dep'
		}
	)
	// the flag must survive task()
	if t := ctx.get_task('dep') {
		assert t.repeatable == true
	} else {
		assert false, 'dep task missing'
	}
	runs.clear()
	ctx.exec('a')
	ctx.exec('b')
	assert runs.join(',') == 'dep,a,dep,b'
}
