V3 remaining failure groups

These files split the remaining V3 failures into non-overlapping groups for
parallel fix sessions.

Groups:
- 01_stdlib_services.txt: crypto, db, net, veb, and x modules (71)
- 02_stdlib_other_and_tools.txt: cmd, examples, and other non-vlib/v modules (25)
- 03_v_compiler_infra.txt: vlib/v tests outside vlib/v/tests (11)
- 04_v_tests_generics.txt: vlib/v/tests generics and generic root tests (67)
- 05_v_tests_comptime_and_options.txt: comptime and option tests (47)
- 06_v_tests_fns_and_control_flow.txt: fns, conditions, loops, concurrency (13)
- 07_v_tests_collections_aliases_casts.txt: arrays, maps, strings, aliases, casts (0)
- 08_v_tests_types_interfaces_sumtypes.txt: interfaces, structs, pointers, unions,
  and sum types (10)
- 09_v_tests_misc_language_projects.txt: remaining vlib/v/tests language/project
  tests (53)

All ORM tests (vlib/orm, vlib/db *_orm_*, and vlib/v/tests/orm_*) are owned by a
separate session and are tracked in v3_test_skipped.txt, not here. Do not add ORM
tests back to these groups or to the master failure list.

The vlib/x/atomics tests (i32/i64/u32/u64) are tracked in v3_test_skipped.txt:
that module implements its API only via amd64/i386 inline assembly and has no
arm64 backend, so the tests do not compile on arm64 hosts with any V compiler
(V 0.5.2 fails identically). They are a platform gap in the module, not a v3
compiler failure.

When a session fixes a group, re-check that group and update both the master
pass/failure lists and the relevant group file.
