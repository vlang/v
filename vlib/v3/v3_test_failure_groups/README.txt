V3 remaining failure groups

These files split the remaining V3 failures into non-overlapping groups for
parallel fix sessions.

Groups:
- 01_stdlib_services.txt: crypto, db, net, orm, veb, and x modules (180)
- 02_stdlib_other_and_tools.txt: cmd, examples, and other non-vlib/v modules (86)
- 03_v_compiler_infra.txt: vlib/v tests outside vlib/v/tests (34)
- 04_v_tests_generics.txt: vlib/v/tests generics and generic root tests (88)
- 05_v_tests_comptime_and_options.txt: comptime and option tests (79)
- 06_v_tests_fns_and_control_flow.txt: fns, conditions, loops, concurrency (20)
- 07_v_tests_collections_aliases_casts.txt: arrays, maps, strings, aliases, casts (0)
- 08_v_tests_types_interfaces_sumtypes.txt: interfaces, structs, pointers, unions,
  and sum types (19)
- 09_v_tests_misc_language_projects.txt: remaining vlib/v/tests language/project
  tests (109)

When a session fixes a group, re-check that group and update both the master
pass/failure lists and the relevant group file.
