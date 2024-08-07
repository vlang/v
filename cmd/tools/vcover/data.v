// Copyright (c) 2024 Felipe Pena and Delyan Angelov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

// vcounter_*.csv files contain counter lines in a CSV format. They can get quite large,
// for big programs, since they contain all non zero coverage counters.
// Their names are timestamp (rand.ulid + clock_gettime) based, to minimise the chance that parallel runs
// will overwrite each other, but without the overhead of additional synchronisation/locks.
struct CounterLine {
mut:
	file string // retrieved based on the loaded meta
	line int    // retrieved based on the loaded meta

	meta  string // A filename in the sibling meta/ folder, should exist, to match the value from this field. The filename is a hash of both the path and the used build options, to facilitate merging coverage data from different builds/programs
	point int    // The index of a source point. Note that it is not a line number, but an index in the meta data file, keyed by the field `meta` above.
	hits  u64    // How many times the coverage point was executed. Only counters that are != 0 are recorded.
}

// Source metadata files in meta/*.txt, contain JSON encoded fields (mappings from v source files to point line numbers).
// Their names are a result of a hashing function, applied over both the source file name, and the build options.
// This has several benefits:
//   a) it makes sure, that the resulting path is normalised
//   b) the meta data is deduplicated between runs that use the same source files
//   c) coverage data from different runs can be merged by simply reusing the same -coverage folder,
//      or by copy/pasting all files from 1 run, to the folder of another.
struct MetaData {
	file          string // V source file path
	fhash         string // fhash is the name of the meta file
	v_version     string // the V version, used to generate the coverage meta data file
	build_options string // the build options for the program
	npoints       int    // the number of stored coverage points
	points        []int  // the line numbers corresponding to each point
}
