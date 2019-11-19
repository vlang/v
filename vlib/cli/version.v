module cli

fn version_flag() Flag {
	return Flag{
		flag: .bool,
		global: true,
		name: 'version',
		abbrev: 'v',
		description: 'Prints version information',
	}
}

fn version_cmd() Command {
	return Command{
		name: 'version'
		description: 'Prints version information',
		execute: version_func,
	}
}

fn version_func(cmd cli.Command) {
	root := cmd.root()

	version := '${root.name} v${root.version}'
	println(version)
}
