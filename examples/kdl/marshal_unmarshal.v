// KDL: marshal V structs to KDL and unmarshal KDL back into V structs
import kdl

struct Package {
	name    string
	version string
}

struct Dependencies {
	packages []string @[kdl: 'args']
}

struct Build {
	command string @[kdl: 'arg']
	target  string @[kdl: 'arg']
}

struct Config {
	app    Package      @[kdl: 'app,child']
	deps   Dependencies @[kdl: 'deps,child']
	build  Build        @[kdl: 'build,child']
	active bool
}

fn main() {
	// Encode a struct tree to KDL
	cfg := Config{
		app:    Package{
			name:    'my-app'
			version: '1.2.3'
		}
		deps:   Dependencies{
			packages: ['libA', 'libB']
		}
		build:  Build{
			command: 'v'
			target:  '.'
		}
		active: true
	}
	println('--- encode ---')
	print(kdl.encode(cfg))

	// Decode KDL back into a struct
	println('\n--- decode ---')
	parsed := kdl.decode[Config](kdl.encode(cfg))!
	println('app: ${parsed.app.name} v${parsed.app.version}')
	println('deps: ${parsed.deps.packages}')
	println('build: ${parsed.build.command} ${parsed.build.target}')
	println('active: ${parsed.active}')

	// Using rename strategies
	println('\n--- snake_case rename ---')
	opts := kdl.EncodeOpts{
		rename_all: 'snake_case'
	}
	print(kdl.encode_opts(cfg, opts))

	println('\n--- kebab-case rename ---')
	opts2 := kdl.EncodeOpts{
		rename_all: 'kebab-case'
	}
	print(kdl.encode_opts(cfg, opts2))
}
