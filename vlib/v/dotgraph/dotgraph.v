module dotgraph

import strings

[heap]
pub struct DotGraph {
mut:
	sb strings.Builder
}

pub fn new(name string, label string, color string) &DotGraph {
	mut res := &DotGraph{
		sb: strings.new_builder(1024)
	}
	res.writeln('  subgraph cluster_${name} {')
	res.writeln('\tedge [fontname="Helvetica",fontsize="10",labelfontname="Helvetica",labelfontsize="10",style="solid",color="black"];')
	res.writeln('\tnode [fontname="Helvetica",fontsize="10",style="filled",fontcolor="black",fillcolor="white",color="black",shape="box"];')
	res.writeln('\trankdir="LR";')
	res.writeln('\tcolor="${color}";')
	res.writeln('\tlabel="${label}";')
	// Node14 [shape="box",label="PrivateBase",URL="$classPrivateBase.html"];
	// Node15 -> Node9 [dir=back,color="midnightblue",fontsize=10,style="solid"];
	return res
}

pub fn (mut d DotGraph) writeln(line string) {
	d.sb.writeln(line)
}

pub fn (mut d DotGraph) finish() {
	d.sb.writeln('  }')
	println(d.sb.str())
}

//

pub struct NewNodeConfig {
	node_name        string
	should_highlight bool
	tooltip          string
	ctx              voidptr = unsafe { nil }
	name2node_fn     FnLabel2NodeName = node_name
}

pub fn (mut d DotGraph) new_node(nlabel string, cfg NewNodeConfig) {
	mut nname := cfg.name2node_fn(nlabel, cfg.ctx)
	if cfg.node_name != '' {
		nname = cfg.node_name
	}
	if cfg.should_highlight {
		d.writeln('\t${nname} [label="${nlabel}",color="blue",height=0.2,width=0.4,fillcolor="#00FF00",tooltip="${cfg.tooltip}",shape=oval];')
	} else {
		d.writeln('\t${nname} [shape="box",label="${nlabel}"];')
	}
}

//

pub struct NewEdgeConfig {
	should_highlight bool
	ctx              voidptr = unsafe { nil }
	name2node_fn     FnLabel2NodeName = node_name
}

pub fn (mut d DotGraph) new_edge(source string, target string, cfg NewEdgeConfig) {
	nsource := cfg.name2node_fn(source, cfg.ctx)
	ntarget := cfg.name2node_fn(target, cfg.ctx)
	if cfg.should_highlight {
		d.writeln('\t${nsource} -> ${ntarget} [color="blue"];')
	} else {
		d.writeln('\t${nsource} -> ${ntarget};')
	}
}

//

pub type FnLabel2NodeName = fn (string, voidptr) string

pub fn node_name(name string, context voidptr) string {
	return name.replace('.', '_')
}
