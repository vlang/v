module dns

// system_config reads the servers and options of /etc/resolv.conf.
fn system_config() !ResolvConf {
	return parse_resolv_conf('/etc/resolv.conf')
}
