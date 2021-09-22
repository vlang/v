# WIKIs

@for site in sites
@if site.cat == .wiki

## @{site.domains[0]}@port_str

- [errors]("//@{site.domains[0]}@port_str/errors")

### domains

@for dom in site.domains
- @dom
@end
@end
@end
