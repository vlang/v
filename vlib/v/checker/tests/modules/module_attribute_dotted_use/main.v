import mod

@[mod.Route('/a', 'b')]
fn bare_args() {
}

@[mod.Route('/a')]
fn missing_arg() {
}

@[mod.Route('/a', path: '/b')]
fn duplicate_arg() {
}

fn main() {
}