// sub module
module priv_sym

type PrivFoo = int
pub type PubFoo = int

type PrivBarFn = fn () int

pub type PubBarFn = fn () int

fn priv() {
}
