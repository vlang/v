// sub module
module priv_sym

type Foo = int
pub type PubFoo = int

type BarFn = fn () int

pub type PubBarFn = fn () int

fn priv() {
}
