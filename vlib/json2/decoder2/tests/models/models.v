module models

// Animal represents a test sum type declared outside the calling module.
pub type Animal = Cat | Dog

// Cat represents the cat variant of Animal.
pub struct Cat {
pub:
	cat_name string
}

// Dog represents the dog variant of Animal.
pub struct Dog {
pub:
	dog_name string
}
