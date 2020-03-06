struct User {
	int age;
	string name;
};

// multi return structs
// end of definitions
int main() {
	User user = (User){
};
	user.age = 10;
	user.age++;
	user.name = tos3("bob");
	return 0;
}
