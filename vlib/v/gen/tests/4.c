struct Bar {
	int a;
};

struct Foo {
	string a;
	Bar b;
};

// multi return structs
typedef struct {
	int arg0;
	string arg1;
} multi_return_int_string;

// end of definitions #endif
typedef Option Option_string;
typedef Option Option_multi_return_int_string;
multi_return_int_string mr_test();
int testa();
string testb(int a);
int testc(int a);
int Foo_testa(Foo* f);
int Foo_testb(Foo* f);
int Bar_testa(Bar* b);
Option_string optional_a();
Option_string optional_b();
Option_multi_return_int_string optional_mr();
// >> typeof() support for sum types
// << typeof() support for sum types

int main(int argc, char** argv) {
	_vinit();
    Bar b = (Bar){
        .a = 122,
    };
    Foo a = (Foo){
        .a = tos3("hello"),
        .b = b,
    };
    a.a = tos3("da");
    a.b.a = 111;
    string a1 = a.a;
    int a2 = Bar_testa(&b);
    int c = testa();
    c = 1;
    string d = testb(1);
    d = tos3("hello");
    string e = tos3("hello");
    e = testb(111);
	e = tos3("world");
	array_int f = new_array_from_c_array(4, 4, sizeof(int), (int[4]){
        testa(), 2, 3, 4,
	});
	array_string g = new_array_from_c_array(2, 2, sizeof(string), (string[2]){
		testb(1), tos3("hello"),
	});
	array_Foo arr_foo = new_array_from_c_array(1, 1, sizeof(Foo), (Foo[1]){
		a,
	});
	Foo af_idx_el = (*(Foo*)array_get(arr_foo, 0));
	string foo_a = af_idx_el.a;
    map_string_string m1 = new_map(sizeof(string));
    map_string_int m2 = new_map_init(2, sizeof(int), (string[2]){tos3("v"), tos3("lang"), }, (int[2]){1, 2, });
    string ma1 = tos3("hello");
    string ma2 = tos3("vlang");
	multi_return_int_string mr_566 = mr_test();
	int mr1 = mr_566.arg0;
	string mr2 = mr_566.arg1;
	string opt1 = tos3("opt1");
	Option_string opt2 = optional_a();
	if (!opt2.ok) {
        string err = opt2.v_error;
        int errcode = opt2.ecode;
	};
	string opt3 = tos3("opt3");
	Option_string opt4 = optional_b();
	if (!opt4.ok) {
        string err = opt4.v_error;
        int errcode = opt4.ecode;
	};
	Option_multi_return_int_string mr_669 = optional_mr();
	if (!mr_669.ok) {
        string err = mr_669.v_error;
        int errcode = mr_669.ecode;
	};
	int opt_mr1 = (*(multi_return_int_string*)mr_669.data).arg0;
	string opt_mr12 = (*(multi_return_int_string*)mr_669.data).arg1;
    return 0;
}

multi_return_int_string mr_test() {
    return (multi_return_int_string){.arg0=1,.arg1=tos3("v")};
}

int testa() {
    return testc(1);
}

string testb(int a) {
    return tos3("hello");
}

int testc(int a) {
    return a;
}

int Foo_testa(Foo* f) {
    int a = Foo_testb(f);
    a = 1;
    return 4;
}

int Foo_testb(Foo* f) {
    return 4;
}

int Bar_testa(Bar* b) {
    return 4;
}

Option_string optional_a() { 
	return opt_ok(& (string []) { tos3("111") }, sizeof(string));
}

Option_string optional_b() { 
	return opt_ok(& (string []) { tos3("222") }, sizeof(string));
}

Option_multi_return_int_string optional_mr() { 
	return opt_ok(& (multi_return_int_string []) { (multi_return_int_string){.arg0=1,.arg1=tos3("111")} }, sizeof(multi_return_int_string));
}

void _vinit() {

}
