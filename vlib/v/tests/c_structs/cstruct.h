
struct Abc {
   int field;
};

typedef struct Test2 {
    int a;
} Test2;

typedef struct Test1 {
    Test2 a;
} Test1;

/////

typedef struct Foo {
    int a;
} Foo;

typedef struct Bar {
    int a;
} Bar;
