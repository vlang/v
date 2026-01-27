
struct Abc {
   char *char_pointer_field;
};

// Simulates a struct like FT_Generic which is a struct, not a pointer
struct InnerStruct {
   int x;
   int y;
};

// Simulates a struct like FT_FaceRec which has InnerStruct fields
// that might be declared as voidptr in V bindings
struct OuterStruct {
   int id;
   struct InnerStruct inner;  // This is a struct value, not a pointer
};
