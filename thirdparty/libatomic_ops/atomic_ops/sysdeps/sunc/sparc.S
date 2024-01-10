        .seg    "text"
        .globl  AO_test_and_set_full
AO_test_and_set_full:
        retl
          ldstub        [%o0],%o0
