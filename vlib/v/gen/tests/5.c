// loop label loop_cgen
int i = 0;
while (1) {
    i++
    while(1) {
        label:
        if (i == 1){
            goto outer_loop_cgen;
        }
        if (i == 2){
            goto loop_cgen;
        }
        if (i == 3){
            break
        }
        if (i == 4){
            continue
        }
        if (i == 5){
            goto label;
        }
        if (i == 6){
            goto goto_label;
        }
    }
    loop_cgen:
    ;
    if (i == 7) {
        goto goto_label
    }
    goto_label:
    if (i == 8) {
        break
    }
    if (i == 9) {
        continue
    }
    if (i == 10) {
        goto goto_label
    }
}
outer_loop_cgen:
;
