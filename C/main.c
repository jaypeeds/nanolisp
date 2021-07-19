//
//  main.c
//  NanoLisp
//
//  Created by Jean-Pierre De Soza on 15/07/2021.
//

#include <stdio.h>
#include "NanoLisp.h"

env_t GLOBAL_ENV;

int main(int argc, const char * argv[]) {
    Sexp m, s;
   
    DONE = false;
    TRACE = true;

    setup();

    DBG_TRACE(f_oblist());
    INDEX = MAX_STRING_LENGTH;
    printf(BANNER);
    do {
        m = f_load(S_CONSOLE);
        DBG_TRACE(f_print(m));
        ERROR = false;
        s = f_eval(m);
        DBG_TRACE(f_print(s));
        if (!(ERROR || DONE))
            f_print(s);
        printf("\n");
    } while (!(ERROR || DONE));
    return 0;
}
