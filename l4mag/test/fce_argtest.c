#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <argtable2.h>

int main(int nArgs, char** args) {

    char * myname;

    struct arg_lit *opt_mag;
    struct arg_int *opt_int;
    struct arg_dbl *opt_scale;
    struct arg_end *opt_end;
    double scale;
    int nErrors;

    void *argTable[] = {
        opt_mag = arg_lit0("m", NULL, "output magnetic field"),
        opt_scale = arg_dbl0("s", NULL, NULL, "output scale factor"),
        opt_int = arg_int0("i", NULL, NULL, "Dummy int paremeter"),
        opt_end = arg_end(5),
    };

    opt_scale->dval = &scale;

    if (arg_nullcheck(argTable) != 0) {
        printf("Error\n");
    }

    myname = basename(args[0]);

    arg_print_syntax(stderr, argTable, "\n");
    arg_print_syntaxv(stderr, argTable, "\n");
    arg_print_glossary(stderr, argTable, " %-30s %s\n");

    printf("scale opt: %s\n", opt_scale->hdr.shortopts);

    nErrors = arg_parse(nArgs, args, argTable);

    if (nErrors == 0) {
        printf("mag: %d\n", opt_mag->count);
        if (opt_scale->count == 1) {
            printf("scale: %f\n", scale);
        }
        printf("opt_int: %d\n", opt_int->ival[0]);
    }
    else {
        arg_print_errors(stderr, opt_end, myname);
    }

    return 0;
}
