#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

// Insert Copyright here

// C-calling-Fortran

extern void F77_NAME(binary_to_num)(int *raw, int *Flength, int *ret,
                                    int *Fna, int *Fclutter);
                                    
extern void F77_NAME(binary_to_num_rx)(int *raw, int *Flength, int *ret,
                                       int *Fna, int *Fclutter);

// R-calling-C

extern SEXP B2N_C(SEXP raw, SEXP Flength, SEXP Fna, SEXP Fclutter) {
    SEXP ret;
    PROTECT(ret = allocVector(INTSXP, INTEGER(Flength)[0]));
    F77_CALL(binary_to_num)(INTEGER(raw), INTEGER(Flength), INTEGER(ret),
             INTEGER(Fna), INTEGER(Fclutter));
    UNPROTECT(1);
    return(ret);
}

extern SEXP B2NRX_C(SEXP raw, SEXP Flength, SEXP Fna, SEXP Fclutter) {
    SEXP ret;
    PROTECT(ret = allocVector(INTSXP, INTEGER(Flength)[0]));
    F77_CALL(binary_to_num_rx)(INTEGER(raw), INTEGER(Flength), INTEGER(ret),
             INTEGER(Fna), INTEGER(Fclutter));
    UNPROTECT(1);
    return(ret);
}

// Methods & Registration
static const R_CallMethodDef CallEntries[] = {
    {"B2N_C",   (DL_FUNC) &B2N_C,   4},
    {"B2NRX_C", (DL_FUNC) &B2N_C,   4},
    {NULL,      NULL,               0}
};

void R_init_rdwd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
