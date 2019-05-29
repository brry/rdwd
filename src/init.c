#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* Kept as generated with tools::package_native_routine_registration_skeleton(".", "src/init.c")*/

/* .Fortran calls */
extern void F77_NAME(binary_to_num)(void *, void *, void *, void *, void *);
extern void F77_NAME(binary_to_num_rx)(void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"binary_to_num",    (DL_FUNC) &F77_NAME(binary_to_num),    5},
    {"binary_to_num_rx", (DL_FUNC) &F77_NAME(binary_to_num_rx), 5},
    {NULL, NULL, 0}
};

void R_init_rdwd(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
