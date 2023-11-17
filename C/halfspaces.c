#define qh_QHimport
#include "qhull_ra.h"

double** intersections(
  double*   halfspaces,
  double*   ipoint,
  unsigned  dim,
  unsigned  n,
  unsigned* nintersections,
  unsigned* exitcode,
  unsigned  print
)
{
  char opts[250];
  snprintf(opts, sizeof(opts),  "qhull s Fp H H");
  for(unsigned i = 0; i < dim; i++) {
    int integerPart = ipoint[i];                      // Get integer part.
    double fracPart = fabs(ipoint[i] - integerPart);  // Get fractional part.
    int fractPart_e8 = trunc(fracPart * 1e8);         // Turn it into integer.
    double fracPart2 = fracPart - (double)fractPart_e8 / 1.0e8;
    int fractPart_e16 = trunc(fracPart2 * 1e16);
    snprintf(
      opts + strlen(opts), sizeof(opts) - strlen(opts), 
      "%s%d.%d%d", i == 0 ? "" : ",", integerPart, fractPart_e8, fractPart_e16
    );
  }
  // printf("%s", opts); printf("\n");

  qhT qh_qh; /* Qhull's data structure */
  qhT* qh= &qh_qh;
  QHULL_LIB_CHECK
  qh_meminit(qh, stderr);
  boolT ismalloc  = False; /* True if qhull should free points 
                              in qh_freeqhull() or reallocation */
  FILE *errfile = NULL;
  FILE* outfile = print ? stdout : NULL;
  qh_zero(qh, errfile);
  *exitcode = qh_new_qhull(
    qh, dim+1, n, halfspaces, ismalloc, opts, outfile, errfile
  );
  // printf("exitcode: %u\n", *exitcode);

  double** out;
  if(!(*exitcode)) {
    *nintersections = qh->num_facets;
    out = malloc(*nintersections * sizeof(double*));
    facetT *facet;
    unsigned i_facet = 0;
    FORALLfacets {
      if(facet->offset != 0) {
        out[i_facet] = malloc(dim * sizeof(double));
        for(unsigned i = 0; i < dim; i++){
          out[i_facet][i] = - facet->normal[i] / facet->offset +
                            qh->feasible_point[i]; // = interiorpoint ? yes
        }
        i_facet++;
      } else {
        (*nintersections)--;
      }
    }

  }

  /* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
  qh_freeqhull(qh, !qh_ALL);                /* free long memory */
  qh_memfreeshort(qh, &curlong, &totlong);  /* free short memory 
                                               and memory allocator */
  if(*exitcode) {
    free(out);
    return 0;
  } else {
    return out;
  }

}
