#include <stdint.h>

#define PARAM_N 1024
#define PARAM_Q 12289

extern double omegas_double[];
extern double omegas_uint16[];

void bitrev_vector(int32_t* poly);
void ntt_double(int32_t *, const double *, const double *);
void ntt(int32_t *coeffs);

void bitrev_vector_ref(uint16_t* poly);
void ntt_ref(uint16_t *poly);
