#include "ntt.h"
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <x86intrin.h>
#include "cpucycles.h"

#define N_samples 100

static int cmp_llu(const void *a, const void*b)
{
  if(*(double *)a < *(double *)b) return -1;
  if(*(double *)a > *(double *)b) return 1;
  return 0;
}

static double median(double *l, size_t llen)
{
  qsort(l,llen,sizeof(double),cmp_llu);

  if(llen%2) return l[llen/2];
  else return (l[llen/2-1]+l[llen/2])/2;
}

int main() {
  const int n_iters = 100000;
  double cycles[N_samples];

  for (int i = 0; i < N_samples; ++i) {
    {
      int32_t input[PARAM_N];
      unsigned long long start_cycle = cpucycles();
      for (int i = 0; i < n_iters; ++i) {
        ntt(input);
      }
      unsigned long long end_cycle = cpucycles();
      double elapsed_cycles = (end_cycle - start_cycle) / n_iters;
      cycles[i] = elapsed_cycles;
    }
  }
  printf("median of 100 runs: %f\n", median(cycles, N_samples));

  return 0;
}
