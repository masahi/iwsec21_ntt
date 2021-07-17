#include "ntt.h"
#include "cpucycles.h"
#include <stdio.h>
#include <time.h>

double get_time(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (ts.tv_sec) + (ts.tv_nsec / (1000.0 * 1000.0 * 1000.0));
}

int main() {

  const int n_iters = 100000;
  {
    int32_t input[PARAM_N];
    double start = get_time();
    unsigned long long start_cycle = cpucycles();
    for (int i = 0; i < n_iters; ++i) {
      ntt(input);
    }
    unsigned long long end_cycle = cpucycles();
    double end = get_time();
    double elapsed = (end - start) / n_iters;
    double elapsed_cycles = (end_cycle - start_cycle) / n_iters;
    int num_per_sec = 1.0 / elapsed;
    printf("Newhope avx2 time %f, %d\n", elapsed, num_per_sec);
    printf("Newhope avx2 cycles %f\n", elapsed_cycles);
  }

  {
    uint16_t input[PARAM_N];
    double start = get_time();
    unsigned long long start_cycle = cpucycles();
    for (int i = 0; i < n_iters; ++i) {
      fft(input);
    }
    unsigned long long end_cycle = cpucycles();
    double end = get_time();
    double elapsed = (end - start) / n_iters;
    double elapsed_cycles = (end_cycle - start_cycle) / n_iters;
    int num_per_sec = 1.0 / elapsed;
    printf("generated fft time %f, %d\n", elapsed, num_per_sec);
    printf("generated fft cycles %f\n", elapsed_cycles);
  }

  return 0;
}
