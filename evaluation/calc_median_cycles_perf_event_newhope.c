#include "ntt.h"
#include <asm/unistd.h>
#include <linux/perf_event.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>
#include <x86intrin.h>

#include <inttypes.h>
#include <sys/types.h>

static long perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
                            int cpu, int group_fd, unsigned long flags) {
  int ret;

  ret = syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
  return ret;
}

#define N_samples 100

static int cmp_llu(const void *a, const void *b) {
  if (*(double *)a < *(double *)b)
    return -1;
  if (*(double *)a > *(double *)b)
    return 1;
  return 0;
}

static double median(double *l, size_t llen) {
  qsort(l, llen, sizeof(double), cmp_llu);

  if (llen % 2)
    return l[llen / 2];
  else
    return (l[llen / 2 - 1] + l[llen / 2]) / 2;
}

int main() {
  struct perf_event_attr pe;
  memset(&pe, 0, sizeof(struct perf_event_attr));
  pe.type = PERF_TYPE_HARDWARE;
  pe.size = sizeof(struct perf_event_attr);
  pe.config = PERF_COUNT_HW_CPU_CYCLES;
  pe.disabled = 1;
  pe.exclude_kernel = 1;
  // Don't count hypervisor events.
  pe.exclude_hv = 1;

  int fd = perf_event_open(&pe, 0, -1, -1, 0);
  if (fd == -1) {
    fprintf(stderr, "Error opening perf event\n");
    exit(EXIT_FAILURE);
  }

  const int n_iters = 100000;
  double cycles[N_samples];

  long long count;

  for (int i = 0; i < N_samples; ++i) {
    {
      int32_t input[PARAM_N];
      ioctl(fd, PERF_EVENT_IOC_RESET, 0);
      ioctl(fd, PERF_EVENT_IOC_ENABLE, 0);
      for (int i = 0; i < n_iters; ++i) {
        ntt(input);
      }
      ioctl(fd, PERF_EVENT_IOC_DISABLE, 0);
      read(fd, &count, sizeof(long long));
      double elapsed_cycles = (double)count / n_iters;
      cycles[i] = elapsed_cycles;
    }
  }

  for (int i = 0; i < N_samples; ++i) {
    printf("%f\n", cycles[i]);
  }
  printf("median of 100 runs: %f\n", median(cycles, N_samples));

  return 0;
}
