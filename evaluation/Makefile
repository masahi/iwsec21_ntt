newhope_compare_lazy2:
	clang-11 -O3 -mavx2 consts.s bitrev.s ntt_double.s omegas.c ntt.c avx2/generated_lazy2.c newhope_compare.c reduce.c cpucycles.c -o newhope_compare_lazy2

newhope_compare_lazy2_avx512:
	clang-11 -O3 -mavx2 -mavx512bw -mavx512f  consts.s bitrev.s ntt_double.s omegas.c ntt.c avx512/generated_lazy2.c newhope_compare.c reduce.c cpucycles.c -o newhope_compare_lazy2_avx512

newhope_compare_lazy3_avx512:
	clang-11 -O3 -mavx512bw -mavx512f consts.s bitrev.s ntt_double.s omegas.c ntt.c avx512/generated_lazy3.c newhope_compare.c reduce.c cpucycles.c -o newhope_compare_lazy3_avx512

calc_median_cycles_newhope:
	clang-11 -O3 -mavx2 consts.s bitrev.s ntt_double.s omegas.c ntt.c reduce.c calc_median_cycles_newhope.c cpucycles.c -o calc_median_cycles_newhope

calc_median_cycles_avx2:
	clang-11 -O3 -mavx2 avx2/generated_lazy2.c calc_median_cycles.c cpucycles.c -o calc_median_cycles_avx2

calc_median_cycles_avx512:
	clang-11 -O3 -mavx512bw -mavx512f avx512/generated_lazy2.c calc_median_cycles.c cpucycles.c -o calc_median_cycles_avx512

calc_median_cycles_avx512_lazy3:
	clang-11 -O3 -mavx512bw -mavx512f avx512/generated_lazy3.c calc_median_cycles.c cpucycles.c -o calc_median_cycles_avx512_lazy3

calc_median_cycles_perf_event_newhope:
	clang-11 -O3 -mavx2 consts.s bitrev.s ntt_double.s omegas.c ntt.c reduce.c calc_median_cycles_perf_event_newhope.c cpucycles.c -o calc_median_cycles_perf_event_newhope

calc_median_cycles_perf_event_avx2:
	clang-11 -O3 -mavx2 avx2/generated_lazy2.c calc_median_cycles_perf_event.c cpucycles.c -o calc_median_cycles_perf_event_avx2

calc_median_cycles_perf_event_avx2_lazy3:
	clang-11 -O3 -mavx2 avx2/generated_lazy3.c calc_median_cycles_perf_event.c cpucycles.c -o calc_median_cycles_perf_event_avx2_lazy3

calc_median_cycles_perf_event_avx512:
	clang-11 -O3 -mavx512bw -mavx512f avx512/generated_lazy2.c calc_median_cycles_perf_event.c cpucycles.c -o calc_median_cycles_perf_event_avx512

calc_median_cycles_perf_event_avx512_lazy3:
	clang-11 -O3 -mavx512bw -mavx512f avx512/generated_lazy3.c calc_median_cycles_perf_event.c cpucycles.c -o calc_median_cycles_perf_event_avx512_lazy3


clean:
	rm newhope_compare_lazy2 newhope_compare_lazy3
