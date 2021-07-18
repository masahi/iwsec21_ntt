calc_median_cycles_newhope:
	clang -O3 -mavx2 newhope/consts.s newhope/bitrev.s newhope/ntt_double.s newhope/omegas.c newhope/ntt.c newhope/reduce.c calc_median_cycles_newhope.c -o calc_median_cycles_newhope

calc_median_cycles_avx2:
	clang -O3 -mavx2 ../generated/ntt_avx2.c calc_median_cycles.c -o calc_median_cycles_avx2

calc_median_cycles_avx512:
	clang -O3 -mavx512bw -mavx512f ../generated/ntt_avx512.c calc_median_cycles.c -o calc_median_cycles_avx512


all:
	calc_median_cycles_newhope calc_median_cycles_avx2
