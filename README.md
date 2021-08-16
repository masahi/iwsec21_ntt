To compare performance of generated code against newhope, go to `evaluation` directory and run `make`.

Three executables, corresponding to our avx2, avx512 generated code and newhope avx2 assembly will be created. Since we use Linux `perf_event` feature to evaluate cycle counts, sudo is probably required to run the executables.


Example outputs:
```
$ ./calc_cycles_newhope
median of 100 runs: 6082.810720
$ ./calc_cycles_avx2
median of 100 runs: 5401.729290
$ ./calc_cycles_avx512
median of 100 runs: 4351.765025
```
