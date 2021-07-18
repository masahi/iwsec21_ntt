To compare performance of generated code against newhope, go to `evaluation` directory and run `make`.

Three executables, corresponding to our avx2, avx512 generated code and newhope avx2 assembly will be created. Since we use Linux `perf_event` feature to evaluate cycle counts, sudo is probably required to run the executables.
