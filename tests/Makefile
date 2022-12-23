test_pool: module_pool.o test_pool.o
	gfortran -g -o test_pool module_pool.o test_pool.o
test_field: module_pool.o module_field.o test_field.o
	gfortran -g -o test_field module_pool.o module_field.o test_field.o

module_%.o: ../module_%.f90
	gfortran -g -c $< -o $@
test_%.o: test_%.f90
	gfortran -g -c $< -o $@

.PHONY: clean test
clean:
	rm *.o *.mod
test: test_pool test_field
	./test_pool
	./test_field
