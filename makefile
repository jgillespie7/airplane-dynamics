source = types.f90 constants.f90 sim_properties.f90 pid.f90 airplane.f90 euler_rotate.f90 dot_cross.f90 dynamics.f90 
airplane:	$(source)
	gfortran -o airplane $(source)

.PHONY: clean
clean:
	rm -f *.mod
