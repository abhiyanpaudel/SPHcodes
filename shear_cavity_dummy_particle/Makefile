
##########################################################
 PROGRAM = shear_cavity_dummy_part_sph
##########################################################
# Suffix Rule for f90
# Note: use "gfortran -O2" for best performance, but
#       don't use it until you're sure bugs are removed.
##########################################################
.SUFFIXES : .o .f90
.f90.o:
#	gfortran -O0 -g -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file  -fcheck=all  -std=f2008  -pedantic  -fbacktrace -fall-intrinsics -c $<
	gfortran -O2 -c $<
##########################################################
SDIR = .
OBCTS =	$(SDIR)/av_velocity.o\
    $(SDIR)/dummy_particles_generation.o\
    $(SDIR)/dummy_mapping.o\
    $(SDIR)/boundary_par.o\
    $(SDIR)/density_summation_subroutine.o\
	$(SDIR)/neighbor_search.o\
	$(SDIR)/eos.o\
	$(SDIR)/time_integration.o\
	$(SDIR)/input.o\
	$(SDIR)/kernel.o\
	$(SDIR)/navier_stoke_equation_subroutine.o\
	$(SDIR)/output.o\
	$(SDIR)/single_step.o\
	$(SDIR)/sph.o\

##########################################################
# Make executable "mg"
# Note: use "gfortran -O2" for best performance, but
#       don't use it until you're sure bugs are removed.
##########################################################
$(PROGRAM): $(OBCTS)
#	gfortran -O0 -g -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file  -fcheck=all  -std=f2008  -pedantic  -fbacktrace -fall-intrinsics -o $@ $(OBCTS)
	gfortran -O2 -o $@ $(OBCTS)
##########################################################
# Clean up
##########################################################
clean:
	rm -f *.o
	rm -f *.mod
	rm -f shear_cavity_dummy_part_sph
