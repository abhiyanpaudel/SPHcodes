##########################################################
# Makefile for EDU2D-CCFV-Euler-EXPLCT
##########################################################
 PROGRAM = poiseuille_sph
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
    $(SDIR)/periodic_bc.o\
    $(SDIR)/boundary_part.o\
    $(SDIR)/density.o\
	$(SDIR)/neighbor_search.o\
	$(SDIR)/eos.o\
	$(SDIR)/time_integration.o\
	$(SDIR)/ghost_solid_bc.o\
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
	rm -f poiseuille_sph
