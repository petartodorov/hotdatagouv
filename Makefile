FC = gfortran

FCFLAGS = -g -fbounds-check
FCFLAGS = -O2

FCFLAGS += -I/usr/include

PROGRAMS = readandtransform

all: $(PROGRAMS)

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<
