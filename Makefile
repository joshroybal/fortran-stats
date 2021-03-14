FC = gfortran
FCFLAGS = -g -Werror
FLFLAGS =
#FCFLAGS = -O2 -Werror
#FLFLAGS = -static -s
program = stats_demo
objects = driver.o io.o report.o stats.o

$(program): $(objects)
	$(FC) $(FLFLAGS) $^ -o $(program)

driver.o: driver.f90 stats.o
	$(FC) $(FCFLAGS) -c $<

report.o: report.f90 stats.o
	$(FC) $(FCFLAGS) -c $<

io.o: io.f90
	$(FC) $(FCFLAGS) -c $<

stats.o: stats.f90
	$(FC) $(FCFLAGS) -c $<

.PHONY : clean
clean:
	$(RM) $(program) *.o *.mod *~
