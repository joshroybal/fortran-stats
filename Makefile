FC = gfortran
#FCFLAGS = -g -Werror
#FLFLAGS =
FCFLAGS = -O2 -Werror
FLFLAGS = -static -s
program = driver
objects = driver.o io.o report.o stats.o
module = stats.mod

$(program): $(objects)
	$(FC) $(FLFLAGS) $^ -o $(program)

driver.o: stats.o $(module)

report.o: stats.o $(module)

$(module): stats.o

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

.PHONY : clean
clean:
	$(RM) $(program) *.o *.mod *~
