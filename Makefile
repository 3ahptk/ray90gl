# This makefile has been tested on
#   computer: MacBook Pro 2013
#         OS: MacOS X 10.3.5
#   compiler: gfortran (GNU Fortran (Homebrew gcc 6.1.0) 6.1.0)
#     OpenGL: OpenGL 1.3.3, Glut 3.2.7, f90gl 1.2.9

#------------- User configuration parameters ---------------

# modify these for your system

# the directories containing the OpenGL libraries, f90gl libraries, GLUT
# libraries, and f90gl GLUT libraries
OGLLIBDIR = -L./lib

# the directory containing the X11 libraries
X11LIBDIR =

# the fortran 90 libraries for OpenGL, including GLUT, GLU and OpenGL
F90GLUTLIB = -lf90glut -lf90GLU -lf90GL

# the X11 libraries
#X11LIB = -Wl,-framework -Wl,GLUT -Wl,-framework -Wl,OpenGL -Wl,-framework -Wl,Cocoa
X11LIB = -framework GLUT -framework OpenGL -framework Cocoa

# the f90 compiler flag for specifying the location of MOD files
MODS = -I./include/GL

# fortran 90 compiler and compiler flags
F90 = gfortran
F90FLAGS = -O -fno-range-check

# fortran 90 compiler flag for fixed source form
FFIXED = -fixed

#----------- end of user configuration parameters ------------

all: raygl

raygl: raygl.f90
	$(F90) $(F90FLAGS) -o raygl $(MODS) raygl.f90 \
	$(OGLLIBDIR) $(F90GLUTLIB) $(X11LIBDIR) $(X11LIB)

clean:
	rm -f *.o *.mod raygl
