# makefile

# The main cpp file
SOURCES = hw2.cpp matrix/matrix4.cpp
# The object file to be compiled.
OBJECTS = hw2.o matrix/matrix4.o
# The header file to link.
HEADERS = hw2.h matrix/matrix4.h
# The C compiler to use.
CC = g++

# Check OS so the correct libraries are linked.
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin) #if macOS
CFLAGS = -framework GLUT -framework OpenGL 
else #if Linux or else.
CFLAGS = -lGL -lGLU -lGLEW -lglut
endif

# Make the project.
hw2: $(OBJECTS)
	$(CC) $(SOURCES) $(CFLAGS) -o hw2   

# Remove any previous instances of the project. 
clean:
	rm -fR *o hw2 matrix/*o