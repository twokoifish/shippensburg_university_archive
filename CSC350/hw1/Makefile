# makefile

# The main cpp file
SOURCES = hw1.cpp
# The object file to be compiled.
OBJECTS = hw1.o
# The header file to link.
HEADERS = hw1.h
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
hw1: $(OBJECTS)
	$(CC) $(SOURCES) $(CFLAGS) -o hw1    

# Remove any previous instances of the project. 
clean:
	rm -fR *o hw1