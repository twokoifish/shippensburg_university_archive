# makefile

# Final runner name
END = hw3
# The main cpp file
SOURCES = $(END).cpp lib/elementary/mat4/mat4.cpp lib/art/color/color.cpp lib/art/shape/shape.cpp lib/elementary/vec3/vec3.cpp
# The object file to be compiled.
OBJECTS = $(END).o lib/elementary/mat4/mat4.o lib/art/color/color.o lib/art/shape/shape.o lib/elementary/vec3/vec3.o
# The header file to link.
HEADERS = $(END).h lib/elementary/mat4/mat4.h lib/art/color/color.h lib/art/shape/shape.h lib/elementary/vec3/vec3.h
# The files to clean
CLEANS = $(END) lib/elementary/mat4/*o lib/art/color/*o lib/art/shape/*o lib/elementary/vec3/*o
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
	$(CC) $(SOURCES) $(CFLAGS) -o $(END)   

# Remove any previous instances of the project. 
clean:
	rm -fR *o $(CLEANS)