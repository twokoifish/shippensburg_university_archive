# makefile

# Final runner name
OUT = hw4
# The main cpp file
SOURCES = $(OUT).cpp lib/helper/vec4/vec4.cpp lib/helper/mat4/mat4.cpp lib/helper/transformation/transformation.cpp lib/helper/shape/shape.cpp lib/helper/color/color.cpp
# The object file to be compiled.
OBJECTS = $(OUT).o lib/helper/vec4/vec4.o lib/helper/mat4/mat4.o lib/helper/transformation/transformation.o lib/helper/shape/shape.o lib/helper/color/color.o
# The header file to link.
HEADERS = $(OUT).h lib/helper/vec4/vec4.h lib/helper/mat4/mat4.h lib/helper/transformation/transformation.h lib/helper/shape/shape.h lib/helper/color/color.h
# The files to clean
CLEANS = $(OUT) lib/helper/vec4/*o lib/helper/mat4/*o lib/helper/transformation/*o lib/helper/shape/*o lib/helper/color/*o
# The C compiler to use.
CC = g++
# C++
CXXFLAGS := -std=c++11

CFLAGS = -std=c++11

# Check OS so the correct libraries are linked.
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin) #if macOS
CFLAGS += -framework GLUT -framework OpenGL 
else #if Linux or else.
CFLAGS += -lGL -lGLU -lGLEW -lglut
endif

# Make the project.
hw4: $(OBJECTS)
	$(CC) $(CFLAGS) $(SOURCES) -o $(OUT)

# Remove any previous instances of the project. 
clean:
	rm -fR *o $(CLEANS)