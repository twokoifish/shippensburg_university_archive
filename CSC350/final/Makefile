# makefile

# Final runner name
OUT = final
# The main cpp file
SOURCES = $(OUT).cpp lib/helper/vec4/vec4.cpp lib/helper/mat4/mat4.cpp lib/helper/transformation/transformation.cpp lib/helper/shape/shape.cpp lib/helper/color/color.cpp lib/helper/component/component.cpp
# The object file to be compiled.
OBJECTS = $(OUT).o lib/helper/vec4/vec4.o lib/helper/mat4/mat4.o lib/helper/transformation/transformation.o lib/helper/shape/shape.o lib/helper/color/color.o lib/helper/component/component.o
# The header file to link.
HEADERS = $(OUT).h lib/helper/vec4/vec4.h lib/helper/mat4/mat4.h lib/helper/transformation/transformation.h lib/helper/shape/shape.h lib/helper/color/color.h lib/helper/component/component.h
# The files to clean
CLEANS = $(OUT) lib/helper/vec4/*o lib/helper/mat4/*o lib/helper/transformation/*o lib/helper/shape/*o lib/helper/color/*o lib/helper/component/*o
# The C compiler to use.
CC = g++
# C++

CFLAGS = -framework GLUT -framework OpenGL 

# Make the project.
final: $(OBJECTS)
	$(CC) $(CFLAGS) $(SOURCES) -o $(OUT)

# Remove any previous instances of the project. 
clean:
	rm -fR *o $(CLEANS)