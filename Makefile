FLAGS=-std=gnu99 -Wall -W -Wextra -Wno-unused -g
EXTRA_MODULES=boneposix.o

MODULES=bone.o main.o $(EXTRA_MODULES)

%.o: %.c bone.h
	gcc $(FLAGS) -c $< -o $@

bone: $(MODULES)
	gcc $(MODULES) -o bone

clean:
	rm -f bone *.o

test: bone
	prove -e ./bone tests/*.bn
