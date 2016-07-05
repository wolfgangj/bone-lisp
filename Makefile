CC=clang
FLAGS=-std=gnu99 -Wall -W -Wextra -Wno-unused -Wno-unused-parameter -g
#FLAGS=-std=gnu99 -Wall -W -Wextra -Wno-unused -Wno-unused-parameter -O3
EXTRA_MODULES=boneposix.o

MODULES=bone.o main.o $(EXTRA_MODULES)

%.o: %.c bone.h
	$(CC) $(FLAGS) -c $< -o $@

bone: $(MODULES)
	$(CC) $(MODULES) -o bone

clean:
	rm -f bone *.o

test: bone
	prove -e ./bone tests/*.bn
