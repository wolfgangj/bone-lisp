COMPILE_FLAGS=-std=gnu99 -Wall -W -Wextra -Wno-unused -Wno-unused-parameter
FLAGS=-g
#FLAGS=-g -pg
#FLAGS=-O3

EXTRA_MODULES=boneposix.o

MODULES=bone.o main.o $(EXTRA_MODULES)

%.o: %.c bone.h
	$(CC) $(FLAGS) $(COMPILE_FLAGS) -c $< -o $@

bone: $(MODULES)
	$(CC) $(FLAGS) $(MODULES) -lm -o bone

clean:
	rm -f bone *.o

test: bone
	prove -e ./bone tests/*.bn

docs: bone
	./bone gendoc.bn -i core.bn prelude.bn posix.bn posixprelude.bn std/*.bn
	mkdir -p doc/std
	for f in index.md *.bn.md std/*.bn.md; do markdown "$$f" >"doc/`echo $$f | sed 's/.md$$/.html/'`"; done
	rm index.md; find . -name '*.bn.md' | xargs rm
