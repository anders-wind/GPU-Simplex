# can be run with `make run V=<number_of_variables> C=<number_of_constraints> N=<number_of_instances>`

SRC=src/
TARGET=bin/
CPLEX=/opt/ibm/ILOG/CPLEX_Studio1271/cplex/
V=3
C=3
N=1

default: compile

compile:
	mkdir -p $(TARGET)
	javac -cp $(SRC):$(CPLEX)lib/* $(SRC)Program.java -d $(TARGET)

run:
	java -cp $(TARGET):$(CPLEX)lib/* -Djava.library.path=$(CPLEX)bin/x86-64_linux/ Program $(V) $(C) $(N)

clean:
	rm -rf $(TARGET)

.PHONY: clean compile run
