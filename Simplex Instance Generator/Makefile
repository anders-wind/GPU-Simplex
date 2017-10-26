# can be run with `make run V=<number_of_variables> C=<number_of_constraints> N=<number_of_instances>`

SRC=src/
TARGET=bin/
CPLEX=/opt/ibm/ILOG/CPLEX_Studio1271/cplex/
N=1
VLOW=3
VHIGH=3
CLOW=3
CHIGH=3

default: compile

compile: $(TARGET)*.class

$(TARGET)%.class: $(SRC)%.java
	mkdir -p $(TARGET)
	javac -cp $(SRC):$(CPLEX)lib/* $(SRC)Program.java -d $(TARGET)

run: $(TARGET)*.class
	java -Xmx4096m -cp $(TARGET):$(CPLEX)lib/* -Djava.library.path=$(CPLEX)bin/x86-64_linux/ \
		Program $(N) $(VLOW) $(VHIGH) $(CLOW) $(CHIGH)

clean:
	rm -rf $(TARGET)

.PHONY: clean compile run
