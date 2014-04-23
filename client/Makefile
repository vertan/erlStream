.SILENT:

JAVAC := javac
JAVAC_FLAGS :=

JAVA_FILES := $(wildcard src/*.java)
CLASS_FILES := $(patsubst src/%.java,bin/%.class,${JAVA_FILES})

CLASS_DIR := bin

all: $(CLASS_FILES)

bin/%.class: src/%.java
	$(JAVAC) $(JAVAC_FLAGS) $< -d $(CLASS_DIR) 

start: all
	(cd bin && java Client)

test: all
	# Run tests

doc:
	# javadoc src/*.java

clean:
	rm -f bin/*.class
	rm -f doc/*