#################### 
GROUP_NUMBER := 01
####################

.SILENT:

all: clean server client test doc

test: 
	echo "Testing server..."
	make test_server
	echo
	echo "Testing client... (Should print nothing)"
	make test_client
	echo "Tests done!"
	echo

doc: doc_server doc_client

clean: clean_server clean_client


##########
# SERVER #
##########

ERLC := erlc
ERLC_FLAGS := -W -I server/include

ERL_FILES := $(wildcard server/src/*.erl)
BEAM_FILES := $(patsubst server/src/%.erl,server/ebin/%.beam,${ERL_FILES})

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out server/%_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst server/src/%.erl,'server/src/%.erl', $(EDOC_SRC)))]

server: $(BEAM_FILES)

server/ebin/%.beam: server/src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o server/ebin $<

start_server: server
	(cd server/ebin && erl -noshell -eval 'server:start_cli(), init:stop()')

test_server: server
	(cd server/ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc_server: $(BEAM_FILES)
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'server/doc/html'}])" -s init stop

clean_server:
	rm -fr server/.#* *.dump
	rm -fr server/ebin/*.dump
	rm -fr server/ebin/*.beam
	(cd server/doc/html && find . -name "*" -a ! -name overview.edoc ! -name . -exec rm -rf {} \;)


##########
# CLIENT #
##########

JAVAC := javac
JAVA_FLAGS := -cp "../libs/*:. ../bin/*:."

JAVA_FILES :=  	client/src/Client.java \
		client/src/UI.java \
		client/src/GUI.java \
		client/src/CLI.java \
		client/src/AudioManager.java \
		client/src/Song.java \
		client/src/Communicator.java

CLASS_DIR := ../bin

client:	FORCE
	(cd client/src && $(JAVAC) $(JAVA_FLAGS) Client.java -d $(CLASS_DIR))

FORCE:

start_client: client
	(cd client/bin && java $(JAVA_FLAGS) Client)

start_client_cli: client
	(cd client/bin && java $(JAVA_FLAGS) Client cli)

test_client: client
	(cd client/src && $(JAVAC) $(JAVA_FLAGS) *Test*.java -d $(CLASS_DIR))
	(cd client/bin && java $(JAVA_FLAGS) AllTests)

doc_client:
	javadoc $(JAVA_FILES) -d client/doc/

clean_client:
	rm -f client/bin/*.class
	rm -fr client/doc/*


#########
# OTHER #
#########

REQUIRED_DIR_NAME := pop_2012_project_group_$(GROUP_NUMBER)

PROJECT_DIR := $(notdir $(shell pwd))

USER=$(shell whoami)
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H:%M:%S")__.tar.gz
ARCHIVE_DIR := ..

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" server/src/*.erl server/include/*.hrl doc/* server/doc/html/*

archive: clean
ifeq ($(REQUIRED_DIR_NAME), $(PROJECT_DIR))
	(cd $(ARCHIVE_DIR) && tar cvfz $(ARCHIVE_NAME) $(PROJECT_DIR) )
	@echo 
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo Error: Wrong directory name >$(PROJECT_DIR)<, change to >$(REQUIRED_DIR_NAME)<
endif
