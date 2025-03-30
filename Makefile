# tool macros
CXX := gcc

CXXFLAGS := -std=c99 -O3\
			-rdynamic\
			-Wl,--gc-sections\
			-flto=auto\
			-Wall -Wextra -Wpedantic -Wshadow -Winline -Wswitch\
			-Wfatal-errors\
			-Wno-unused-parameter\
			-Wno-unused-command-line-argument

PRFFLAGS := -g\
			-ggdb3\

DBGFLAGS := -ggdb3\
			-fno-lto\
			-DHW_DEBUG_CODE_ENABLE\
			-fsanitize=address

CCOBJFLAGS := $(CXXFLAGS) -c

# path macros
BIN_PATH := bin
PRF_PATH := prf
OBJ_PATH := obj
SRC_PATH := src
DBG_PATH := debug
LIB_PATH := lib

project_name := HayWire

# compile macros
TARGET_NAME := hw
ifeq ($(OS),Windows_NT)
	TARGET_NAME := $(addsuffix .exe,$(TARGET_NAME))
endif
TARGET := $(BIN_PATH)/$(TARGET_NAME)
TARGET_DEBUG := $(DBG_PATH)/$(TARGET_NAME)
TARGET_PROFILE := $(PRF_PATH)/$(TARGET_NAME)

# src files & obj files
SRC := $(foreach x, $(SRC_PATH), $(wildcard $(addprefix $(x)/*,.c*)))
OBJ := $(addprefix $(OBJ_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))
OBJ_PROF := $(addprefix $(PRF_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))
OBJ_DEBUG := $(addprefix $(DBG_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))

# clean files list
DISTCLEAN_LIST := $(OBJ) \
				  $(OBJ_PROF)\
                  $(OBJ_DEBUG)
CLEAN_LIST := $(TARGET) \
			  $(TARGET_DEBUG) \
			  $(TARGET_PROFILE) \
			  $(DISTCLEAN_LIST)\

# default rule
default: makedir all

builder-build :
	docker build -f builder.Dockerfile -t $(project_name)-builder:latest .

builder-run :
	docker run \
		--rm \
		-it \
		--platform linux/amd64 \
		--workdir /builder/mnt \
		-v ${PWD}:/builder/mnt \
		$(project_name)-builder:latest \
		/bin/bash


# non-phony targets
$(TARGET): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ)

$(OBJ_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CXX) $(CCOBJFLAGS) -o $@ $<

$(DBG_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CXX) $(CCOBJFLAGS) $(DBGFLAGS) -o $@ $<

$(PRF_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CXX) $(CCOBJFLAGS) $(PRFFLAGS) -o $@ $<

$(TARGET_DEBUG): $(OBJ_DEBUG)
	$(CXX) $(CXXFLAGS) $(DBGFLAGS) $(OBJ_DEBUG) -o $@

$(TARGET_PROFILE): $(OBJ_PROF)
	$(CXX) $(CXXFLAGS) $(PRFFLAGS) $(OBJ_PROF) -o $@

# phony rules
.PHONY: makedir
makedir:
	@mkdir -p $(BIN_PATH) $(PRF_PATH) $(OBJ_PATH) $(DBG_PATH)

.PHONY: all
all: $(TARGET)

.PHONY: debug
debug: $(TARGET_DEBUG)

.PHONY: prof
prof: $(TARGET_PROFILE)

.PHONY: clean
clean:
	@echo CLEAN $(CLEAN_LIST)
	@rm -rf $(CLEAN_LIST)

.PHONY: distclean
distclean:
	@echo CLEAN $(CLEAN_LIST)
	@rm -rf $(DISTCLEAN_LIST)
