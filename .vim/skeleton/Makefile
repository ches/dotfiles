# A quick general Makefile for simple clang C++ scratch projects, GNU make.
# I'm not a Make guru, this may not be idiomatic.
#
# For some -MMD deps magic, see:
#   https://github.com/rikusalminen/makefile-for-c

# Implicit rule variables
CXX := c++
CXXFLAGS = -std=c++11 -stdlib=libc++ $(WARN) $(DEBUG)
LDLIBS :=
LDFLAGS :=
LINK.o = $(LINK.cpp)  # Override C linking with C++

# User override options
WARN := -Wall -W
DEBUG := -g

# Internal vars
srcs := # TODO
targets := # TODO
objs := $(srcs:.cpp=.o)

all: $(targets)

.PHONY: all

# Starter catch-all
$(targets): $(objs)

clean:
	$(RM) $(targets)
	$(RM) $(objs)

.PHONY: clean

