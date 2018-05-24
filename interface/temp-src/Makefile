OBJS=objs/main.o objs/sexpr.o objs/server.o
CXXFLAGS=-Wall -Wextra -std=c++14
LDFLAGS=-lboost_system
CC=g++
OUT=tmp.out

all: objs $(OUT)

$(OUT): $(OBJS)
	$(CC) $(CXXFLAGS)    -o $@ $^ $(LDFLAGS)

objs/%.o: %.cpp
	$(CC) $(CXXFLAGS) -c -o $@ $<

objs:
	mkdir objs

clean:
	@rm -rf objs
	@rm $(OUT)

.PHONY:all clean

