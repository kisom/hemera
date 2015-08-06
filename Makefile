BIN :=		dist/build/hemera/hemera
SOURCES :=	$(shell ls src/*.hs src/*.lhs)

all: $(SOURCES)
	cabal build
	cp $(BIN) .

clean:
	cabal clean

