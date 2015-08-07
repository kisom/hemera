BIN :=		dist/build/hemera/hemera
SOURCES :=	$(shell ls src/*.hs src/*.lhs 2>/dev/null)

all: $(SOURCES)
	cabal build
	cp $(BIN) .

show-source:
	echo "$(SOURCES)"

clean:
	cabal clean

