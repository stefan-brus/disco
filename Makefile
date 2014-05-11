# Common for all targets
COMMON_SRC = src/disco/absyn/SExp.d \
	src/disco/parser/Grammar.d src/disco/parser/Parser.d \
	src/disco/util/Array.d


# Pegged source files
PEGGED_SRC = Pegged/pegged/peg.d Pegged/pegged/grammar.d Pegged/pegged/parser.d \
	Pegged/pegged/dynamic/grammar.d Pegged/pegged/dynamic/peg.d


# Main target
MAIN_TARGET = src/disco/Disco.d $(COMMON_SRC) $(PEGGED_SRC)


# dmd flags
DCC = dmd
DFLAGS = -debug -g -unittest


# Targets
all: $(MAIN_TARGET)
	$(DCC) $(MAIN_TARGET) $(DFLAGS)

dummy: