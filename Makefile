# Common for all targets
COMMON_SRC = src/disco/absyn/Env.d src/disco/absyn/SExp.d src/disco/absyn/Val.d \
    src/disco/parser/Grammar.d src/disco/parser/Parser.d \
    src/disco/runtime/Builtin.d src/disco/runtime/Constants.d src/disco/runtime/Eval.d \
    src/disco/runtime/builtin/Base.d src/disco/runtime/builtin/Compare.d src/disco/runtime/builtin/Math.d \
    src/disco/util/Array.d \
    src/disco/util/app/Application.d src/disco/util/app/Arguments.d \
    src/disco/util/container/HashMap.d \
    src/disco/util/tmpl/Singleton.d


# Pegged source files
PEGGED_SRC = Pegged/pegged/peg.d Pegged/pegged/grammar.d Pegged/pegged/parser.d \
    Pegged/pegged/dynamic/grammar.d Pegged/pegged/dynamic/peg.d


# Main target
MAIN_TARGET = src/disco/Disco.d $(COMMON_SRC) $(PEGGED_SRC)


# dmd flags
DCC = dmd
DFLAGS = -debug -g -unittest
#DFLAGS = $(DFLAGS) -debug=Parser
#DFLAGS = $(DFLAGS) -debug=ExpTree


# Targets
all: $(MAIN_TARGET)
    $(DCC) $(MAIN_TARGET) $(DFLAGS)

dummy: