/**
 * This module contains the PEG grammar for the Disco language
 */

module disco.parser.Grammar;


/**
 * Imports
 */

private import pegged.grammar;

mixin(grammar(`
Disco:

    Exp      < Number / Symbol / SExp

    Number   <- "-"? ~([0-9]+) ("." ~([0-9]+))*
    Symbol   <- identifier / Operator
    Operator <- "+" / "-" / "*" / "/" / "==" / "!=" / ">=" / "<=" / ">" / "<"
    SExp     < "(" Exp* ")"

`));