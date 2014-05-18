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

    Exp      < Symbol / Number / SExp

    Symbol   <- identifier
    Number   <- "-"? ~([0-9]+) ("." ~([0-9]+))*
    SExp     < "(" Exp* ")"

`));