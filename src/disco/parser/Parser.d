/**
 * Parser, calls the pegged grammar generator
 *
 * Example usage:
 *
 * Parser parser = new Parser;
 * auto parse_tree = parser.parse(str);
 */

module disco.parser.Parser;


/**
 * Imports
 */

private import disco.parser.Grammar;

private import pegged.peg;


/**
 * Parser class
 */

public class Parser
{
    /**
     * Parse a string and return the generated parse tree
     *
     * Params:
     *      str = The token array
     *
     * Returns:
     *      The parse tree
     */

    public ParseTree parse ( string str )
    {
        return Disco(str);
    }
}