/**
 * Main logic module for the Disco interpreter
 */

module disco.Disco;


/**
 * Imports
 */

private import disco.absyn.SExp;

private import disco.parser.Parser;

private import std.stdio;


/**
 * Disco interpreter main class
 */

public class Disco
{
    /**
     * Parser
     */

    private Parser parser;


    /**
     * Constructor
     */

    public this ( )
    {
        this.parser = new Parser;
    }


    /**
     * Main program logic
     */

    public void run ( )
    {
        char[] input_buf;

        writefln("Disco!");

        while ( true )
        {
            writef("> ");
            stdin.readln(input_buf);
            auto pt = this.parser.parse(cast(string)input_buf);
            writefln("%s", pt);
            auto exp = Exp.create(pt);
            writefln("%s", exp.str);
        }
    }
}


/**
 * Main function
 */

void main ( string[] args )
{
    auto disco = new Disco;
    disco.run;
}