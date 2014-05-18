/**
 * Main logic module for the Disco interpreter
 */

module disco.Disco;


/**
 * Imports
 */

private import disco.absyn.Env;

private import disco.absyn.SExp;

private import disco.absyn.Val;

private import disco.parser.Parser;

private import disco.runtime.Builtin;

private import disco.runtime.Constants;

private import disco.runtime.Eval;

private import disco.util.app.Application;

private import std.stdio;


/**
 * Disco interpreter main class
 */

public class Disco : Application
{
    /**
     * Parser
     */

    private Parser parser;


    /**
     * Constructor
     *
     * Params:
     *      str_args = The command line arguments
     */

    public this ( string[] str_args )
    {
        super(str_args);
        this.parser = new Parser;
    }


    /**
     * Process the command line arguments
     *
     * Returns:
     *      True if the program should keep running, false otherwise
     */

    override protected bool processArgs ( )
    {
        return true;
    }


    /**
     * Main program logic
     *
     * Params:
     *      first_run = Whether or not this is the first run of the program
     *
     * Returns:
     *      True if the program should keep running, false otherwise
     */

    override protected bool appMain ( bool first_run )
    {
        char[] input_buf;

        Constants().setupEnv;

        BuiltinFunctions().setupEnv;

        if ( first_run ) writefln("Disco!");

        while ( true )
        {
            writef("> ");
            stdin.readln(input_buf);

            auto pt = this.parser.parse(cast(string)input_buf);

            debug ( Parser ) writefln("%s", pt);

            if ( !pt.successful )
            {
                throw new SExpException("Parse error. Check your parentheses.");
            }

            auto exp = Exp.create(pt);

            debug ( ExpTree ) writefln("%s", exp.str);

            auto val = Evaluator.eval(exp, Env.global);

            if ( val.type == Type.Nil )
            {
                writefln("nil");
            }
            else if ( val.type == Type.Boolean )
            {
                writefln("%s", val.val.boolean);
            }
            else if ( val.type == Type.Number )
            {
                writefln("%s", val.val.number);
            }
            else if ( val.type == Type.SExp )
            {
                writefln("%s", val.val.sexp.str);
            }
            else
            {
                throw new SExpException("Main: Unknown expression type");
            }
        }

        return true;
    }


    /**
     * Reset the program
     */

    override protected void reset ( )
    {

    }
}


/**
 * Main function
 */

void main ( string[] args )
{
    auto disco = new Disco(args);
    disco.run;
}