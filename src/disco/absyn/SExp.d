/**
 * The S-Expression tree representation
 *
 * Each expression type can build itself recursively
 */

module disco.absyn.SExp;


/**
 * Imports
 */

private import Array = disco.util.Array;

private import pegged.peg;

private import std.conv;

private import std.string;


/**
 * S-Expression exception class
 */

public class SExpException : Exception
{
    /**
     * Constructor
     *
     * Params:
     *      msg = The error message
     */

    public this ( string msg )
    {
        super(msg);
    }
}


/**
 * Expression base class
 */

public abstract class Exp
{
    /**
     * Constructor
     *
     * Params:
     *      parse_tree = The parse tree to construct the expression from
     */

    public this ( ParseTree parse_tree )
    {
        this.build(parse_tree);
    }


    /**
     * Default constructor
     */

    public this ( )
    {

    }


    /**
     * Expression factory method
     *
     * Params:
     *      p = The parse tree
     *
     * Returns:
     *      A new expression
     */

    public static Exp create ( ParseTree p )
    {
        switch ( p.name )
        {
            case "Disco":
                return create(p.children[0]);
            case "Disco.Exp":
                return create(p.children[0]);
            case "Disco.SExp":
                return new SExp(p);
            case "Disco.Symbol":
                return new Symbol(p);
            case "Disco.Number":
                return new Number(p);
            default:
                assert(false, "Unknown parse token");
        }
    }


    /**
     * Same as above, but creates a list of expressions from a list of parse trees
     *
     * Params:
     *      pts = The parse trees
     *
     * Returns:
     *      The list of created expressions
     */

    public static Exp[] create ( ParseTree[] pts )
    {
        Exp[] result;

        foreach ( p; pts )
        {
            result ~= create(p);
        }

        return result;
    }


    /**
     * Get the string representation of this expression
     */

    abstract public string str ( );


    /**
     * Build the expression from the given parse tree
     *
     * Params:
     *      p = The parse tree
     */

    abstract protected void build ( ParseTree p );
}


/**
 * S-Expression
 */

public class SExp : Exp
{
    /**
     * Constructor
     *
     * Params:
     *      parse_tree = The parse tree to construct the expression from
     */

    public this ( ParseTree parse_tree )
    {
        super(parse_tree);
    }


    /**
     * Constructor
     *
     * Params:
     *      exps = The child expressions
     */

    public this ( Exp[] exps )
    {
        super();
        this.exps = exps;
    }


    /**
     * The rest of the expressions
     */

    public Exp[] exps;


    /**
     * To string implementation
     */

    override public string str ( )
    {
        string result = format("(");

        foreach ( exp; this.exps )
        {
            result ~= format(" %s", exp.str);
        }

        result ~= " )";

        return result;
    }


    /**
     * Build implementation
     */

    override protected void build ( ParseTree p )
    in
    {
        assert(p.name == "Disco.SExp", "Not an S-Expression");
    }
    body
    {
        this.exps = Exp.create(p.children);
    }
}


/**
 * Symbol
 */

public class Symbol : Exp
{
    /**
     * The string of this symbol
     */

    private string _str;


    /**
     * Constructor
     *
     * Params:
     *      parse_tree = The parse tree to construct the expression from
     */

    public this ( ParseTree parse_tree )
    {
        super(parse_tree);
    }


    /**
     * To string implementation
     */

    override public string str ( )
    {
        return this._str;
    }


    /**
     * Build implementation
     */

    override protected void build ( ParseTree p )
    in
    {
        assert(p.name == "Disco.Symbol", "Not a Symbol");
    }
    body
    {
        this._str = p.matches[0];
    }
}


/**
 * Number
 */

public class Number : Exp
{
    /**
     * Constructor
     *
     * Params:
     *      parse_tree = The parse tree to construct the expression from
     */

    public this ( ParseTree parse_tree )
    {
        super(parse_tree);
    }


    /**
     * The number value
     */

    public double num;


    /**
     * To string implementation
     */

    override public string str ( )
    {
        return format("%s", this.num);
    }


    /**
     * Build implementation
     */

    override protected void build ( ParseTree p )
    in
    {
        assert(p.name == "Disco.Number", "Not a Number");
    }
    body
    {
        this.num = to!double(Array.flatten(p.matches));
    }
}