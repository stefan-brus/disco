/**
 * The expression value types
 *
 * Each value is a pair of type information, and the value itself
 */

module disco.absyn.Val;


/**
 * Imports
 */

private import disco.absyn.SExp;


/**
 * The value "nil"
 */

public const Value NIL = { Type.Nil, { sexp: new SExp([]) } };


/**
 * The value "true"
 */

public const Value TRUE = { Type.Boolean, { boolean: true } };


/**
 * The value "false"
 */

public const Value FALSE = { Type.Boolean, { boolean: false } };


/**
 * Type info enumerator
 */

public enum Type
{
    Nil,
    SExp,
    Number,
    Boolean
}


/**
 * Possible value union
 */

public union PossibleVals
{
    /**
     * Number value
     */

    double number;


    /**
     * Boolean value
     */

    bool boolean;


    /**
     * S-Expression value
     */

    Exp sexp;
}


/**
 * Value struct
 */

public struct Value
{
    /**
     * This value's type
     */

    Type type;


    /**
     * The actual value
     */

    PossibleVals val;


    /**
     * "Constructor" to easily create a new value
     *
     * Template Params:
     *      T = The type of the value
     *
     * Params:
     *      value = The value itself
     */

    static Value opCall ( T ) ( T value )
    in
    {
        static assert(is(T == double) || is(T == bool) || is(T : Exp), "Unknown value type");
    }
    body
    {
        Value result;

        static if ( is(T == double) )
        {
            result.type = Type.Number;
            result.val.number = value;
        }
        else static if ( is(T == bool) )
        {
            result.type = Type.Boolean;
            result.val.boolean = value;
        }
        else static if ( is(T : Exp) )
        {
            result.type = Type.SExp;
            result.val.sexp = value;
        }

        return result;
    }


    /**
     * Create an expression from a value
     *
     * Params:
     *      val = The value to turn to an expression
     *
     * Returns:
     *      The created expression
     */

    static Exp toExp ( Value val )
    {
        with ( Type ) switch ( val.type )
        {
            case Nil:
                return new .SExp([]);
            case SExp:
                return val.val.sexp;
            case Number:
                return new .Number(val.val.number);
            case Boolean:
                if ( val.val.boolean )
                {
                    return new .Symbol("true");
                }
                else
                {
                    return new .Symbol("false");
                }
            default:
                throw new SExpException("Unknown value type");
        }
    }
}