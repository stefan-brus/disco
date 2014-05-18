/**
 * The expression value types
 *
 * Each value is a pair of type information, and the value itself
 */

module disco.absyn.Val;


/**
 * The value "nil"
 */

public const Value NIL = { Type.Nil, cast(PossibleVals)0 };


/**
 * Type info enumerator
 */

public enum Type
{
    Nil,
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
        static assert(is(T == double) || is(T == bool), "Unknown value type");
    }
    body
    {
        Value result;

        static if ( is(T == double) )
        {
            result.type = Type.Number;
        }
        else if ( is(T == bool) )
        {
            result.type = Type.Boolean;
        }

        result.val = cast(PossibleVals)value;

        return result;
    }
}