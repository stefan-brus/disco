/**
 * The built-in comparison functions of the Disco language
 */

module disco.runtime.builtin.Compare;


/**
 * Imports
 */

private import disco.absyn.Env;

private import disco.absyn.SExp;

private import disco.absyn.Val;

private import disco.runtime.Eval;

private import disco.util.tmpl.Singleton;


/**
 * Built in math functions class
 */

public class Compare : Singleton!(Compare)
{
    /**
     * "==" function
     *
     * Checks if the values of two expressions are equal
     */

    public Value eqDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("==: expected 2 arguments");
        }
    }
    body
    {
        auto val1 = Evaluator.eval(args[0], env);
        auto val2 = Evaluator.eval(args[1], env);

        if ( val1.type != val2.type )
        {
            return FALSE;
        }
        else if ( val1.type == Type.Boolean )
        {
            return Value(val1.val.boolean == val2.val.boolean);
        }
        else if ( val2.type == Type.Number )
        {
            return Value(val1.val.number == val2.val.number);
        }

        return FALSE;
    }


    /**
     * "!=" function
     *
     * Checks if the values of two expressions are not equal
     */

    public Value notEqDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("!=: expected 2 arguments");
        }
    }
    body
    {
        auto val1 = Evaluator.eval(args[0], env);
        auto val2 = Evaluator.eval(args[1], env);

        if ( val1.type != val2.type )
        {
            return TRUE;
        }
        else if ( val1.type == Type.Boolean )
        {
            return Value(val1.val.boolean != val2.val.boolean);
        }
        else if ( val2.type == Type.Number )
        {
            return Value(val1.val.number != val2.val.number);
        }

        return TRUE;
    }


    /**
     * ">" function
     *
     * Checks if the first expression is greater than the other
     */

    public Value gtDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException(">: expected 2 arguments");
        }
    }
    body
    {
        auto val1 = Evaluator.eval(args[0], env);
        auto val2 = Evaluator.eval(args[1], env);

        if ( val1.type != val2.type )
        {
            return FALSE;
        }
        else if ( val1.type == Type.Boolean )
        {
            return FALSE;
        }
        else if ( val2.type == Type.Number )
        {
            return Value(val1.val.number > val2.val.number);
        }

        return FALSE;
    }


    /**
     * "<" function
     *
     * Checks if the first expression is less than the other
     */

    public Value ltDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("<: expected 2 arguments");
        }
    }
    body
    {
        auto val1 = Evaluator.eval(args[0], env);
        auto val2 = Evaluator.eval(args[1], env);

        if ( val1.type != val2.type )
        {
            return FALSE;
        }
        else if ( val1.type == Type.Boolean )
        {
            return FALSE;
        }
        else if ( val2.type == Type.Number )
        {
            return Value(val1.val.number < val2.val.number);
        }

        return FALSE;
    }


    /**
     * ">=" function
     *
     * Checks if the first expression is greater than or equal to the other
     */

    public Value gtEqDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException(">=: expected 2 arguments");
        }
    }
    body
    {
        auto val1 = Evaluator.eval(args[0], env);
        auto val2 = Evaluator.eval(args[1], env);

        if ( val1.type != val2.type )
        {
            return FALSE;
        }
        else if ( val1.type == Type.Boolean )
        {
            return FALSE;
        }
        else if ( val2.type == Type.Number )
        {
            return Value(val1.val.number >= val2.val.number);
        }

        return FALSE;
    }


    /**
     * "<=" function
     *
     * Checks if the first expression is less than or equal to the other
     */

    public Value ltEqDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("<=: expected 2 arguments");
        }
    }
    body
    {
        auto val1 = Evaluator.eval(args[0], env);
        auto val2 = Evaluator.eval(args[1], env);

        if ( val1.type != val2.type )
        {
            return FALSE;
        }
        else if ( val1.type == Type.Boolean )
        {
            return FALSE;
        }
        else if ( val2.type == Type.Number )
        {
            return Value(val1.val.number <= val2.val.number);
        }

        return FALSE;
    }
}