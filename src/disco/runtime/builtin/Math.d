/**
 * The built-in math functions of the Disco language
 */

module disco.runtime.builtin.Math;


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

public class Math : Singleton!(Math)
{
    /**
     * "+" function
     *
     * Adds an arbitrary number of numeric values
     */

    public Value plusDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length < 2 )
        {
            throw new SExpException("+: expected at least 2 arguments");
        }
    }
    body
    {
        double result = 0;

        foreach ( exp; args )
        {
            auto val = Evaluator.eval(exp, env);

            if ( val.type != Type.Number )
            {
                throw new SExpException("+: arguments must be numbers");
            }

            result += val.val.number;
        }

        return Value(result);
    }


    /**
     * "-" function
     *
     * Subtracts an arbitrary number of numeric values
     */

    public Value minusDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length < 2 )
        {
            throw new SExpException("-: expected at least 2 arguments");
        }
    }
    body
    {
        auto first_val = Evaluator.eval(args[0], env);

        if ( first_val.type != Type.Number )
        {
            throw new SExpException("-: arguments must be numbers");
        }

        double result = first_val.val.number;

        foreach ( exp; args[1 .. $] )
        {
            auto val = Evaluator.eval(exp, env);

            if ( val.type != Type.Number )
            {
                throw new SExpException("-: arguments must be numbers");
            }

            result -= val.val.number;
        }

        return Value(result);
    }


    /**
     * "*" function
     *
     * Multiplies an arbitrary number of numeric values
     */

    public Value mulDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length < 2 )
        {
            throw new SExpException("*: expected at least 2 arguments");
        }
    }
    body
    {
        double result = 1;

        foreach ( exp; args )
        {
            auto val = Evaluator.eval(exp, env);

            if ( val.type != Type.Number )
            {
                throw new SExpException("*: arguments must be numbers");
            }

            result *= val.val.number;
        }

        return Value(result);
    }


    /**
     * "/" function
     *
     * Divides an arbitrary number of numeric values
     */

    public Value divDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length < 2 )
        {
            throw new SExpException("/: expected at least 2 arguments");
        }
    }
    body
    {
        auto first_val = Evaluator.eval(args[0], env);

        if ( first_val.type != Type.Number )
        {
            throw new SExpException("/: arguments must be numbers");
        }

        double result = first_val.val.number;

        foreach ( exp; args[1 .. $] )
        {
            auto val = Evaluator.eval(exp, env);

            if ( val.type != Type.Number )
            {
                throw new SExpException("/: arguments must be numbers");
            }

            if ( val.val.number == 0 )
            {
                throw new SExpException("/: division by 0");
            }

            result /= val.val.number;
        }

        return Value(result);
    }
}