/**
 * The base built-in functions of the Disco language
 */

module disco.runtime.builtin.Base;


/**
 * Imports
 */

private import disco.absyn.Env;

private import disco.absyn.SExp;

private import disco.absyn.Val;

private import disco.runtime.Eval;

private import disco.util.tmpl.Singleton;


/**
 * Base built in functions class
 */

public class Base : Singleton!(Base)
{
    /**
     * "eval" function
     *
     * Evaluates the given expressions in order, returns the value of the last one
     */

    public Value evalDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length < 1 )
        {
            throw new SExpException("eval: expected at least 1 argument");
        }
    }
    body
    {
        Value retval;

        foreach ( arg; args )
        {
            retval = Evaluator.eval(Value.toExp(Evaluator.eval(arg, env)), env);
        }

        return retval;
    }


    /**
     * "set" function
     *
     * Sets a variable in the global environment to the value of the given expression
     */

    public Value setDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("set: expected 2 arguments");
        }

        if ( !cast(Symbol)args[0] )
        {
            throw new SExpException("set: first argument must be a symbol");
        }
    }
    body
    {
        auto name = args[0].str;
        auto exp = args[1];
        auto cur_obj = name in env;

        if ( cast(Constant)cur_obj )
        {
            throw new SExpException("set: " ~ name ~ " is a constant");
        }
        else if ( cast(Builtin)cur_obj )
        {
            throw new SExpException("set: " ~ name ~ " is a built-in function");
        }

        env.objs[name] = new Variable(Evaluator.eval(exp, env));

        return NIL;
    }


    /**
     * "fun" function
     *
     * Defines a function with the given name, arguments and expression list
     */

    public Value funDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length < 3 )
        {
            throw new SExpException("fun: expected at least 3 arguments");
        }

        if ( !cast(Symbol)args[0] )
        {
            throw new SExpException("fun: first argument must be a symbol");
        }

        SExp arg_list = cast(SExp)args[1];

        if ( !arg_list )
        {
            throw new SExpException("fun: second argument must be a list of symbols");
        }

        foreach ( arg; arg_list.exps )
        {
            if ( !cast(Symbol)arg )
            {
                throw new SExpException("fun: second argument must be a list of symbols");
            }
        }
    }
    body
    {
        Symbol toSymbol ( Exp exp )
        {
            return cast(Symbol) exp;
        }

        auto name = args[0].str;
        auto exp = cast(SExp)args[1];
        auto cur_obj = name in env;

        if ( cast(Constant)cur_obj )
        {
            throw new SExpException("fun: " ~ name ~ " is a constant");
        }
        else if ( cast(Builtin)cur_obj )
        {
            throw new SExpException("fun: " ~ name ~ " is a built-in function");
        }

        auto arg_list = Array.transform(exp.exps, &toSymbol);

        env.objs[name] = new Function(arg_list, args[2 .. $]);

        return NIL;
    }


    /**
     * "if" function
     *
     * If the value of the cond expression is true, evaluate the first expression, otherwise the second one
     *
     * Nil expressions are always considered false
     *
     * Numeric expressions are considered "true" for non-zero values
     */

    public Value ifDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 3 )
        {
            throw new SExpException("if: expected 3 arguments");
        }
    }
    body
    {
        auto cond = Evaluator.eval(args[0], env);

        with ( Type ) switch ( cond.type )
        {
            case Nil:
                return Evaluator.eval(args[2], env);

            case Boolean:
                return cond.val.boolean ? Evaluator.eval(args[1], env) : Evaluator.eval(args[2], env);

            case Number:
                return cond.val.number != 0 ? Evaluator.eval(args[1], env) : Evaluator.eval(args[2], env);

            default:
                throw new SExpException("if: unknown condition type");
        }
    }


    /**
     * "len" function
     *
     * Returns the length of the value of an S-Expression
     */

    public Value lenDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("len: expected 1 argument");
        }
    }
    body
    {
        if ( cast(SExp)args[0] )
        {
            auto val = Evaluator.eval(args[0], env);

            if ( val.type == Type.SExp )
            {
                auto sexp = cast(SExp)val.val.sexp;

                if ( sexp )
                {
                    return Value(cast(double)sexp.exps.length);
                }
                else
                {
                    return Value(cast(double)1);
                }
            }
            else
            {
                return Value(cast(double)1);
            }
        }
        else
        {
            return Value(cast(double)1);
        }
    }


    /**
     * "cons" function
     *
     * Makes an S-Expression of 2 expressions
     */

    public Value consDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("cons: expected 2 arguments");
        }
    }
    body
    {
        Exp[] exps;

        auto exp1 = Value.toExp(Evaluator.eval(args[0], env));
        auto exp2 = Value.toExp(Evaluator.eval(args[1], env));

        exps ~= exp1;

        if ( cast(SExp)exp2 )
        {
            exps ~= (cast(SExp)exp2).exps;
        }
        else
        {
            exps ~= exp2;
        }

        return Value(new SExp(exps));
    }


    /**
     * "car" function
     *
     * Gets the first value of an S-Expression
     *
     * Result type of evaluating the argument must be an S-Expression
     */

    public Value carDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("car: expected 1 argument");
        }

        if ( !cast(SExp)args[0] )
        {
            throw new SExpException("car: argument must be an S-Expression");
        }
    }
    body
    {
        auto sexp = cast(SExp)args[0];

        if ( sexp.exps.length == 0 )
        {
            return NIL;
        }
        else
        {
            auto val = Evaluator.eval(sexp, env);

            if ( val.type == Type.SExp )
            {
                auto val_sexp = cast(SExp)val.val.sexp;

                if ( val_sexp )
                {
                    return val_sexp.exps.length > 0 ? Value(val_sexp.exps[0]) : NIL;
                }
                else
                {
                    return val;
                }
            }
            else
            {
                return val;
            }
        }
    }


    /**
     * "cdr" function
     *
     * Gets all but the first sub-expressions of an S-Expression
     *
     * Result type of evaluating the argument must be an S-Expression
     */

    public Value cdrDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("cdr: expected 1 argument");
        }

        if ( !cast(SExp)args[0] )
        {
            throw new SExpException("cdr: argument must be an S-Expression");
        }
    }
    body
    {
        auto sexp = cast(SExp)args[0];

        if ( sexp.exps.length < 2 )
        {
            return NIL;
        }
        else
        {
            auto val = Evaluator.eval(sexp, env);

            if ( val.type == Type.SExp )
            {
                auto val_sexp = cast(SExp)val.val.sexp;

                if ( val_sexp )
                {
                    return val_sexp.exps.length > 1 ? Value(new SExp(val_sexp.exps[1 .. $])) : NIL;
                }
                else
                {
                    return NIL;
                }
            }
            else
            {
                return NIL;
            }
        }
    }


    /**
     * "append" function
     *
     * Makes an S-Expression of 2 expressions
     */

    public Value appendDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 2 )
        {
            throw new SExpException("append: expected 2 arguments");
        }

        if ( !cast(SExp)args[0] )
        {
            throw new SExpException("append: first argument must be an S-Expression");
        }
    }
    body
    {
        Exp[] exps;

        auto result = cast(SExp)Value.toExp(Evaluator.eval(args[0], env));

        if ( !result )
        {
            throw new SExpException("append: first argument must evaluate to an S-Expression");
        }

        auto exp2 = Value.toExp(Evaluator.eval(args[1], env));

        result.exps ~= exp2;

        return Value(result);
    }


    /**
     * "list" function
     *
     * Makes an S-Expression of 2 expressions
     */

    public Value listDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length == 0 )
        {
            throw new SExpException("list: expected at least 1 argument");
        }
    }
    body
    {
        Exp[] exps;

        foreach ( arg; args )
        {
            exps ~= Value.toExp(Evaluator.eval(arg, env));
        }

        return Value(new SExp(exps));
    }


    /**
     * "quote" function
     *
     * Returns the literal expression passed to it
     */

    public Value quoteDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("quote: expected 1 argument");
        }
    }
    body
    {
        return Value(args[0]);
    }


    /**
     * "isNil" function
     *
     * Checks if the given expression evaluates to nil
     */

    public Value isNilDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("isNil: expected 1 argument");
        }
    }
    body
    {
        auto val = Evaluator.eval(args[0], env);

        if ( val.type == Type.Nil ||
            (val.type == Type.SExp && cast(SExp)val.val.sexp && (cast(SExp)val.val.sexp).exps.length == 0) )
        {
            return Value(true);
        }
        else
        {
            return Value(false);
        }
    }


    /**
     * "isSExp" function
     *
     * Checks if the given expression evaluates to an S-Expression
     */

    public Value isSExpDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("isSExp: expected 1 argument");
        }
    }
    body
    {
        auto val = Evaluator.eval(args[0], env);

        if ( val.type == Type.Nil || (val.type == Type.SExp && cast(SExp)val.val.sexp) )
        {
            return Value(true);
        }
        else
        {
            return Value(false);
        }
    }


    /**
     * "isNum" function
     *
     * Checks if the given expression evaluates to a number
     */

    public Value isNumDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("isNum: expected 1 argument");
        }
    }
    body
    {
        auto val = Evaluator.eval(args[0], env);

        if ( val.type == Type.Number )
        {
            return Value(true);
        }
        else
        {
            return Value(false);
        }
    }


    /**
     * "isBool" function
     *
     * Checks if the given expression evaluates to a boolean
     */

    public Value isBoolDg ( Exp[] args, ref Env env )
    in
    {
        if ( args.length != 1 )
        {
            throw new SExpException("isBool: expected 1 argument");
        }
    }
    body
    {
        auto val = Evaluator.eval(args[0], env);

        if ( val.type == Type.Boolean )
        {
            return Value(true);
        }
        else
        {
            return Value(false);
        }
    }
}