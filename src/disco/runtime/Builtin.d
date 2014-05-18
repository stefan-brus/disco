/**
 * This class initializes the built-in functions of Disco
 *
 * Featured functions:
 * See Disco documentation
 *
 * TODO: Create some kind of documentation for Disco
 */

module disco.runtime.Builtin;


/**
 * Imports
 */

private import disco.absyn.Env;

private import disco.absyn.SExp;

private import disco.absyn.Val;

private import disco.runtime.Eval;

private import disco.util.container.HashMap;

private import disco.util.tmpl.Singleton;

private import Array = disco.util.Array;


/**
 * Built-in functions class
 */

public class BuiltinFunctions : Singleton!(BuiltinFunctions)
{
    /**
     * Function name to function delegate map
     */

    private HashMap!(string, BuiltinDg) fn_map;


    /**
     * Initialize the function map
     */

    override protected void init ( )
    {
        this.fn_map = new HashMap!(string, BuiltinDg);

        this.fn_map["eval"] = &this.evalDg;

        this.fn_map["set"] = &this.setDg;
        this.fn_map["fun"] = &this.funDg;
        this.fn_map["if"] = &this.ifDg;

        this.fn_map["+"] = &this.plusDg;
        this.fn_map["-"] = &this.minusDg;
        this.fn_map["*"] = &this.mulDg;
        this.fn_map["/"] = &this.divDg;

        this.fn_map["=="] = &this.eqDg;
        this.fn_map["!="] = &this.notEqDg;
        this.fn_map[">"] = &this.gtDg;
        this.fn_map["<"] = &this.ltDg;
        this.fn_map[">="] = &this.gtEqDg;
        this.fn_map["<="] = &this.ltEqDg;
    }


    /**
     * Initialize the global environment with the built in functions
     */

    public void setupEnv ( )
    {
        foreach ( name, dg; this.fn_map )
        {
            Env.global.objs[name] = new Builtin(dg);
        }
    }


    /**
     * "eval" function
     *
     * Evaluates the given expressions in order, returns the value of the last one
     */

    private Value evalDg ( Exp[] args, ref Env env )
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
            retval = Evaluator.eval(arg, env);
        }

        return retval;
    }


    /**
     * "set" function
     *
     * Sets a variable in the global environment to the value of the given expression
     */

    private Value setDg ( Exp[] args, ref Env env )
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