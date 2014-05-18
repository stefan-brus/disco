/**
 * This class initializes the built-in functions of Disco
 *
 * Featured functions:
 *      (set var exp): sets the variable var to the value of the expression exp
 *      (fun fn-name (args) exps...): defines the function fn-name with the arguments args and the expression list exps
 *      (if cond exp1 exp2): if cond is true, return the value of exp1, else the value of exp2
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

        this.fn_map["set"] = &setDg;
        this.fn_map["fun"] = &funDg;
        this.fn_map["if"] = &ifDg;
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
        env.objs[args[0].str] = new Variable(Evaluator.eval(args[1], env));

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

        auto arg_list = Array.transform((cast(SExp)args[1]).exps, &toSymbol);

        env.objs[args[0].str] = new Function(arg_list, args[2 .. $]);

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
}