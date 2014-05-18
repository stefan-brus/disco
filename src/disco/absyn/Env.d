/**
 * Environment class, to be used for both global and local scope
 */

module disco.absyn.Env;


/**
 * Imports
 */

private import disco.absyn.SExp;

private import disco.absyn.Val;

private import disco.runtime.Eval;

private import disco.util.container.HashMap;


/**
 * Built-in function delegate alias
 */

public alias Builtin.BuiltinDg BuiltinDg;


/**
 * Environment class
 */

public class Env
{
    /**
     * Global environment instance
     */

    private static Env _global;


    /**
     * String to environment object map
     */

    public HashMap!(string, Obj) objs;


    /**
     * The parent environment, if null, this instance is the global env
     */

    public Env parent;


    /**
     * Constructor
     *
     * Params:
     *      parent = The parent env
     */

    public this ( Env parent )
    {
        this.parent = parent;

        this.objs = new HashMap!(string, Obj);
    }


    /**
     * Static constructor
     */

    static this ( )
    {
        _global = new Env(null);
    }


    /**
     * Static method to get the global environment instance
     *
     * Returns:
     *      The global environment
     */

    public static Env global ( )
    {
        return _global;
    }


    /**
     * In operator
     * Enables usage of "str in env" syntax
     *
     * If the given string is not found in this environment, look in the parent env, unless it is null
     *
     * Params:
     *      key = The key
     *
     * Returns:
     *      The object bound to key, or null if key is unbound
     */

    public Obj opIn_r ( string key )
    in
    {
        assert(key, "Environment symbol is empty string");
    }
    body
    {
        if ( key in this.objs )
        {
            return this.objs[key];
        }
        else if ( parent )
        {
            return key in parent;
        }

        return null;
    }


    /**
     * Static function caller method
     *
     * Calls a function with the given name, arguments and environment, either user defined or built in
     *
     * Params:
     *      fn = The name of the function
     *      args = The arguments
     *      parent_env = The environment the function was called in
     */

    public static Value call ( string fn, Exp[] args, Env parent_env )
    {
        auto fn_obj = fn in parent_env;

        if ( !fn_obj )
        {
            throw new SExpException(fn ~ ": unknown function");
        }
        else if ( cast(Builtin)fn_obj )
        {
            return (cast(Builtin)fn_obj).dg(args, parent_env);
        }
        else if ( cast(Function)fn_obj )
        {
            auto func = cast(Function)fn_obj;
            auto env = new Env(parent_env);

            if ( args.length < func.args.length )
            {
                throw new SExpException(fn ~ ": too few arguments");
            }

            foreach ( idx, arg; func.args )
            {
                env.objs[arg.str] = new Variable(Evaluator.eval(args[idx], parent_env));
            }

            Value retval;

            foreach ( exp; func.exps )
            {
                retval = Evaluator.eval(exp, env);
            }

            return retval;
        }
        else
        {
            throw new SExpException(fn ~ ": unknown function type");
        }
    }
}


/**
 * Environment object base class
 */

private abstract class Obj
{

}


/**
 * Variable
 */

public class Variable : Obj
{
    /**
     * The value bound to this variable
     */

    public Value val;


    /**
     * Constructor
     *
     * Params:
     *      val = The value of this variable
     */

    public this ( Value val )
    {
        this.val = val;
    }
}


/**
 * Constant
 */

public class Constant : Variable
{
    /**
     * Constructor
     *
     * Params:
     *      val = The value of this constant
     */

    public this ( Value val )
    {
        super(val);
    }
}


/**
 * Base function object class
 */

public abstract class BaseFn : Obj
{
    /**
     * The function's arguments
     */

    public Symbol[] args;


    /**
     * Constructor
     *
     * Params:
     *      args = The arguments
     */

    public this ( Symbol[] args )
    {
        this.args = args;
    }
}


/**
 * User defined function
 */

public class Function : BaseFn
{
    /**
     * The function's expressions
     */

    public Exp[] exps;


    /**
     * Constructor
     *
     * Params:
     *      args = The arguments
     *      exps = The expression list
     */

    public this ( Symbol[] args, Exp[] exps )
    {
        super(args);

        this.exps = exps;
    }
}


/**
 * Built-in function
 */

public class Builtin : BaseFn
{
    /**
     * Built-in function delegate alias
     */

    private alias Value delegate ( Exp[], ref Env ) BuiltinDg;


    /**
     * The function's delegate
     */

    public BuiltinDg dg;


    /**
     * Constructor
     *
     * Params:
     *      dg = The delegate
     */

    public this ( BuiltinDg dg )
    {
        super([]);

        this.dg = dg;
    }
}