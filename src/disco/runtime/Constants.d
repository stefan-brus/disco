/**
 * This class initializes the constants in Disco's global environment
 */

module disco.runtime.Constants;


/**
 * Imports
 */

private import disco.absyn.Env;

private import disco.absyn.Val;

private import disco.util.container.HashMap;

private import disco.util.tmpl.Singleton;


/**
 * Constants class
 */

public class Constants : Singleton!(Constants)
{
    /**
     * Constant name to value map
     */

    private HashMap!(string, Value) constant_map;


    /**
     * Initialize the constant map
     */

    override protected void init ( )
    {
        this.constant_map = new HashMap!(string, Value);

        this.constant_map["nil"] = NIL;
        this.constant_map["true"] = TRUE;
        this.constant_map["false"] = FALSE;
    }


    /**
     * Initialize the global environment with the built in functions
     */

    public void setupEnv ( )
    {
        foreach ( name, val; this.constant_map )
        {
            Env.global.objs[name] = new Constant(val);
        }
    }
}