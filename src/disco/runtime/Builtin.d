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

private import disco.runtime.builtin.Base;

private import disco.runtime.builtin.Compare;

private import disco.runtime.builtin.Math;

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

        this.fn_map["eval"] = &Base().evalDg;

        this.fn_map["set"] = &Base().setDg;
        this.fn_map["fun"] = &Base().funDg;
        this.fn_map["if"] = &Base().ifDg;
        this.fn_map["len"] = &Base().lenDg;

        this.fn_map["+"] = &Math().plusDg;
        this.fn_map["-"] = &Math().minusDg;
        this.fn_map["*"] = &Math().mulDg;
        this.fn_map["/"] = &Math().divDg;

        this.fn_map["=="] = &Compare().eqDg;
        this.fn_map["!="] = &Compare().notEqDg;
        this.fn_map[">"] = &Compare().gtDg;
        this.fn_map["<"] = &Compare().ltDg;
        this.fn_map[">="] = &Compare().gtEqDg;
        this.fn_map["<="] = &Compare().ltEqDg;
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
}