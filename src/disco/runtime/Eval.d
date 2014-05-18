/**
 * Expression evaluator class
 */

module disco.runtime.Eval;


/**
 * Imports
 */

private import disco.absyn.Env;

private import disco.absyn.SExp;

private import disco.absyn.Val;


/**
 * Evaluator class
 */

public class Evaluator
{
    /**
     * Static method for evaluating an S-Expression
     *
     * Turns an expression into a value
     * Symbols are evaluated as variables
     * Numbers are simply turned into values
     * S-Expressions are evaluated as function calls
     */

    public static Value eval ( Exp exp, Env env )
    {
        if ( cast(Symbol)exp )
        {
            if ( exp.str in env )
            {
                return (cast(Variable)(exp.str in env)).val;
            }
            else
            {
                return NIL;
            }
        }
        else if ( cast(Number)exp )
        {
            return Value((cast(Number)exp).num);
        }
        else if ( cast(SExp)exp )
        {
            auto exps = (cast(SExp)exp).exps;

            if ( !cast(Symbol)exps[0] )
            {
                throw new SExpException("First expression in a function call must be a symbol");
            }

            return Env.call(exps[0].str, exps[1 .. $], env);
        }
        else
        {
            throw new SExpException("Unknown expression type");
        }
        return NIL;
    }
}