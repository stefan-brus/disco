/**
 * Utility functions for operating on arrays
 */

module disco.util.Array;


/**
 * Combines all elements in the given list of arrays into one array
 *
 * Template Params:
 *      T = The type of elements stored in the arrays
 *
 * Params:
 *      arrs = The arrays to flatten
 *
 * Returns:
 *      The flattened array
 */

public T[] flatten ( T ) ( T[][] arrs )
in
{
    assert(arrs.length, "Empty array");
}
body
{
    T[] result;

    foreach ( arr; arrs )
    {
        result ~= arr;
    }

    return result;
}