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
{
    T[] result;

    foreach ( arr; arrs )
    {
        result ~= arr;
    }

    return result;
}


/**
 * Transforms an array of one type of element to an array of another type of element
 *
 * Calls the given transform delegate on each element in the given array
 *
 * Template Params:
 *      T = The type of the array
 *      U = The type to convert to
 *
 * Params:
 *      arr = The array to convert
 *      dg = The transform delegate
 *
 * Returns:
 *      The transformed array
 */

public U[] transform ( T, U ) ( T[] arr, U delegate ( T ) dg )
{
    U[] result;

    foreach ( elm; arr )
    {
        result ~= dg(elm);
    }

    return result;
}


/**
 * Unittests
 */

unittest
{
    /**
     * Error message
     */

    const err_msg = "Array unittests failed";


    /**
     * flatten
     */

    assert(flatten([[1, 2, 3], [4, 5, 6, 7], [], [8]]) == [1, 2, 3, 4, 5, 6, 7, 8], err_msg);
    assert(flatten(["Flatten ", "Test"]) == "Flatten Test", err_msg);


    /**
     * transform
     */

    import std.string;

    char[] transformDg ( int num )
    {
        return format("%s", num);
    }

    assert(transform([123, 456, 789], &transformDg) == ["123", "456", "789"], err_msg);
}