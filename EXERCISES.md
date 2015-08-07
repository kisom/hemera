[X] Our strings aren't quite R5RS compliant, because they don't support
    escaping of internal quotes within the string. Change parseString
    so that \" gives a literal quote character instead of terminating
    the string. You may want to replace noneOf "\"" with a new parser
    action that accepts either a non-quote character or a backslash
    followed by a quote mark.
[X] Modify the previous exercise to support \n, \r, \t, \\, and any other
    desired escape characters
[ ] parseExpr: Instead of using the try combinator, left-factor the
    grammar so that the common subsequence is its own parser. You
    should end up with a parser that matches a string of expressions,
    and one that matches either nothing or a dot and a single
    expressions. Combining the return values of these into either a
    List or a DottedList is left as a (somewhat tricky) exercise for
    the reader: you may want to break it out into another helper
    function.
