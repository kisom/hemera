1. Our strings aren't quite R5RS compliant, because they don't support
   escaping of internal quotes within the string. Change parseString so
   that \" gives a literal quote character instead of terminating the
   string. You may want to replace noneOf "\"" with a new parser action
   that accepts either a non-quote character or a backslash followed by
   a quote mark.
2. Modify the previous exercise to support \n, \r, \t, \\, and any other
   desired escape characters
