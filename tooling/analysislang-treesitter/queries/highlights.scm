; Keywords
"syntax" @keyword
"rules" @keyword
"rule" @keyword
"transition" @keyword
"in" @keyword

; Operators
"::=" @operator
"=>" @operator
"=" @operator
"~>" @operator
"|" @operator

; Punctuation
"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"," @punctuation.delimiter
";" @punctuation.delimiter

; Strings
(string) @string

; Comments
(comment) @comment

; Function calls (functors) - highlight the function name
(functor (identifier) @function)

; Regular identifiers
(atom) @variable
(identifier) @variable

; String literals in rule names get special highlighting
(rule (string) @string.special)

; Error highlighting for malformed syntax
ERROR @error