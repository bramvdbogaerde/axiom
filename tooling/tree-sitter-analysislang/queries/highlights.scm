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

; Function calls (functors) - highlight the function name first
(functor (identifier) @function)

; Sort names in syntax declarations - the identifier after "in" 
(syntax_rule
  (variable_list)
  "in"
  (identifier) @type)

; Sort names in transition declarations
(transition_declaration 
  (identifier) @type
  "~>"
  (identifier) @type)

; Regular identifiers - atoms and remaining identifiers
(atom) @variable

; String literals in rule names get special highlighting
(rule (string) @string.special)

; Error highlighting for malformed syntax  
(ERROR) @error