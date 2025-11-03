; Keywords
"syntax" @keyword
"rules" @keyword
"rule" @keyword
"transition" @keyword
"in" @keyword
"alias" @keyword
"as" @keyword
"latex" @keyword

; Operators
"::=" @operator
"=>" @operator
"=" @operator
"/=" @operator
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
(regular_comment) @comment

; Test comments - highlight the prefix and terms separately
(test_comment 
  "%test:" @comment.special)

; Haskell expressions and blocks
(haskell_expression) @string.special.haskell
(haskell_block) @string.special.haskell 

; Terms in test comments should be highlighted normally
(test_comment (term) @none)

; Function calls (functors) - highlight the function name first
(functor (identifier) @function)

; Type alias declarations
(type_alias
  original_type: (type_ref (type_name) @type)
  alias_name: (type_name) @type)

; Type references in parameterized types
(type_ref (type_name) @type)

; Sort names in syntax declarations - the type_ref after "in"
(syntax_rule
  (variable_list)
  "in"
  (type_ref (type_name) @type))

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
