; Highlight regular comments as comments
((regular_comment) @injection.content
 (#set! injection.language "comment"))

; Test comments don't need special injection since they contain normal terms

; Inject Haskell syntax highlighting into Haskell expressions
((haskell_expression) @injection.content
 (#set! injection.language "haskell"))

; Inject Haskell syntax highlighting into Haskell blocks  
((haskell_block) @injection.content
 (#set! injection.language "haskell"))
