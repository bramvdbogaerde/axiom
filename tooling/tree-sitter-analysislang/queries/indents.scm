; Increase indent for content inside syntax blocks

(syntax_block
  "{" @indent @extend
  "}" @outdent)

(rules_block
  "{" @indent @extend
  "}" @outdent)

(rule
  "[" @indent @extend
  (term_list
    (term) @anchor (term)* @align)
  "]" @outdent)

