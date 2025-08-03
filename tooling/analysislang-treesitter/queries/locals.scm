; Define scopes
(syntax_block) @local.scope
(rules_block) @local.scope
(rewrite_rule) @local.scope

; Variable definitions in syntax rules
(syntax_rule 
  (variable_list (identifier) @local.definition.variable))

; Variable references
(atom (identifier) @local.reference)

; Function definitions (rewrite rules)
(rewrite_rule (identifier) @local.definition.function)

; Function references (functors)
(functor (identifier) @local.reference)