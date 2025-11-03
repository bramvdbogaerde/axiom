/**
 * @file Axiom grammar for tree-sitter
 * @author Bram Vandenbogaerde <bram.vandenbogaerde@vub.be>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "axiom",

  extras: $ => [
    /\s/,
    $.regular_comment
  ],

  rules: {

    source_file: $ => repeat(choice(
      seq($.declaration, ';'),
      $.haskell_block,
      $.test_comment
    )),

    declaration: $ => choice(
      $.syntax_block,
      $.rules_block,
      $.latex_block,
      $.transition_declaration,
      $.rewrite_rule,
      $.rewrite_type
    ),

    // Syntax block: syntax { ... }
    syntax_block: $ => seq(
      'syntax',
      '{',
      repeat(seq($.syntax_rule, ';')),
      '}'
    ),

    // Syntax rule: vars in Type ::= production | production
    // OR: alias TypeRef as AliasName
    syntax_rule: $ => choice(
      $.type_alias,
      seq(
        $.variable_list,
        'in',
        $.type_ref,
        optional(seq(
          '::=',
          $.production_list
        ))
      )
    ),

    // Latex rule block
    latex_block: $ => seq('latex', '{', repeat(seq($.latex_rule, ';')), '}'),
    latex_rule: $ => seq($.term, '=>', sep(choice($.identifier, $.string), ",")),

    // A rewrite type: ': functor(arg1) => ret"
    rewrite_type: $ => seq(':', $.term, '=>', $.term),
  
    // Type alias: alias TypeRef as AliasName
    type_alias: $ => seq(
      'alias',
      field('original_type', $.type_ref),
      'as',
      field('alias_name', $.type_name)
    ),

    // Type reference (can be simple or parameterized)
    type_ref: $ => choice(
      $.type_name,
      seq(
        $.type_name,
        '(',
        sep1($.type_ref, ','),
        ')'
      ),
      $.haskell_expression
    ),

    // Type name
    type_name: $ => $.identifier,

    variable_list: $ => sep1($.identifier, ','),

    production_list: $ => sep1($.term, '|'),

    // Rules block: rules { ... }
    rules_block: $ => seq(
      'rules',
      '{',
      repeat(seq($.rule, ';')),
      '}'
    ),

    // Rule: rule "name" [preconditions] => [consequences]
    rule: $ => seq(
      'rule',
      $.string,
      '[',
      optional($.term_list),
      ']',
      '=>',
      '[',
      optional($.term_list),
      ']'
    ),

    term_list: $ => sep1($.term, ';'),

    // Transition declaration: transition Type ~> Type
    transition_declaration: $ => seq(
      'transition',
      $.identifier,
      '~>',
      $.identifier
    ),

    // Rewrite rule: name(args) = body
    rewrite_rule: $ => seq(
      $.identifier,
      '(',
      sep($.term, ','),
      ')',
      '=',
      $.term
    ),

    // Terms can be atoms, functors, equalities, inequalities, or transitions
    // Using precedence to handle operator precedence correctly
    term: $ => choice(
      $.equality,
      $.inequality,
      $.transition_term,
      $.primary_term
    ),

    primary_term: $ => choice(
      $.atom,
      $.functor,
      $.haskell_expression,
      $.integer_literal,
      $.wildcard,
      $.big_step_expr,
      seq(optional($.term), "[", sep(seq($.identifier, "|->", $.term), ","), "]") 
    ),

    atom: $ => $.identifier,

    functor: $ => seq(
      $.identifier,
      '(',
      sep($.term, ','),
      ')'
    ),

    equality: $ => prec.left(1, seq(
      $.primary_term,
      '=',
      $.term
    )),

    inequality: $ => prec.left(1, seq(
      $.primary_term,
      '/=',
      $.term
    )),

    transition_term: $ => prec.left(2, seq(
      $.primary_term,
      '~>',
      $.term
    )),

    // Wildcard: _
    wildcard: $ => '_',

    // Big step evaluation: (terms) ⇓ (terms)
    big_step_expr: $ => seq(
      '(',
      optional(sep1($.term, ',')),
      ')',
      '⇓',
      '(',
      optional(sep1($.term, ',')),
      ')'
    ),

    // Test comments: %test: <term>
    test_comment: $ => prec(1, seq(
      '%test:',
      $.term
    )),

    // Regular comments start with % and continue to end of line
    regular_comment: $ => token(prec(-1, seq('%', /.*/))),

    // Haskell expressions: ${...}
    haskell_expression: $ => token(seq(
      '${',
      repeat(/[^}]/),
      '}'
    )),

    // Haskell blocks: {{{ ... }}}
    haskell_block: $ => choice(hblock('{{', '}}'), hblock('[[', ']]')),
    
    // Integer literals
    integer_literal: $ => /[0-9]+/,

    // Identifiers
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Strings in double quotes
    string: $ => /"([^"\\]|\\.)*"/,
  }
});

// Helper function for comma-separated lists with at least one element
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

// Helper function for comma-separated lists (can be empty)
function sep(rule, separator) {
  return optional(sep1(rule, separator));
}

function hblock(start, end) {
  return token(seq(
      start,
      repeat(choice(
        /[^}]/,
        seq('}', /[^}]/),
        seq('}', '}', /[^}]/)
      )),
      end
    ));
}
