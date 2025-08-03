/**
 * @file Analysislang grammar for tree-sitter
 * @author Bram Vandenbogaerde <bram.vandenbogaerde@vub.be>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "analysislang",

  extras: $ => [
    /\s/,
    $.comment
  ],

  rules: {
    source_file: $ => repeat(seq($.declaration, ';')),

    declaration: $ => choice(
      $.syntax_block,
      $.rules_block,
      $.transition_declaration,
      $.rewrite_rule
    ),

    // Syntax block: syntax { ... }
    syntax_block: $ => seq(
      'syntax',
      '{',
      repeat(seq($.syntax_rule, ';')),
      '}'
    ),

    // Syntax rule: vars in Type ::= production | production
    syntax_rule: $ => seq(
      $.variable_list,
      'in',
      $.identifier,
      optional(seq(
        '::=',
        $.production_list
      ))
    ),

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

    // Terms can be atoms, functors, equalities, or transitions
    // Using precedence to handle operator precedence correctly
    term: $ => choice(
      $.equality,
      $.transition_term,
      $.primary_term
    ),

    primary_term: $ => choice(
      $.atom,
      $.functor
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

    transition_term: $ => prec.left(2, seq(
      $.primary_term,
      '~>',
      $.term
    )),

    // Comments start with % and continue to end of line
    comment: $ => token(seq('%', /.*/)),

    // Identifiers
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Strings in double quotes
    string: $ => /"([^"\\]|\\.)*"/
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
