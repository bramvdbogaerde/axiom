/**
 * @file Analysislang grammar for tree-sitter
 * @author Bram Vandenbogaerde <bram.vandenbogaerde@vub.be>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

function sepBy(rule, sep) {
  return choice(
    repeat(seq(rule, sep)),
    sep
  )
}

module.exports = grammar({
  name: "analysislang",

  extras: $ => [
    /\s/,
    '\n',
    '\t',
    $.comment
  ],

  rules: {
    // keyword: $ => choice('syntax', 'rules', 'rule', '::=', 'in'),
    source_file: $ => repeat1($.section),    section: $ => seq(choice(
      alias(seq('syntax', '{', optional($.syntax), '}'), $.syntaxblock),
      $.rewrite,
      seq('rules', '{', $.rules, '}')
    ), token(';')),

    rewrite: $ => seq(
      $.identifier,
      token('('),
      sepBy($.production, ','),
      token(')'),
      token('='),
      $.production,
    ),
    rules: $ => repeat1($.ruleRule),
    ruleRule: $ => seq(
        "rule",
        $.string,
        "[",
        $.production,
        "]",
        "=>",
        "[",
        $.production,
        "]",
        ";"
    ),
        
    syntax: $ => repeat1($.syntaxRule),
    syntaxRule: $ => seq(
      sepBy($.identifier, token(',')),
      token('in'),
      $.identifier,
      optional(seq(
        token('::='),
        sepBy($.production, token('|'))
      )),
      token(';')
    ),
    production: $ => choice(
        $.identifier,
        seq($.identifier, '(', sepBy($.identifier, ','), ')')
      ),
    comment: $ => token(/%.*\n/),
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    string: _ => /"([^"\\]|\\.)*"/
  }
});
