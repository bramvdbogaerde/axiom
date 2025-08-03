package tree_sitter_analysislang_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_analysislang "github.com/tree-sitter/tree-sitter-analysislang/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_analysislang.Language())
	if language == nil {
		t.Errorf("Error loading AnalysisLang grammar")
	}
}
