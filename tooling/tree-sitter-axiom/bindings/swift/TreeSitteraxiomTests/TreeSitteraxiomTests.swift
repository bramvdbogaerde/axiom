import XCTest
import SwiftTreeSitter
import TreeSitterAxiom

final class TreeSitterAxiomTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_axiom())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading axiom grammar")
    }
}
