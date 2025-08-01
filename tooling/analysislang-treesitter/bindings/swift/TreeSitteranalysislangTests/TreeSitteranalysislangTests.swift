import XCTest
import SwiftTreeSitter
import TreeSitterAnalysislang

final class TreeSitterAnalysislangTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_analysislang())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading AnalysisLang grammar")
    }
}
