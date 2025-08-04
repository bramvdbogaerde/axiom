#!/usr/bin/env python3
import xml.etree.ElementTree as ET
import sys
import os

def main():
    # Read test output
    try:
        with open('test-results/test-output.txt', 'r') as f:
            content = f.read()
    except FileNotFoundError:
        print("Error: test-results/test-output.txt not found")
        sys.exit(1)

    # Parse test results
    failures = content.count('FAIL')
    passes = content.count('✔')
    total_tests = passes + failures
    errors = 0

    # Create XML structure
    testsuites = ET.Element('testsuites')
    testsuite = ET.SubElement(testsuites, 'testsuite', {
        'name': 'analysislang-tests', 
        'tests': str(total_tests),
        'failures': str(failures),
        'errors': str(errors),
        'time': '0.0'
    })

    # Add a summary test case
    testcase = ET.SubElement(testsuite, 'testcase', {
        'classname': 'TestSuite',
        'name': f'{total_tests} tests run',
        'time': '0.0'
    })

    if failures > 0:
        failure = ET.SubElement(testcase, 'failure', {'message': f'{failures} test(s) failed'})
        failure.text = f'See test output for details. {failures} failures out of {total_tests} tests.'

    # Write XML file
    tree = ET.ElementTree(testsuites)
    ET.indent(tree, space='  ')
    tree.write('test-results/results.xml', encoding='utf-8', xml_declaration=True)
    
    print(f"Generated JUnit XML report: {total_tests} tests, {failures} failures")

if __name__ == '__main__':
    main()