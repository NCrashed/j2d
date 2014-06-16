// Written in D programming language
/**
*   Copyright: © 2014 Anton Gushcha
*   License: Subject to the terms of the MIT license, as written in the included LICENSE file.
*   Authors: NCrashed <ncrashed@gmail.com>
*
*   Describes java parser.
*/
module parser.java;

import pegged.grammar;
import pegged.tester.grammartester;

mixin(grammar(`
Java:
    Identifier <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
    Keyword <   
        "abstract" / "continue" / "for"        / "new"       / "switch"       / 
        "assert"   / "default"  / "if"         / "package"   / "synchronized" / 
        "boolean"  / "do"       / "goto"       / "private"   / "this"         / 
        "break"    / "double"   / "implements" / "protected" / "throw"        / 
        "byte"     / "else"     / "import"     / "public"    / "throws"       / 
        "case"     / "enum"     / "instanceof" / "return"    / "transient"    / 
        "catch"    / "extends"  / "int"        / "short"     / "try"          / 
        "char"     / "final"    / "interface"  / "static"    / "void"         / 
        "class"    / "finally"  / "long"       / "strictfp"  / "volatile"     / 
        "const"    / "float"    / "native"     / "super"     / "while"
    QualifiedIdentifier < Identifier ('.' Identifier)*
    QualifiedIdentifierList < QualifiedIdentifier (',' QualifiedIdentifier)*

    CompilationUnit < (Annotations? "package" QualifiedIdentifier ';')? ImportDeclaration* TypeDeclaration*
    ImportDeclaration < "import" StaticImport? Identifier ('.' Identifier)* ImportAll? ';'
    StaticImport <~ "static"
    ImportAll <~ ".*"
    TypeDeclaration < eps

    Annotations < Annotation (Annotation)*
    Annotation < '@' QualifiedIdentifier ('(' AnnotationElement* ')')?
    AnnotationElement < ElementValuePairs / ElementValue
    ElementValuePairs < ElementValuePair (',' ElementValuePair)*
    ElementValuePair  < Identifier '=' ElementValue
    ElementValue < Annotation / Expression1 / ElementValueArrayInitializer
    ElementValueArrayInitializer < (ElementValues? ','?)*
    ElementValues < ElementValue (',' ElementValue)*

    Expression1 < eps
`));

version(unittest)
{
    import std.algorithm;
    import std.functional;
    import std.exception;
    import std.typecons;
    import std.range;
    import std.stdio;
    import std.traits;
    
    alias Test = Tuple!(string, "tcase", string, "result", bool, "positive");
    
    void evaluate(R)(R range) if(isInputRange!R && isCallable!(ForeachType!R))
    {
        foreach(elem; range)
        {
            elem();
        }
    }
    
    auto test(T...)(GrammarTester!T tester, auto const ref Test test)
    {
        return 
        {
            if(test.positive)
                tester.assertSimilar(test.tcase, test.result);
            else
                tester.assertSimilar(test.tcase, test.result).assertThrown!Error;
        };
    }
    
    void runTests(string startSymbol)(Test[] tests)
    {
        auto tester = new GrammarTester!(Java, startSymbol);
        tests.map!(curry!(test, tester)).evaluate;
    }
}
unittest
{
    [
        Test(`foo123`, `QualifiedIdentifier -> Identifier`, true),
        Test(`1foo123`, `QualifiedIdentifier -> Identifier`, false),
        Test(`abstract`, `QualifiedIdentifier -> Identifier`, false),
        Test(`foo123.ahola`, `QualifiedIdentifier -> { Identifier Identifier }`, true),
    ].runTests!"QualifiedIdentifier";
    
    [
        Test(`foo123.ahola, foo.boo`, `
        QualifiedIdentifierList -> 
        {
            QualifiedIdentifier -> { Identifier Identifier }
            QualifiedIdentifier -> { Identifier Identifier }
        }`
        , true)
    ].runTests!"QualifiedIdentifierList";
    
    [
        Test(q{
            package net.mypackage;
            
            import com.google.common.base.Stopwatch;
            import com.google.common.collect.Queues;
            import com.google.common.collect.Sets; 
            
            import static java.lang.Math.*;
        },
        `
            CompilationUnit ->
            {
                QualifiedIdentifier -> { Identifier Identifier }
                
                ImportDeclaration -> {Identifier Identifier Identifier Identifier Identifier}
                ImportDeclaration -> {Identifier Identifier Identifier Identifier Identifier}
                ImportDeclaration -> {Identifier Identifier Identifier Identifier Identifier}
    
                ImportDeclaration -> {StaticImport Identifier Identifier Identifier ImportAll}
            }
        `, true)
    ].runTests!"CompilationUnit";
    
    [
        Test(q{@StartObject}, `Annotation -> QualifiedIdentifier -> Identifier`, true),
        Test(q{@StartObject()}, `Annotation -> QualifiedIdentifier -> Identifier`, true),
    ].runTests!"Annotation";
}