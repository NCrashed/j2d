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
`));

version(unittest)
{
    import std.exception;
}
unittest
{
    auto javaTester = new GrammarTester!(Java, "Identifier");
    javaTester.assertSimilar(`foo123`, `Identifier`);
    javaTester.assertSimilar(`abstract`, `Identifier`).assertThrown!Error;
}