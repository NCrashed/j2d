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
    # Spacing and Comments
    #============================================================

    Spacing <- :(' ' / '\t' / '\r' / '\n' / '\r\n' / Comment)*
    Comment <- BlockComment
             / LineComment
    
    BlockComment <~ :'/*' (!'*/' .)* :'*/'
    LineComment <~ :'//' (!endOfLine .)* :endOfLine

    # Identifiers
    #============================================================

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

    # Literals
    #============================================================
    Literal <- FloatingPointLiteral / IntegerLiteral / BooleanLiteral / CharacterLiteral / StringLiteral / NullLiteral
    Underscores <~ '_'+

    # Integer literals

    IntegerLiteral <- HexIntegerLiteral / OctalIntegerLiteral / BinaryIntegerLiteral / DecimalIntegerLiteral
    DecimalIntegerLiteral <- DecimalNumeral IntegerTypeSuffix?
    HexIntegerLiteral <- HexNumeral IntegerTypeSuffix?
    OctalIntegerLiteral <- OctalNumeral IntegerTypeSuffix?
    BinaryIntegerLiteral <- BinaryNumeral IntegerTypeSuffix?
    IntegerTypeSuffix <- 'l' / 'L'

    DecimalNumeral <~ '0' / (NonZeroDigit Digits?) / (NonZeroDigit Underscores Digits)
    Digits <~ digit (digit / '_')*
    NonZeroDigit <- [1-9]

    HexNumeral <~ ("0x" / "0X") HexDigits
    HexDigits <~ HexDigit (HexDigit / '_')*
    HexDigit <~ [0-9a-fA-F]

    OctalNumeral <~ '0' OctalDigits / '0' Underscores OctalDigits
    OctalDigits <~ OctalDigit (OctalDigit / '_')*
    OctalDigit <~ [0-7]

    BinaryNumeral <~ ("0b"/ "0B") BinaryDigits
    BinaryDigits <~ BinaryDigit (BinaryDigit / '_')*
    BinaryDigit <~ [01]

    # Floating point literals
    FloatingPointLiteral < DecimalFloatingPointLiteral / HexadecimalFloatingPointLiteral
    
    DecimalFloatingPointLiteral <~ 
        Digits '.' Digits? ExponentPart? FloatTypeSuffix? /
        '.' Digits ExponentPart? FloatTypeSuffix? /
        Digits ExponentPart FloatTypeSuffix? /
        Digits ExponentPart? FloatTypeSuffix
    
    ExponentPart <~ ExponentIndicator SignedInteger
    ExponentIndicator <~ [eE]
    
    SignedInteger <~ Sign? Digits
    Sign <~ '+' / '-'
    
    FloatTypeSuffix <~ [fFdD]
    
    HexadecimalFloatingPointLiteral <~ HexSignificand BinaryExponent FloatTypeSuffix?
    HexSignificand <~ HexNumeral '.'? / ("0x" / "0X") HexDigits? '.' HexDigits
    
    BinaryExponent <~ BinaryExponentIndicator SignedInteger
    BinaryExponentIndicator <~ [pP]

    # Boolean literal
    BooleanLiteral <~ "true" / "false"
    
    # Character literal
    CharacterLiteral <~ quote (!quote (EscapeSequence / .)) quote
    
    # String literal
    StringLiteral <~ doublequote (JChar)* doublequote
    JChar <- EscapeSequence / !doublequote .
    EscapeSequence <- backslash ( quote
                                / doublequote
                                / backslash
                                / [bfnrtv]
                                / 'u'+ HexDigit HexDigit HexDigit HexDigit
                                / OctalEscape
                                )
    OctalEscape <~ [0-3] OctalDigit OctalDigit / OctalDigit OctalDigit / OctalDigit 

    # Null literal
    NullLiteral <~ "null"

    # Compilation Unit
    #============================================================

    CompilationUnit < (Annotations? "package" QualifiedIdentifier ';')? ImportDeclaration* TypeDeclaration*
    ImportDeclaration < "import" StaticImport? Identifier ('.' Identifier)* ImportAll? ';'
    StaticImport <~ "static"
    ImportAll <~ ".*"

    # Annotations
    #============================================================
    Annotations < Annotation (Annotation)*
    Annotation < '@' QualifiedIdentifier ('(' AnnotationElement* ')')?
    AnnotationElement < ElementValuePairs / ElementValue
    ElementValuePairs < ElementValuePair (',' ElementValuePair)*
    ElementValuePair  < Identifier '=' ElementValue
    ElementValue < Annotation / Expression1 / ElementValueArrayInitializer
    ElementValueArrayInitializer < (ElementValues? ','?)*
    ElementValues < ElementValue (',' ElementValue)*

    AnnotationTypeBody < '{' AnnotationTypeElementDeclaration* '}'

    AnnotationTypeElementDeclaration < Modifier* AnnotationTypeElementRest
    AnnotationTypeElementRest < 
        / Type Identifier AnnotationMethodOrConstantRest ';'
        / ClassDeclaration
        / InterfaceDeclaration
        / EnumDeclaration
        / AnnotationTypeDeclaration

    AnnotationMethodOrConstantRest <
        / AnnotationMethodRest
        / ConstantDeclaratorsRest

    AnnotationMethodRest < '(' ')' SquareParens? ("default" ElementValue)?

    # Types
    #============================================================
    TypeDeclaration < ClassOrInterfaceDeclaration / ';'
    ClassOrInterfaceDeclaration < Modifier* (ClassDeclaration / InterfaceDeclaration)

    Type < BasicType SquareParens* / ReferenceType SquareParens*
    SquareParens < '[' ']'
    BasicType <- "byte" / "short" / "char" / "int" / "long" / "float" / "double" / "boolean"
    ReferenceType < Identifier TypeArguments? ('.' Identifier TypeArguments?)*
    TypeArguments < '<' TypeArgument (',' TypeArgument)* '>'
    TypeArgument < ReferenceType / '?' (("extends" / "super") ReferenceType)?

    NonWildcardTypeArguments < '<' TypeList '>'
    TypeList < ReferenceType (',' ReferenceType)*
    
    TypeArgumentsOrDiamond < '<' '>' / TypeArguments
    NonWildcardTypeArgumentsOrDiamond < '<' '>' / NonWildcardTypeArguments

    TypeParameters < '<' TypeParameter (',' TypeParameter)* '>'
    TypeParameter < Identifier ("extends" Bound)?
    Bound < ReferenceType ('&' ReferenceType)?

    Modifier <- 
        / Annotation 
        / "public" 
        / "protected" 
        / "private" 
        / "static" 
        / "abstract" 
        / "final" 
        / "native" 
        / "synchronized" 
        / "trasient" 
        / "volatile" 
        / "strictfp"
    
    # Classes
    #============================================================
    ClassDeclaration < NormalClassDeclaration / EnumDeclaration
    NormalClassDeclaration < "class" Identifier TypeParameters? ExtendBody? ImplementBody? ClassBody
    ExtendBody < "extends" Type
    ImplementBody < "implements" TypeList
    ClassBody < '{' ClassBodyDeclaration* '}'
    ClassBodyDeclaration < ';' / Modifier* MemberDecl / "static"? Block

    MemberDecl < 
        / MethodOrFieldDecl
        / "void" Identifier VoidMethodDeclaratorRest
        / Identifier ConstructorDeclaratorRest
        / GenericMethodOrConstructorDecl
        / ClassDeclaration
        / InterfaceDeclaration

    MethodOrFieldDecl < Type Identifier MethodOrFieldRest
    MethodOrFieldRest < FieldDeclaratorsRest ';' / MethodDeclaratorRest
    FieldDeclaratorsRest < VariableDeclaratorRest (',' VariableDeclarator)*
    MethodDeclaratorRest < FormalParameters SquareParens* ("throws" QualifiedIdentifierList)? (Block / ';')
    VoidMethodDeclaratorRest < FormalParameters ("throws" QualifiedIdentifierList)? (Block / ';')
    ConstructorDeclaratorRest < FormalParameters ("throws" QualifiedIdentifierList)? Block
    
    GenericMethodOrConstructorDecl < TypeParameters GenericMethodOrConstructorRest
    GenericMethodOrConstructorRest < 
        / (Type / "void") Identifier MethodDeclaratorRest
        / Identifier ConstructorDeclaratorRest
    
    # Enums
    #============================================================
    EnumDeclaration < "enum" Identifier ("implements" TypeList)? EnumBody
    EnumBody < eps

    # Interfaces
    #============================================================
    InterfaceDeclaration < NormalInterfaceDeclaration / AnnotationTypeDeclaration
    NormalInterfaceDeclaration < "interface" Identifier TypeParameters? ("extends" TypeList)? InterfaceBody
    AnnotationTypeDeclaration < '@' "interface" Identifier AnnotationTypeBody
    
    InterfaceBody < '{' InterfaceBodyDeclaration* '}'
    InterfaceBodyDeclaration < Modifier* InterfaceMemberDecl / ';'
    
    InterfaceMemberDecl < 
        / InterfaceMethodOrFieldDecl
        / "void" Identifier VoidInterfaceMethodDeclaratorRest
        / InterfaceGenericMethodDecl
        / ClassDeclaration
        / InterfaceDeclaration
    
    InterfaceMethodOrFieldDecl < Type Identifier InterfaceMethodOrFieldRest
    InterfaceMethodOrFieldRest < ConstantDeclaratorsRest ';' / InterfaceMethodDeclaratorRest
    ConstantDeclaratorsRest < ConstantDeclaratorRest (',' ConstantDeclarator)*
    ConstantDeclaratorRest < SquareParens* '=' VariableInitializer
    ConstantDeclarator < Identifier ConstantDeclaratorRest
    
    InterfaceMethodDeclaratorRest < FormalParameters SquareParens* ("throws" QualifiedIdentifierList)? ';'
    VoidInterfaceMethodDeclaratorRest < FormalParameters ("throws" QualifiedIdentifierList)? ';'
    InterfaceGenericMethodDecl < TypeParameters (Type / "void") Identifier InterfaceMethodDeclaratorRest

    # Variables and parameters
    #============================================================
    FormalParameters < '(' FormalParameterDecls? ')'
    FormalParameterDecls < VariableModifier* Type FormalParameterDeclsRest
    VariableModifier <- "final" / Annotation
    FormalParameterDeclsRest < VariableDeclaratorId (',' FormalParameterDecls)? / "..." VariableDeclaratorId
    VariableDeclaratorId < Identifier SquareParens*

    VariableDeclarators < VariableDeclarator (',' VariableDeclarator)*
    VariableDeclarator < Identifier VariableDeclaratorRest
    VariableDeclaratorRest < SquareParens* ('=' VariableInitializer)?

    VariableInitializer <- Expression / ArrayInitializer
    ArrayInitializer < ('[' VariableInitializer (',' VariableInitializer)* ','? ']')*

    # Expression
    #============================================================
    Expression1 < eps
    Expression < eps

    # Block
    #============================================================
    Block < eps
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
            /**
            *   My awesome package
            */
            package net.mypackage;
            
            // imports
            import com.google.common.base.Stopwatch;
            import com.google.common.collect.Queues;
            import com.google.common.collect.Sets; 
            
            /* I like comments */
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
    
    // lets test literals
    [
        // integer
        Test(`0`, `Literal -> IntegerLiteral -> DecimalIntegerLiteral -> DecimalNumeral`, true),
        Test(`0x7fff_ffff`, `Literal -> IntegerLiteral -> HexIntegerLiteral -> HexNumeral`, true),
        Test(`0177_7777_7777`, `Literal -> IntegerLiteral -> OctalIntegerLiteral -> OctalNumeral`, true),
        Test(`0b0111_1111_1111_1111_1111_1111_1111_1111`, `Literal -> IntegerLiteral -> BinaryIntegerLiteral -> BinaryNumeral`, true),
        
        Test(`0x8000_0000`, `Literal -> IntegerLiteral -> HexIntegerLiteral -> HexNumeral`, true),
        Test(`0200_0000_0000`, `Literal -> IntegerLiteral -> OctalIntegerLiteral -> OctalNumeral`, true),
        Test(`0b1000_0000_0000_0000_0000_0000_0000_0000`, `Literal -> IntegerLiteral -> BinaryIntegerLiteral -> BinaryNumeral`, true),
        
        Test(`0xffff_ffff`, `Literal -> IntegerLiteral -> HexIntegerLiteral -> HexNumeral`, true),
        Test(`0377_7777_7777`, `Literal -> IntegerLiteral -> OctalIntegerLiteral -> OctalNumeral`, true),
        Test(`0b1111_1111_1111_1111_1111_1111_1111_1111`, `Literal -> IntegerLiteral -> BinaryIntegerLiteral -> BinaryNumeral`, true),
        
        Test(`9223372036854775808L`, `Literal -> IntegerLiteral -> DecimalIntegerLiteral -> { DecimalNumeral IntegerTypeSuffix }`, true),
        Test(`0l`, `Literal -> IntegerLiteral -> DecimalIntegerLiteral -> { DecimalNumeral IntegerTypeSuffix }`, true),
        Test(`0b111`, `Literal -> IntegerLiteral -> BinaryIntegerLiteral -> BinaryNumeral`, true),
        Test(`0b111l`, `Literal -> IntegerLiteral -> BinaryIntegerLiteral -> { BinaryNumeral IntegerTypeSuffix }`, true),
        Test(`123`, `Literal -> IntegerLiteral -> DecimalIntegerLiteral -> DecimalNumeral`, true),
        Test(`0123`, `Literal -> IntegerLiteral -> OctalIntegerLiteral -> OctalNumeral`, true),
        Test(`0123L`, `Literal -> IntegerLiteral -> OctalIntegerLiteral -> { OctalNumeral IntegerTypeSuffix }`, true),
        Test(`0X123`, `Literal -> IntegerLiteral -> HexIntegerLiteral -> HexNumeral`, true),
        Test(`0X123l`, `Literal -> IntegerLiteral -> HexIntegerLiteral -> { HexNumeral IntegerTypeSuffix }`, true),
        
        // floating point
        Test(`1e1f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`2.f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`.3f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`0f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`3.14f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`6.022137e+23f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        
        Test(`1e1`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`2.`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`.3`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`0.0`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`3.14`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`1e-9d`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`1e137`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        
        Test(`3.4028235e38f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`1.40e-45f`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`1.7976931348623157e308`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        Test(`4.9e-324`, `Literal -> FloatingPointLiteral -> DecimalFloatingPointLiteral`, true),
        
        // boolean
        Test(`true`, `Literal -> BooleanLiteral`, true),
        Test(`false`, `Literal -> BooleanLiteral`, true),
        
        // character
        Test(q{'a'}, `Literal -> CharacterLiteral`, true),
        Test(q{'%'}, `Literal -> CharacterLiteral`, true),
        Test(q{'\t'}, `Literal -> CharacterLiteral`, true),
        Test(q{'\\'}, `Literal -> CharacterLiteral`, true),
        Test(q{'\''}, `Literal -> CharacterLiteral`, true),
        Test(q{'\u03a9'}, `Literal -> CharacterLiteral`, true),
        Test(`'\uFFFF'`, `Literal -> CharacterLiteral`, true),
        Test(q{'\177'}, `Literal -> CharacterLiteral`, true),
        // Test(q{'Я'}, `Literal -> CharacterLiteral`, true),
        // Test(q{'Ω'}, `Literal -> CharacterLiteral`, true), uncomment when pegged starts supporting dstrings
        
        // string
        Test(q{""}, `Literal -> StringLiteral`, true),
        Test(q{"Hello"}, `Literal -> StringLiteral`, true),
        Test(q{"\t\n"}, `Literal -> StringLiteral`, true),
        Test(q{"Привет"}, `Literal -> StringLiteral`, true),
        
        // null
        Test("null", `Literal -> NullLiteral`, true),
    ].runTests!"Literal";
    
    // classes
    [
        Test(q{class A {}}, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                ClassBody
            }
        `,
        true),
        Test(q{class B <T> extends A implements C {}}, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                TypeParameters -> TypeParameter -> Identifier
                ExtendBody -> Type -> ReferenceType -> Identifier
                ImplementBody -> TypeList -> ReferenceType -> Identifier
                ClassBody
            }
        `,
        true),
        Test(q{
            class A {
               String field1;
            }
        }, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                ClassBody -> ClassBodyDeclaration -> MemberDecl -> MethodOrFieldDecl ->
                {
                    Type -> ReferenceType -> Identifier
                    Identifier
                    MethodOrFieldRest
                }
            }
        `,
        true),
        Test(q{
            class A {
               int foo(boolean param)
            }
        }, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                ClassBody -> ClassBodyDeclaration -> MemberDecl -> MethodOrFieldDecl ->
                {
                    Type -> BasicType
                    Identifier
                    MethodOrFieldRest -> MethodDeclaratorRest ->
                    {
                        FormalParameters -> FormalParameterDecls ->
                        {
                            Type -> BasicType
                            FormalParameterDeclsRest -> VariableDeclaratorId -> Identifier
                        }
                        Block
                    }
                }
            }
        `,
        true),
        Test(q{
            class A {
               A(boolean param)
            }
        }, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                ClassBody -> ClassBodyDeclaration -> MemberDecl ->
                {
                    Identifier
                    ConstructorDeclaratorRest ->
                    {
                        FormalParameters -> FormalParameterDecls ->
                        {
                            Type -> BasicType
                            FormalParameterDeclsRest -> VariableDeclaratorId -> Identifier
                        }
                        Block
                    }
                }
            }
        `,
        true),
        Test(q{
            class A {
               <T> A(boolean param)
            }
        }, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                ClassBody -> ClassBodyDeclaration -> MemberDecl ->
                {
                    GenericMethodOrConstructorDecl -> 
                    {
                        TypeParameters -> TypeParameter -> Identifier
                        GenericMethodOrConstructorRest ->
                        {
                            Identifier
                            ConstructorDeclaratorRest ->
                            {
                                FormalParameters -> FormalParameterDecls ->
                                {
                                    Type -> BasicType
                                    FormalParameterDeclsRest -> VariableDeclaratorId -> Identifier
                                }
                                Block
                            }
                        }
                    }
                }
            }
        `,
        true),
        Test(q{
            class A {
               <T extends C & I> int foo(boolean param)
            }
        }, `
            ClassDeclaration -> NormalClassDeclaration ->
            {
                Identifier
                ClassBody -> ClassBodyDeclaration -> MemberDecl -> GenericMethodOrConstructorDecl ->
                {
                    TypeParameters -> TypeParameter ->
                    {
                        Identifier
                        Bound ->
                        {
                             ReferenceType -> Identifier
                             ReferenceType -> Identifier
                        }
                    }
                    GenericMethodOrConstructorRest -> 
                    {
                        Type -> BasicType
                        Identifier
                        MethodDeclaratorRest->
                        {
                            FormalParameters -> FormalParameterDecls ->
                            {
                                Type -> BasicType
                                FormalParameterDeclsRest -> VariableDeclaratorId -> Identifier
                            }
                            Block
                        }
                    }
                }
            }
        `,
        true),
    ].runTests!"ClassDeclaration";
    
    // interfaces
    [
        Test(q{interface A {}}, `
            InterfaceDeclaration -> NormalInterfaceDeclaration ->
            {
                Identifier
                InterfaceBody
            }
        `,
        true),
        Test(q{interface B <T> extends A {}}, `
            InterfaceDeclaration -> NormalInterfaceDeclaration ->
            {
                Identifier
                TypeParameters -> TypeParameter -> Identifier
                TypeList -> ReferenceType -> Identifier
                InterfaceBody
            }
        `,
        true),
        Test(q{
            interface A {
               String field1 = ;
            }
        }, `
            InterfaceDeclaration -> NormalInterfaceDeclaration ->
            {
                Identifier
                InterfaceBody -> InterfaceBodyDeclaration -> InterfaceMemberDecl -> InterfaceMethodOrFieldDecl ->
                {
                    Type -> ReferenceType -> Identifier
                    Identifier
                    InterfaceMethodOrFieldRest -> ConstantDeclaratorsRest -> ConstantDeclaratorRest -> VariableInitializer -> Expression
                }
            }
        `,
        true),
        Test(q{
            interface A {
               int foo(boolean param);
            }
        }, `
            InterfaceDeclaration -> NormalInterfaceDeclaration ->
            {
                Identifier
                InterfaceBody -> InterfaceBodyDeclaration -> InterfaceMemberDecl -> InterfaceMethodOrFieldDecl ->
                {
                    Type -> BasicType
                    Identifier
                    InterfaceMethodOrFieldRest -> InterfaceMethodDeclaratorRest ->
                    {
                        FormalParameters -> FormalParameterDecls ->
                        {
                            Type -> BasicType
                            FormalParameterDeclsRest -> VariableDeclaratorId -> Identifier
                        }
                    }
                }
            }
        `,
        true),
        Test(q{
            interface A {
               <T extends C & I> int foo(boolean param);
            }
        }, `
            InterfaceDeclaration -> NormalInterfaceDeclaration ->
            {
                Identifier
                InterfaceBody -> InterfaceBodyDeclaration -> InterfaceMemberDecl -> InterfaceGenericMethodDecl ->
                {
                    TypeParameters -> TypeParameter ->
                    {
                        Identifier
                        Bound ->
                        {
                             ReferenceType -> Identifier
                             ReferenceType -> Identifier
                        }
                    }
                    Type -> BasicType
                    Identifier
                    InterfaceMethodDeclaratorRest->
                    {
                        FormalParameters -> FormalParameterDecls ->
                        {
                            Type -> BasicType
                            FormalParameterDeclsRest -> VariableDeclaratorId -> Identifier
                        }
                    }
                }
            }
        `,
        true),
    ].runTests!"InterfaceDeclaration";
    
    // annotation declaration
    [
        Test(q{
            package test;
             
            import java.lang.annotation.ElementType;
            import java.lang.annotation.Retention;
            import java.lang.annotation.RetentionPolicy;
            import java.lang.annotation.Target;
             
            //@Retention(RetentionPolicy.RUNTIME)
            //@Target(ElementType.METHOD) //can use in method only.
            public @interface Test 
            {
                public boolean enabled() default /*true*/;                
            }
        }, `
            CompilationUnit ->
            {
                QualifiedIdentifier -> Identifier
                ImportDeclaration -> { Identifier Identifier Identifier Identifier }
                ImportDeclaration -> { Identifier Identifier Identifier Identifier }
                ImportDeclaration -> { Identifier Identifier Identifier Identifier }
                ImportDeclaration -> { Identifier Identifier Identifier Identifier }

                TypeDeclaration -> ClassOrInterfaceDeclaration -> 
                {
                    Modifier
                    InterfaceDeclaration -> AnnotationTypeDeclaration -> 
                    {
                        Identifier
                        AnnotationTypeBody -> AnnotationTypeElementDeclaration ->
                        {
                            Modifier
                            AnnotationTypeElementRest -> 
                            {
                                Type -> BasicType
                                Identifier
                                AnnotationMethodOrConstantRest -> AnnotationMethodRest -> ElementValue -> Expression1
                            }
                        }
                    }
                }
            }
        `,
        true),
    ].runTests!"CompilationUnit";
}