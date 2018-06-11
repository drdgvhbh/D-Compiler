module Lexer.Reserved where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Lexer.Parser
import qualified Text.Megaparsec.Char.Lexer as L

reservedWord :: String -> Parser ()
reservedWord word = (lexeme . try)(string word >> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try)(p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reservedWords
                    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                    else return x

reservedWords :: [String]
reservedWords = [
        "abstract",
        "alias",
        "align",
        "asm",
        "auto",

        "body",
        "bool",
        "break",
        "byte",

        "case",
        "cast",
        "catch",
        "cdouble",
        "cent",
        "cfloat",
        "char",
        "class",
        "const",
        "continue",
        "creal",

        "dchar",
        "debug",
        "default",
        "delegate",
        "delete",
        "deprecated",
        "do",
        "double",

        "else",
        "enum",
        "export",
        "extern",

        "false",
        "final",
        "finally",
        "float",
        "for",
        "foreach",
        "foreach_reverse",
        "function",

        "goto",

        "idouble",
        "if",
        "ifloat",
        "immutable",
        "import",
        "in",
        "inout",
        "int",
        "interface",
        "invariant",
        "ireal",
        "is",

        "lazy",
        "long",

        "macro",
        "mixin",
        "module",

        "new",
        "nothrow",
        "null",

        "out",
        "override",

        "package",
        "pragma",
        "private",
        "protected",
        "public",
        "pure",

        "real",
        "ref",
        "return",

        "scope",
        "shared",
        "short",
        "static",
        "struct",
        "super",
        "switch",
        "synchronized",
        
        "template",
        "this",
        "throw",
        "true",
        "try",
        "typedef",
        "typeid",
        "typeof",

        "ubyte",
        "ucent",
        "uint",
        "ulong",
        "union",
        "unittest",
        "ushort",

        "version",
        "void",

        "wchar",
        "while",
        "with",

        "__FILE__",
        "__FILE_FULL_PATH__",
        "__MODULE__",
        "__LINE__",
        "__FUNCTION__",
        "__PRETTY_FUNCTION__",

        "__gshared",
        "__traits",
        "__vector",
        "__parameters"
    ]