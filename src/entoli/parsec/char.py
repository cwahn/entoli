from calendar import c
from typing import Callable, Iterable, TypeVar
from entoli.base.maybe import Just, Maybe, Nothing
from entoli.parsec.prim import (
    ParsecT,
    SourcePos,
    SysUnExpect,
    UnExpect,
    parse,
    skip_many,
    token_prim,
    update_pos_char,
    ParseError,
)
from entoli.prelude import elem

_U = TypeVar("_U")

# satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
# {-# INLINABLE satisfy #-}
# satisfy f           = tokenPrim (\c -> show [c])
#                                 (\pos c _cs -> updatePosChar pos c)
#                                 (\c -> if f c then Just c else Nothing)


def satisfy(f: Callable[[str], bool]) -> ParsecT[Iterable[str], _U, str]:
    return token_prim(
        lambda c: str(c),
        lambda pos, c, cs: update_pos_char(pos, c),
        lambda c: Just(c) if f(c) else Nothing(),
    )


def _test_satisfy():
    assert parse(satisfy(lambda x: x == "a"), "", "") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("")]
    )
    assert parse(satisfy(lambda x: x == "a"), "", "a") == "a"
    assert parse(satisfy(lambda x: x == "a"), "", "b") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("b")]
    )


# oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
# {-# INLINABLE oneOf #-}
# oneOf cs            = satisfy (\c -> elem c cs)


def one_of(cs: str) -> ParsecT[Iterable[str], _U, str]:
    return satisfy(lambda c: elem(c, cs))


def _test_one_of():
    assert parse(one_of("ab"), "", "") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("")]
    )
    assert parse(one_of("ab"), "", "a") == "a"
    assert parse(one_of("ab"), "", "b") == "b"
    assert parse(one_of("ab"), "", "c") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("c")]
    )


# -- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
# -- character /not/ in the supplied list of characters @cs@. Returns the
# -- parsed character.
# --
# -- >  consonant = noneOf "aeiou"

# noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
# {-# INLINABLE noneOf #-}
# noneOf cs           = satisfy (\c -> not (elem c cs))


def none_of(cs: str) -> ParsecT[Iterable[str], _U, str]:
    return satisfy(lambda c: not elem(c, cs))


def _test_none_of():
    assert parse(none_of("ab"), "", "") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("")]
    )
    assert parse(none_of("ab"), "", "a") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("a")]
    )
    assert parse(none_of("ab"), "", "b") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("b")]
    )
    assert parse(none_of("ab"), "", "c") == "c"


# -- | Parses a white space character (any character which satisfies 'isSpace')
# -- Returns the parsed character.

# space :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE space #-}
# space               = satisfy isSpace       <?> "space"


# def space() -> ParsecT[Iterable[str], _U, str]:
#     return satisfy(str.isspace)

space = satisfy(str.isspace)


def _test_space():
    assert parse(space, "", "") == ParseError(SourcePos("", 1, 1), [SysUnExpect("")])
    assert parse(space, "", " ") == " "
    assert parse(space, "", "a") == ParseError(SourcePos("", 1, 1), [SysUnExpect("a")])


# -- | Skips /zero/ or more white space characters. See also 'skipMany'.

# spaces :: (Stream s m Char) => ParsecT s u m ()
# {-# INLINABLE spaces #-}
# spaces              = skipMany space        <?> "white space"


# def spaces() -> ParsecT[Iterable[str], _U, None]:
#     return skip_many(space)

spaces = skip_many(space)


def _test_spaces():
    assert parse(spaces, "", "") == None
    assert parse(spaces, "", " ") == None
    assert parse(spaces, "", "  ") == None
    assert parse(spaces, "", "a") == None


# -- | @char c@ parses a single character @c@. Returns the parsed
# -- character (i.e. @c@).
# --
# -- >  semiColon  = char ';'

# char :: (Stream s m Char) => Char -> ParsecT s u m Char
# {-# INLINABLE char #-}
# char c              = satisfy (==c)  <?> show [c]


def char(c: str) -> ParsecT[Iterable[str], _U, str]:
    return satisfy(lambda x: x == c)


def _test_char():
    assert parse(char("a"), "", "") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("")]
    )
    assert parse(char("a"), "", "a") == "a"
    assert parse(char("a"), "", "b") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("b")]
    )


# -- | Parses a newline character (\'\\n\'). Returns a newline character.

# newline :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE newline #-}
# newline             = char '\n'             <?> "lf new-line"


# def new_line() -> ParsecT[Iterable[str], _U, str]:
#     return char("\n")

new_line = char("\n")


def _test_new_line():
    assert parse(new_line, "", "") == ParseError(SourcePos("", 1, 1), [SysUnExpect("")])
    assert parse(new_line, "", "\n") == "\n"
    assert parse(new_line, "", "a") == ParseError(
        SourcePos("", 1, 1), [SysUnExpect("a")]
    )


# -- | Parses a carriage return character (\'\\r\') followed by a newline character (\'\\n\').
# -- Returns a newline character.

# crlf :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE crlf #-}
# crlf                = char '\r' *> char '\n' <?> "crlf new-line"


# def crlf() -> ParsecT[Iterable[str], _U, str]:
#     return char("\r").then(char("\n"))

crlf = char("\r").then(char("\n"))


def _test_crlf():
    assert parse(crlf, "", "") == ParseError(SourcePos("", 1, 1), [SysUnExpect("")])
    assert parse(crlf, "", "\r\n") == "\n"
    assert parse(crlf, "", "\r") == ParseError(SourcePos("", 1, 2), [SysUnExpect("")])
    assert parse(crlf, "", "\n") == ParseError(SourcePos("", 1, 1), [SysUnExpect("\n")])
    assert parse(crlf, "", "a") == ParseError(SourcePos("", 1, 1), [SysUnExpect("a")])


# -- | Parses a CRLF (see 'crlf') or LF (see 'newline') end-of-line.
# -- Returns a newline character (\'\\n\').
# --
# -- > endOfLine = newline <|> crlf
# --


# endOfLine :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE endOfLine #-}
# endOfLine           = newline <|> crlf       <?> "new-line"


def end_of_line() -> ParsecT[Iterable[str], _U, str]:
    return new_line.or_else(crlf)


# ! temp Error
# def _test_end_of_line():
#     assert parse(end_of_line(), "", "") == ParseError(
#         SourcePos("", 1, 1), [SysUnExpect("")]
#     )
#     assert parse(end_of_line(), "", "\n") == "\n"
#     assert parse(end_of_line(), "", "\r\n") == "\n"
#     assert parse(end_of_line(), "", "\r") == ParseError(
#         SourcePos("", 1, 2), [SysUnExpect("")]
#     )
#     assert parse(end_of_line(), "", "a") == ParseError(
#         SourcePos("", 1, 1), [SysUnExpect("a")]
#     )


# -- | Parses a tab character (\'\\t\'). Returns a tab character.


# tab :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE tab #-}
# tab                 = char '\t'             <?> "tab"

tab = char("\t")


def _test_tab():
    assert parse(tab, "", "") == ParseError(SourcePos("", 1, 1), [SysUnExpect("")])
    assert parse(tab, "", "\t") == "\t"
    assert parse(tab, "", "a") == ParseError(SourcePos("", 1, 1), [SysUnExpect("a")])


# -- | Parses an upper case letter (according to 'isUpper').
# -- Returns the parsed character.

# upper :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE upper #-}
# upper               = satisfy isUpper       <?> "uppercase letter"

# -- | Parses a lower case character (according to 'isLower').
# -- Returns the parsed character.

# lower :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE lower #-}
# lower               = satisfy isLower       <?> "lowercase letter"

# -- | Parses a alphabetic or numeric Unicode characters
# -- according to 'isAlphaNum'. Returns the parsed character.
# --
# -- Note that numeric digits outside the ASCII range (such as arabic-indic digits like e.g. \"٤\" or @U+0664@),
# -- as well as numeric characters which aren't digits, are parsed by this function
# -- but not by 'digit'.

# alphaNum :: (Stream s m Char => ParsecT s u m Char)
# {-# INLINABLE alphaNum #-}
# alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

# -- | Parses an alphabetic Unicode characters (lower-case, upper-case and title-case letters,
# -- plus letters of caseless scripts and modifiers letters according to 'isAlpha').
# -- Returns the parsed character.

# letter :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE letter #-}
# letter              = satisfy isAlpha       <?> "letter"

# -- | Parses an ASCII digit. Returns the parsed character.

# digit :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE digit #-}
# digit               = satisfy isDigit       <?> "digit"

# -- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
# -- \'f\' or \'A\' and \'F\'). Returns the parsed character.

# hexDigit :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE hexDigit #-}
# hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

# -- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
# -- the parsed character.

# octDigit :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE octDigit #-}
# octDigit            = satisfy isOctDigit    <?> "octal digit"


# -- | This parser succeeds for any character. Returns the parsed character.

# anyChar :: (Stream s m Char) => ParsecT s u m Char
# {-# INLINABLE anyChar #-}
# anyChar             = satisfy (const True)

# -- | The parser @satisfy f@ succeeds for any character for which the
# -- supplied function @f@ returns 'True'. Returns the character that is
# -- actually parsed.

# -- >  digit     = satisfy isDigit
# -- >  oneOf cs  = satisfy (\c -> c `elem` cs)


# -- | @'string' s@ parses a sequence of characters given by @s@. Returns
# -- the parsed string (i.e. @s@).
# --
# -- >  divOrMod    =   string "div"
# -- >              <|> string "mod"
# --
# -- Consider using 'string''.

# string :: (Stream s m Char) => String -> ParsecT s u m String
# {-# INLINABLE string #-}
# string s            = tokens show updatePosString s

# -- | @'string'' s@ parses a sequence of characters given by @s@.
# -- Doesn't consume matching prefix.
# --
# -- >  carOrCdr    =   string' "car"
# -- >              <|> string' "cdr"
# --
# -- @since 3.1.16.0

# string' :: (Stream s m Char) => String -> ParsecT s u m String
# {-# INLINABLE string' #-}
# string' s            = tokens' show updatePosString s
