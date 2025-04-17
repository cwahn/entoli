use std::rc::Rc;

use crate::{
    base::RcMut,
    source_ref::{self, SourceRef},
};

#[derive(Debug, Clone, PartialEq)]
pub struct CtorIdent(pub SourceRef);

#[derive(Debug, Clone, PartialEq)]
pub struct VarIdent(pub SourceRef);

#[derive(Debug, Clone, PartialEq)]
pub struct OpIdent(pub SourceRef);

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    List(Rc<[RcMut<SExpr>]>),      // List of SExpr e.g. "(a b c)"
    QuoteList(Rc<[RcMut<SExpr>]>), // Quote List of SExpr e.g. "'(a b c)"

    // keywords
    Binding,   // e.g. "=", operator
    TypeAnnot, // e.g. ":", operator
    DoBinding, // e.g. ":=", operator

    Lambda,    // e.g. "lambda"
    Data,      // e.g. "data"
    Trait,     // e.g. "trait"
    Impl,      // e.g. "impl"
    TypeAlias, // e.g. "type"
    KindAnnot, // e.g. "kind"
    Mod,       // e.g. "mod"
    Use,       // e.g. "use"
    Macro,     // e.g. "macro"
    Do,        // e.g. "do"

    // Pattern matching relate
    Match,    // "match"
    Where,    // "where"
    Wildcard, // "_"
    Ellipsis, // ".."

    // Etc.
    As, // "@" operator

    // Builtin constructors and operators
    Arrow,    // e.g. "->", operator
    Quote,    // e.g. "'",
    Nil,      // e.g. "Nil",
    Hash(u8), // e.g. "#2", "#3" ... "#12"

    Asterisk, // e.g. "*", operator

    Eq, // e.g. "==", operator
    Ne, // e.g. "!=", operator
    Gt, // e.g. ">", operator
    Ge, // e.g. ">=", operator
    Lt, // e.g. "<", operator
    Le, // e.g. "<=", operator

    And, // e.g. "&" operator
    Or,  // e.g. "|" operator
    Not, // e.g. "!" operator

    Add, // e.g. "+", operator
    Sub, // e.g. "-", operator
    // Asterisk is used for multiplication
    Div,     // e.g. "/", operator
    Modulus, // e.g. "%", operator

    Dot, // e.g. ".", operator

    Unit, // e.g. "()", Refers both Unit type and Unit value

    // Type-level literals (use variable identifier)
    BoolType, // e.g. "bool"
    I64Type,  // e.g. "i64"
    F64Type,  // e.g. "f64"
    StrType,  // e.g. "str"

    // Literal data
    Bool(bool),        // e.g. "true", "false"
    I64(i64),          // e.g. "1", "2", "-3"
    F64(f64),          // e.g. "1.0", "2.0", "-3.0"\
    String(SourceRef), // e.g. "\"abc\"", "\"def\""

    // User-defined
    CtorIdent(CtorIdent),    // e.g. "SomeType", "AnotherType"
    VarIdent(VarIdent),      // e.g. "x", "y", "z"
    OpIdent(OpIdent),        // e.g. ">>=", "++",
    FieldAccessor(VarIdent), // e.g. ".some_field", ".another_field" (VarIdent Does not include dot)

    IdentPath(Rc<[RcMut<SExpr>]>), // e.g. "some_module::SomeType::>>=", "some_module::.."
    // What could form IdentPath?
    // mod_name::mod_name...::(ctor_name, trait_name, data_name, and List for trait disambiguation)
    // IdentTree(Rc<[RcMut<SExpr>]>), // e.g. "some_module::another_module::(SomeType Another Value)",
    //                                // First element should be IdentPath, and the rest should be IdentTree
    // IdentTree is nothing but a list whose head is IdentPath and rest is Ident-like or ..
    IdentpathNil, // Used for leading and trailing "::"
}

pub struct Lexer {
    source: Rc<String>,
    pos: u32, // Current position to consume or peek
}

/* -------------------------------------------------------------------------- */
/*                               Implementations                              */
/* -------------------------------------------------------------------------- */

impl SExpr {
    pub fn is_operator(&self) -> bool {
        match self {
            SExpr::Binding
            | SExpr::TypeAnnot
            | SExpr::DoBinding
            | SExpr::As
            | SExpr::Arrow
            | SExpr::Asterisk
            | SExpr::Eq
            | SExpr::Ne
            | SExpr::Gt
            | SExpr::Ge
            | SExpr::Lt
            | SExpr::Le
            | SExpr::And
            | SExpr::Or
            | SExpr::Not
            | SExpr::Add
            | SExpr::Sub
            | SExpr::Div
            | SExpr::Modulus
            | SExpr::Dot
            | SExpr::OpIdent(_) => true,
            SExpr::IdentPath(s_exprs) => match s_exprs.last() {
                Some(s_expr) => s_expr.get().is_operator(),
                None => false,
            },
            _ => false,
        }
    }
}

impl Lexer {
    pub fn new(source: Rc<String>) -> Self {
        Lexer {
            source: source.clone(),
            pos: 0,
        }
    }

    pub fn err_msg(&self, error_message: &str) -> String {
        let source_ref = SourceRef::new(self.source.clone(), self.pos, self.pos);
        source_ref.error(error_message)
    }

    pub fn range_err_msg(&self, start_pos: u32, end_pos: u32, error_message: &str) -> String {
        let source_ref = SourceRef::new(self.source.clone(), start_pos, end_pos);
        source_ref.error(error_message)
    }

    pub fn parse_s_exprs(&mut self) -> Result<Rc<[RcMut<SExpr>]>, String> {
        let mut s_exprs = Vec::new();
        self.skip_whitespace_and_comments();

        while let Some(c) = self.peek() {
            match self.parse_s_expr() {
                Ok(s_expr) => {
                    s_exprs.push(s_expr);
                }
                Err(e) => {
                    return Err(format!("While parsing s-expr: \n{}", e));
                }
            }

            self.skip_whitespace_and_comments();
        }
        Ok(s_exprs.into())
    }

    pub fn parse_s_expr(&mut self) -> Result<RcMut<SExpr>, String> {
        if self.is_path_delimiter() {
            let mut path_elems = Vec::new();
            path_elems.push(RcMut::new(SExpr::IdentpathNil));

            return self.parse_ident_path(path_elems);
        }

        let res = self.parse_s_expr_impl()?;

        if self.is_path_delimiter() {
            // Start of IdentPath
            let mut path_elems = Vec::new();
            path_elems.push(res);
            return self.parse_ident_path(path_elems);
        }

        Ok(res)
    }

    /// Parse a s-expr which is not IdentPath element
    pub fn parse_s_expr_impl(&mut self) -> Result<RcMut<SExpr>, String> {
        // Input should not be empty or comment
        match self.peek() {
            Some('(') => self.parse_list_or_unit(),
            Some('\'') => match self.peek_nth(1) {
                Some('(') => self.parse_quoted_list(),
                Some(c) if c.is_whitespace() => {
                    self.next(); // consume '\''
                    self.skip_whitespace_and_comments();

                    Ok(RcMut::new(SExpr::Quote))
                }
                None => {
                    self.next(); // consume '\''
                    Ok(RcMut::new(SExpr::Quote))
                }
                _ => Err(self.err_msg(&format!(
                    "While parsing s-expr, expected '(' or whitespace after ' but found {}",
                    self.peek_nth(1).unwrap()
                ))),
            },
            Some('#') => match self.peek_nth(1) {
                Some('(') => self.parse_hashed_list(),
                Some(c) if c.is_digit(10) => {
                    self.next(); // consume '#'
                    let start_pos = self.pos;

                    while let Some(c) = self.peek() {
                        if c.is_digit(10) {
                            self.next(); // consume digit
                        } else {
                            break;
                        }
                    }

                    let end_pos = self.pos;

                    self.skip_whitespace_and_comments();

                    let s = &self.source[start_pos as usize..end_pos as usize];
                    let n = s.parse::<u8>().unwrap();

                    Ok(RcMut::new(SExpr::Hash(n)))
                }
                Some(c) => Err(self.err_msg(&format!(
                    "While parsing s-expr, expected '(' or digit after # but found {c}"
                ))),

                None => Err(self.err_msg(&format!(
                    "While parsing s-expr, expected '(' or digit after # but found None"
                ))),
            },
            Some('@') => match self.peek_nth(1) {
                Some('(') => self.parse_compiler_directive(),
                Some(c) if c.is_whitespace() => {
                    self.next(); // consume '@'
                    self.skip_whitespace_and_comments();

                    Ok(RcMut::new(SExpr::As))
                }
                None => {
                    self.next(); // consume '@'
                    Ok(RcMut::new(SExpr::As))
                }
                _ => Err(self.err_msg(&format!(
                    "While parsing s-expr, expected '(', whitespace or None after @ but found {}",
                    self.peek_nth(1).unwrap()
                ))),
            },
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => {
                Err(self.err_msg("While parsing s-expr, expected any character but found None"))
            }
        }
    }

    pub fn parse_list_or_unit(&mut self) -> Result<RcMut<SExpr>, String> {
        self.next(); // consume '('
        self.skip_whitespace_and_comments();

        if let Some(')') = self.peek() {
            self.next(); // consume ')'
            return Ok(RcMut::new(SExpr::Unit)); // No empty list
        }

        let mut s_exprs = Vec::new();

        // Parse first element as normal s-expr

        match self.parse_s_expr() {
            Ok(s_expr) => {
                s_exprs.push(s_expr);
            }
            Err(e) => {
                return Err(self.err_msg(&format!(
                    "While parsing first element of s-expr list: \n{}",
                    e
                )));
            }
        }
        self.skip_whitespace_and_comments();

        // Parse second element

        match self.peek() {
            Some(')') => {
                self.next(); // consume ')'
                self.skip_whitespace_and_comments();

                return Ok(RcMut::new(SExpr::List(s_exprs.into())));
            }
            Some('.') => {
                match self.peek() {
                    Some(')') => {
                        self.next(); // consume '.'
                        self.next(); // consume ')'
                        self.skip_whitespace_and_comments();

                        s_exprs.push(RcMut::new(SExpr::Dot));
                        return Ok(RcMut::new(SExpr::List(s_exprs.into())));
                    }
                    Some(c) if c.is_whitespace() => {
                        self.next(); // consume '.'
                        self.skip_whitespace_and_comments();

                        s_exprs.push(RcMut::new(SExpr::Dot));
                    }
                    Some(_) => {
                        return self.parse_objective_tail(s_exprs.pop().unwrap());
                    }
                    None => {
                        return Err(self.err_msg(
                            "While parsing second element of s-expr list, expected character after '.' but found None",
                        ));
                    }
                }
            }
            Some(_) => {
                match self.parse_s_expr() {
                    Ok(s_expr) => {
                        // Check if SPO applied.
                        // If the second element is operator, it need to be swapped
                        let is_op = s_expr.get().is_operator();
                        match is_op {
                            true => {
                                let head = s_exprs.pop().unwrap();
                                s_exprs.push(s_expr);
                                s_exprs.push(head);
                            }
                            false => {
                                s_exprs.push(s_expr);
                            }
                        };
                    }
                    Err(e) => {
                        return Err(format!(
                            "While parsing second element of s-expr list: \n{}",
                            e
                        ));
                    }
                }
                self.skip_whitespace_and_comments();
            }
            None => {
                return Err(self.err_msg(
                    "While parsing second element of s-expr list, expected any character but found None",
                ));
            }
        }

        // todo parse other elements

        while let Some(c) = self.peek() {
            if c == ')' {
                self.next(); // consume ')'
                break;
            }

            match self.parse_s_expr() {
                Ok(s_expr) => {
                    s_exprs.push(s_expr);
                }
                Err(e) => {
                    return Err(format!("While parsing element of s-expr list: \n{}", e));
                }
            }
            self.skip_whitespace_and_comments();
        }

        Ok(RcMut::new(SExpr::List(s_exprs.into())))
    }

    pub fn parse_quoted_list(&mut self) -> Result<RcMut<SExpr>, String> {
        self.next(); // consume '\''
        self.next(); // consume '('
        self.skip_whitespace_and_comments();

        let mut s_exprs = Vec::new();
        // Quoted list can not be disambiguated at this point
        // It should be done during parsing

        while let Some(c) = self.peek() {
            if c == ')' {
                self.next(); // consume ')'
                break;
            }

            match self.parse_s_expr() {
                Ok(s_expr) => {
                    s_exprs.push(s_expr);
                }
                Err(e) => {
                    return Err(format!(
                        "While parsing element of s-expr quoted list: \n{}",
                        e
                    ));
                }
            }
            self.skip_whitespace_and_comments();
        }

        Ok(RcMut::new(SExpr::QuoteList(s_exprs.into())))
    }

    pub fn parse_hashed_list(&mut self) -> Result<RcMut<SExpr>, String> {
        self.next(); // consume '#'
        self.next(); // consume '('
        self.skip_whitespace_and_comments();

        let mut s_exprs = Vec::new();
        s_exprs.push(RcMut::new(SExpr::Hash(0))); // Placeholder for the hash

        while let Some(c) = self.peek() {
            if c == ')' {
                self.next(); // consume ')'
                break;
            }

            match self.parse_s_expr() {
                Ok(s_expr) => {
                    s_exprs.push(s_expr);
                }
                Err(e) => {
                    return Err(format!(
                        "While parsing element of s-expr hashed list: \n{}",
                        e
                    ));
                }
            }
            self.skip_whitespace_and_comments();
        }

        let n = (s_exprs.len() - 1) as u8;
        if n >= 2 && n < 13 {
            *s_exprs[0].get_mut() = SExpr::Hash(n);
            Ok(RcMut::new(SExpr::List(s_exprs.into())))
        } else {
            Err(self.err_msg(&format!(
                "While parsing s-expr hashed list, expected 2 <= n < 13 but found {}",
                n
            )))
        }
    }

    pub fn parse_string(&mut self) -> Result<RcMut<SExpr>, String> {
        self.next(); // consume '"'
        let start = self.pos;

        while let Some(c) = self.peek() {
            match c {
                '\\' => {
                    // Espace sequence
                    self.next(); // consume '\'
                    match self.peek() {
                        Some(_) => {
                            self.next(); // consume the escaped character
                        }
                        None => {
                            return Err(self.err_msg(
                                "While parsing string, expected character after \\ but found None",
                            ));
                        }
                    }
                }
                '"' => {
                    let end = self.pos;
                    let source_ref = SourceRef::new(self.source.clone(), start, end);

                    self.next(); // consume '"'

                    return Ok(RcMut::new(SExpr::String(source_ref)));
                }
                _ => {
                    self.next(); // consume the character
                }
            }
        }

        Err(self.range_err_msg(
            start,
            self.pos,
            "While parsing s-expr string, expected next character but found None",
        ))
    }

    fn parse_ident_path(&mut self, mut acc: Vec<RcMut<SExpr>>) -> Result<RcMut<SExpr>, String> {
        while self.is_path_delimiter() {
            self.next(); // consume ':'
            self.next(); // consume ':'

            match self.peek() {
                Some(c) => {
                    if c.is_whitespace() || c == ')' || c == ';' {
                        self.next(); // consume whitespace or ')'
                        acc.push(RcMut::new(SExpr::IdentpathNil));
                    } else {
                        match self.parse_s_expr_impl() {
                            Ok(s_expr) => {
                                acc.push(s_expr);
                            }
                            Err(e) => {
                                return Err(format!(
                                    "While parsing ident path after {:?}: \n{}",
                                    acc, e
                                ));
                            }
                        }
                    }
                }
                None => {
                    acc.push(RcMut::new(SExpr::IdentpathNil));
                }
            }
        }

        Ok(RcMut::new(SExpr::IdentPath(acc.into())))
    }

    fn parse_atom(&mut self) -> Result<RcMut<SExpr>, String> {
        let start_pos = self.pos;
        let mut i = 0;

        while let Some(c) = self.peek_nth(i) {
            if c.is_whitespace() || c == ')' || c == ';' || self.is_path_delimiter_nth(i) {
                break;
            }

            i += 1;
        }

        let end_pos = self.pos + i;

        {
            let s = &self.source[start_pos as usize..end_pos as usize]; // First element should be IdentPath, and the rest should be IdentTree

            let res = match s {
                "" => {
                    return Err(
                        self.err_msg("While parsing s-expr, expected any character but found None")
                    );
                }
                "=" => Ok(RcMut::new(SExpr::Binding)),
                ":" => Ok(RcMut::new(SExpr::TypeAnnot)),
                ":=" => Ok(RcMut::new(SExpr::DoBinding)),

                "lambda" => Ok(RcMut::new(SExpr::Lambda)),
                "data" => Ok(RcMut::new(SExpr::Data)),
                "trait" => Ok(RcMut::new(SExpr::Trait)),
                "impl" => Ok(RcMut::new(SExpr::Impl)),
                "type" => Ok(RcMut::new(SExpr::TypeAlias)),
                "kind" => Ok(RcMut::new(SExpr::KindAnnot)),

                "mod" => Ok(RcMut::new(SExpr::Mod)),
                "use" => Ok(RcMut::new(SExpr::Use)),
                "macro" => Ok(RcMut::new(SExpr::Macro)),
                "do" => Ok(RcMut::new(SExpr::Do)),

                "match" => Ok(RcMut::new(SExpr::Match)),
                "where" => Ok(RcMut::new(SExpr::Where)),
                "_" => Ok(RcMut::new(SExpr::Wildcard)),
                ".." => Ok(RcMut::new(SExpr::Ellipsis)),

                "@" => Ok(RcMut::new(SExpr::As)),

                "->" => Ok(RcMut::new(SExpr::Arrow)),
                "'" => Ok(RcMut::new(SExpr::Quote)),
                "Nil" => Ok(RcMut::new(SExpr::Nil)),
                // Hash should be already handled
                "*" => Ok(RcMut::new(SExpr::Asterisk)),

                "==" => Ok(RcMut::new(SExpr::Eq)),
                "!=" => Ok(RcMut::new(SExpr::Ne)),
                ">" => Ok(RcMut::new(SExpr::Gt)),
                ">=" => Ok(RcMut::new(SExpr::Ge)),
                "<" => Ok(RcMut::new(SExpr::Lt)),
                "<=" => Ok(RcMut::new(SExpr::Le)),

                "&" => Ok(RcMut::new(SExpr::And)),
                "|" => Ok(RcMut::new(SExpr::Or)),
                "!" => Ok(RcMut::new(SExpr::Not)),

                "+" => Ok(RcMut::new(SExpr::Add)),
                "-" => Ok(RcMut::new(SExpr::Sub)),
                // Asterisk is used for multiplication
                "/" => Ok(RcMut::new(SExpr::Div)),
                "%" => Ok(RcMut::new(SExpr::Modulus)),

                "." => Ok(RcMut::new(SExpr::Dot)),

                "()" => Ok(RcMut::new(SExpr::Unit)),
                "i64" => Ok(RcMut::new(SExpr::I64Type)),
                "f64" => Ok(RcMut::new(SExpr::F64Type)),
                "bool" => Ok(RcMut::new(SExpr::BoolType)),
                "str" => Ok(RcMut::new(SExpr::StrType)),

                "true" => Ok(RcMut::new(SExpr::Bool(true))),
                "false" => Ok(RcMut::new(SExpr::Bool(false))),
                // String should be already handled
                s => {
                    if let Ok(i) = s.parse::<i64>() {
                        Ok(RcMut::new(SExpr::I64(i)))
                    } else if let Ok(f) = s.parse::<f64>() {
                        Ok(RcMut::new(SExpr::F64(f)))
                    } else {
                        Err("".to_string())
                    }
                }
            };

            if res.is_ok() {
                while self.pos < end_pos {
                    self.next();
                }
                return res;
            }
        }

        let first_char = self.source[start_pos as usize..end_pos as usize]
            .chars()
            .next()
            .unwrap(); // Known to be non-empty

        if first_char.is_uppercase() {
            // CtorIdent
            self.parse_ctor_ident(end_pos)
        } else if first_char.is_lowercase() || first_char == '_' {
            // VarIdent
            self.parse_var_ident(end_pos)
        } else if first_char == '.' {
            // FieldAccessor
            self.parse_field_accessor(end_pos)
        } else {
            // OpIdent
            self.parse_op_ident(end_pos)
        }
    }

    pub fn parse_objective_tail(&mut self, mut head: RcMut<SExpr>) -> Result<RcMut<SExpr>, String> {
        while let Some(c) = self.peek() {
            if c == ')' {
                self.next(); // consume ')'
                break;
            } else if c != '.' {
                return Err(self.err_msg(&format!(
                    "While parsing s-expr objective tail, expected '.' after {:?} but found {}",
                    head, c
                )));
            }

            self.next(); // consume '.'

            match self.peek() {
                Some('(') => {
                    // Objective application
                    self.next(); // consume '('
                    self.skip_whitespace_and_comments();

                    if let Some(')') = self.peek() {
                        return Err(
                            self.err_msg(&format!(
                                "While parsing s-expr objective application, expected function after {:?} but found None",  
                                head
                            ))
                        );
                    }

                    let mut temp_s_exprs = Vec::new();

                    match self.parse_s_expr() {
                        Ok(f) => {
                            temp_s_exprs.push(f);
                            temp_s_exprs.push(head.clone());
                        }
                        Err(e) => {
                            return Err(format!("While parsing objective application: \n{}", e));
                        }
                    }

                    self.skip_whitespace_and_comments();

                    while let Some(c) = self.peek() {
                        if c == ')' {
                            self.next(); // consume ')'
                            self.skip_whitespace_and_comments();

                            break;
                        }
                        match self.parse_s_expr() {
                            Ok(arg_s_expr) => {
                                temp_s_exprs.push(arg_s_expr);
                            }
                            Err(e) => {
                                return Err(format!(
                                    "While parsing element of s-expr objective application: \n{}",
                                    e
                                ));
                            }
                        }
                    }

                    head = RcMut::new(SExpr::List(temp_s_exprs.into()));
                }
                Some(_) => {
                    // Should be field accessor
                    match self.parse_s_expr() {
                        Ok(field_s_expr) => match &*field_s_expr.get() {
                            SExpr::VarIdent(var_id) => {
                                let mut temp_s_exprs = Vec::new();

                                temp_s_exprs.push(RcMut::new(SExpr::FieldAccessor(var_id.clone())));
                                temp_s_exprs.push(head.clone());

                                head = RcMut::new(SExpr::List(temp_s_exprs.into()));
                            }
                            _ => {
                                return Err(self.err_msg(&format!(
                                    "While parsing objective application after {:?}, expected VarIdent of field accessor but found {:?}",
                                    head, field_s_expr
                                )));
                            }
                        },
                        Err(e) => {
                            return Err(format!(
                                "While parsing objective application, expected s-expr but failed with: \n{:?}",
                                e
                            ));
                        }
                    }
                }
                None => unreachable!(),
            }
        }

        Ok(head)
    }

    pub fn parse_compiler_directive(&mut self) -> Result<RcMut<SExpr>, String> {
        self.next(); // consume '@'
        self.next(); // consume '('
        self.skip_whitespace_and_comments();

        let mut s_exprs = Vec::new();

        while let Some(c) = self.peek() {
            if c == ')' {
                self.next(); // consume ')'
                break;
            }

            match self.parse_s_expr() {
                Ok(s_expr) => {
                    s_exprs.push(s_expr);
                }
                Err(e) => {
                    return Err(format!(
                        "While parsing element of s-expr compiler directive: \n{}",
                        e
                    ));
                }
            }
            self.skip_whitespace_and_comments();
        }

        Ok(RcMut::new(SExpr::List(s_exprs.into())))
    }

    pub fn parse_ctor_ident(&mut self, end_pos: u32) -> Result<RcMut<SExpr>, String> {
        let s = &self.source[self.pos as usize..end_pos as usize];

        for c in s.chars() {
            if !c.is_alphanumeric() {
                return Err(self.err_msg(&format!(
                    "While parsing ctor ident, expected alphanumeric but found {}",
                    c
                )));
            }
        }

        let start_pos = self.pos;
        while self.pos < end_pos {
            self.next();
        }

        let source_ref = SourceRef::new(self.source.clone(), start_pos, end_pos);
        Ok(RcMut::new(SExpr::CtorIdent(CtorIdent(source_ref))))
    }

    pub fn parse_var_ident(&mut self, end_pos: u32) -> Result<RcMut<SExpr>, String> {
        let source_ref = self.parse_var_ident_impl(end_pos)?;

        Ok(RcMut::new(SExpr::VarIdent(VarIdent(source_ref))))
    }

    pub fn parse_field_accessor(&mut self, end_pos: u32) -> Result<RcMut<SExpr>, String> {
        self.next(); // consume '.'

        let source_ref = self.parse_var_ident_impl(end_pos)?;

        Ok(RcMut::new(SExpr::FieldAccessor(VarIdent(source_ref))))
    }

    fn parse_var_ident_impl(&mut self, end_pos: u32) -> Result<SourceRef, String> {
        let s = &self.source[self.pos as usize..end_pos as usize];

        for c in s.chars() {
            if !c.is_alphanumeric() && c != '_' {
                return Err(self.err_msg(&format!(
                    "While parsing var ident, expected alphanumeric or '_' but found {}",
                    c
                )));
            }
        }

        let start_pos = self.pos;
        while self.pos < end_pos {
            self.next();
        }

        let source_ref = SourceRef::new(self.source.clone(), start_pos, end_pos);
        Ok(source_ref)
    }

    pub fn parse_op_ident(&mut self, end_pos: u32) -> Result<RcMut<SExpr>, String> {
        let s = &self.source[self.pos as usize..end_pos as usize];

        for c in s.chars() {
            if !c.is_ascii_punctuation() {
                return Err(self.err_msg(&format!(
                    "While parsing op ident, expected ascii punctuation but found {}",
                    c
                )));
            }
        }

        let start_pos = self.pos;
        while self.pos < end_pos {
            self.next();
        }

        let source_ref = SourceRef::new(self.source.clone(), start_pos, end_pos);
        Ok(RcMut::new(SExpr::OpIdent(OpIdent(source_ref))))
    }

    pub fn peek(&self) -> Option<char> {
        self.source[self.pos as usize..].chars().next()
    }

    /// Peek n after the next character
    /// Peek 0 is the same with peek()
    pub fn peek_nth(&self, n: u32) -> Option<char> {
        let nth_pos = self.pos + n;

        self.source
            .get(nth_pos as usize..)
            .and_then(|s| s.chars().next())
    }

    pub fn peek_head(&self, n: u32) -> Option<&str> {
        let start = self.pos as usize;
        let end = (self.pos + n) as usize;

        self.source.get(start..end)
    }

    pub fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    pub fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else if c == ';' {
                self.next();
                while let Some(c) = self.next() {
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    pub fn is_path_delimiter(&self) -> bool {
        if let Some(c) = self.peek() {
            c == ':' && self.peek_nth(1) == Some(':')
        } else {
            false
        }
    }

    pub fn is_path_delimiter_nth(&self, n: u32) -> bool {
        if let Some(c) = self.peek_nth(n) {
            c == ':' && self.peek_nth(n + 1) == Some(':')
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_atom_keywords() {
        let source =
            Rc::new("= : := lambda data trait impl type kind mod use macro do".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(*s_exprs[0].get(), SExpr::Binding);
        assert_eq!(*s_exprs[1].get(), SExpr::TypeAnnot);
        assert_eq!(*s_exprs[2].get(), SExpr::DoBinding);
        assert_eq!(*s_exprs[3].get(), SExpr::Lambda);
        assert_eq!(*s_exprs[4].get(), SExpr::Data);
        assert_eq!(*s_exprs[5].get(), SExpr::Trait);
        assert_eq!(*s_exprs[6].get(), SExpr::Impl);
        assert_eq!(*s_exprs[7].get(), SExpr::TypeAlias);
        assert_eq!(*s_exprs[8].get(), SExpr::KindAnnot);
        assert_eq!(*s_exprs[9].get(), SExpr::Mod);
        assert_eq!(*s_exprs[10].get(), SExpr::Use);
        assert_eq!(*s_exprs[11].get(), SExpr::Macro);
        assert_eq!(*s_exprs[12].get(), SExpr::Do);
    }

    #[test]
    fn test_parse_atom_pattern_matching_related() {
        let source = Rc::new("match where _ ..".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(*s_exprs[0].get(), SExpr::Match);
        assert_eq!(*s_exprs[1].get(), SExpr::Where);
        assert_eq!(*s_exprs[2].get(), SExpr::Wildcard);
        assert_eq!(*s_exprs[3].get(), SExpr::Ellipsis);
    }

    #[test]
    fn test_parse_atom_etc() {
        let source = Rc::new("@".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(*s_exprs[0].get(), SExpr::As);
    }

    #[test]
    fn test_parse_atom_builtins() {
        let source = Rc::new(
            "-> ' Nil #2 #3 #12 * == != > >= < <= & | ! + - / % . () bool i64 f64 str".to_string(),
        );

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(*s_exprs[0].get(), SExpr::Arrow);
        assert_eq!(*s_exprs[1].get(), SExpr::Quote);
        assert_eq!(*s_exprs[2].get(), SExpr::Nil);
        assert_eq!(*s_exprs[3].get(), SExpr::Hash(2));
        assert_eq!(*s_exprs[4].get(), SExpr::Hash(3));
        assert_eq!(*s_exprs[5].get(), SExpr::Hash(12));
        assert_eq!(*s_exprs[6].get(), SExpr::Asterisk);
        assert_eq!(*s_exprs[7].get(), SExpr::Eq);
        assert_eq!(*s_exprs[8].get(), SExpr::Ne);
        assert_eq!(*s_exprs[9].get(), SExpr::Gt);
        assert_eq!(*s_exprs[10].get(), SExpr::Ge);
        assert_eq!(*s_exprs[11].get(), SExpr::Lt);
        assert_eq!(*s_exprs[12].get(), SExpr::Le);
        assert_eq!(*s_exprs[13].get(), SExpr::And);
        assert_eq!(*s_exprs[14].get(), SExpr::Or);
        assert_eq!(*s_exprs[15].get(), SExpr::Not);
        assert_eq!(*s_exprs[16].get(), SExpr::Add);
        assert_eq!(*s_exprs[17].get(), SExpr::Sub);
        assert_eq!(*s_exprs[18].get(), SExpr::Div);
        assert_eq!(*s_exprs[19].get(), SExpr::Modulus);
        assert_eq!(*s_exprs[20].get(), SExpr::Dot);
        assert_eq!(*s_exprs[21].get(), SExpr::Unit);
        assert_eq!(*s_exprs[22].get(), SExpr::BoolType);
        assert_eq!(*s_exprs[23].get(), SExpr::I64Type);
        assert_eq!(*s_exprs[24].get(), SExpr::F64Type);
        assert_eq!(*s_exprs[25].get(), SExpr::StrType);
    }

    #[test]
    fn test_parse_atom_literals() {
        let source = Rc::new("true false 0 42 -9.8".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(*s_exprs[0].get(), SExpr::Bool(true));
        assert_eq!(*s_exprs[1].get(), SExpr::Bool(false));
        assert_eq!(*s_exprs[2].get(), SExpr::I64(0));
        assert_eq!(*s_exprs[3].get(), SExpr::I64(42));
        assert_eq!(*s_exprs[4].get(), SExpr::F64(-9.8));
    }

    #[test]
    fn test_parse_atom_string() {
        let source = Rc::new(r#" "" "Hello, world!" "\"" "\n""#.to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        {
            let s = &*s_exprs[0].get();
            if let SExpr::String(source_ref) = s {
                assert_eq!(source_ref.resolve(), "");
            } else {
                panic!("Expected String but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[1].get();
            if let SExpr::String(source_ref) = s {
                assert_eq!(source_ref.resolve(), "Hello, world!");
            } else {
                panic!("Expected String but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[2].get();
            if let SExpr::String(source_ref) = s {
                assert_eq!(source_ref.resolve(), "\\\"");
            } else {
                panic!("Expected String but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[3].get();
            if let SExpr::String(source_ref) = s {
                assert_eq!(source_ref.resolve(), "\\n");
            } else {
                panic!("Expected String but found {:?}", s);
            }
        }
    }

    #[test]
    fn test_parse_atom_ident() {
        let source = Rc::new("Foo bar >>= .some_field".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        {
            let s = &*s_exprs[0].get();
            if let SExpr::CtorIdent(var_id) = s {
                assert_eq!(var_id.0.resolve(), "Foo");
            } else {
                panic!("Expected CtorIdent but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[1].get();
            if let SExpr::VarIdent(var_id) = s {
                assert_eq!(var_id.0.resolve(), "bar");
            } else {
                panic!("Expected VarIdent but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[2].get();
            if let SExpr::OpIdent(op_id) = s {
                assert_eq!(op_id.0.resolve(), ">>=");
            } else {
                panic!("Expected OpIdent but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[3].get();
            if let SExpr::FieldAccessor(var_id) = s {
                assert_eq!(var_id.0.resolve(), "some_field");
            } else {
                panic!("Expected FieldAccessor but found {:?}", s);
            }
        }
    }

    #[test]
    fn test_parse_atom_ident_path() {
        let source = Rc::new("::foo foo::Bar foo::".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        {
            let s = &*s_exprs[0].get();
            if let SExpr::IdentPath(path) = s {
                match &*path[0].get() {
                    SExpr::IdentpathNil => (),
                    _ => {
                        panic!("Expected IdentpathNil but found {:?}", s);
                    }
                }

                match &*path[1].get() {
                    SExpr::VarIdent(src_ref) => {
                        assert_eq!(src_ref.0.resolve(), "foo")
                    }
                    _ => {
                        panic!("Expected VarIdent but found {:?}", s);
                    }
                }
            } else {
                panic!("Expected IdentPath but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[1].get();
            if let SExpr::IdentPath(path) = s {
                match &*path[0].get() {
                    SExpr::VarIdent(src_ref) => {
                        assert_eq!(src_ref.0.resolve(), "foo")
                    }
                    _ => {
                        panic!("Expected VarIdent but found {:?}", s);
                    }
                }

                match &*path[1].get() {
                    SExpr::CtorIdent(src_ref) => {
                        assert_eq!(src_ref.0.resolve(), "Bar")
                    }
                    _ => {
                        panic!("Expected CtorIdent but found {:?}", s);
                    }
                }
            } else {
                panic!("Expected IdentPath but found {:?}", s);
            }
        }

        {
            let s = &*s_exprs[2].get();
            if let SExpr::IdentPath(path) = s {
                match &*path[0].get() {
                    SExpr::VarIdent(src_ref) => {
                        assert_eq!(src_ref.0.resolve(), "foo")
                    }
                    _ => {
                        panic!("Expected VarIdent but found {:?}", s);
                    }
                }

                match &*path[1].get() {
                    SExpr::IdentpathNil => (),
                    _ => {
                        panic!("Expected IdentpathNil but found {:?}", s);
                    }
                }
            } else {
                panic!("Expected IdentPath but found {:?}", s);
            }
        }
    }

    #[test]
    fn test_parse_list() {
        let source = Rc::new("(0 1)".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(
            *s_exprs[0].get(),
            SExpr::List(vec![RcMut::new(SExpr::I64(0)), RcMut::new(SExpr::I64(1))].into())
        );
    }

    #[test]
    fn test_parse_quoted_list() {
        let source = Rc::new("'(0 1)".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(
            *s_exprs[0].get(),
            SExpr::QuoteList(vec![RcMut::new(SExpr::I64(0)), RcMut::new(SExpr::I64(1))].into())
        );

        // ! temp failing test
        // {
        //     let source = Rc::new("'a".to_string());
        //     let mut lexer = Lexer::new(source.clone());

        //     match lexer.parse_s_exprs() {
        //         Ok(s_exprs) => (),
        //         Err(e) => {
        //             eprintln!("{}", e);
        //             panic!();
        //         }
        //     }
        // }
    }

    #[test]
    fn test_parse_hashed_list() {
        let source = Rc::new("#(0 1)".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(
            *s_exprs[0].get(),
            SExpr::List(
                vec![
                    RcMut::new(SExpr::Hash(2)),
                    RcMut::new(SExpr::I64(0)),
                    RcMut::new(SExpr::I64(1))
                ]
                .into()
            )
        );
    }

    #[test]
    #[should_panic]
    fn test_parse_hashed_list_too_few() {
        let source = Rc::new("#(0)".to_string());

        let mut lexer = Lexer::new(source.clone());
        lexer.parse_s_exprs().unwrap();
    }

    #[test]
    #[should_panic]
    fn test_parse_hashed_list_too_many() {
        let source = Rc::new("#(0 1 2 3 4 5 6 7 8 9 10 11 12)".to_string());

        let mut lexer = Lexer::new(source.clone());
        lexer.parse_s_exprs().unwrap();
    }

    #[test]
    fn test_parse_compiler_directive() {
        let source = Rc::new("@(foo bar)".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        assert_eq!(
            *s_exprs[0].get(),
            SExpr::List(
                vec![
                    RcMut::new(SExpr::VarIdent(VarIdent(SourceRef::new(
                        source.clone(),
                        2,
                        5
                    )))), // foo
                    RcMut::new(SExpr::VarIdent(VarIdent(SourceRef::new(
                        source.clone(),
                        6,
                        9
                    )))) // bar
                ]
                .into()
            )
        );
    }

    #[test]
    fn test_objective_syntax() {
        let source = Rc::new("(foo .bar) (baz .(qux quux))".to_string());

        let mut lexer = Lexer::new(source.clone());
        let s_exprs = lexer.parse_s_exprs().unwrap();

        {
            let s = &*s_exprs[0].get();
            match s {
                SExpr::List(list) => {
                    assert_eq!(list.len(), 2);
                    assert_eq!(
                        *list[0].get(),
                        SExpr::FieldAccessor(VarIdent(SourceRef::new(source.clone(), 6, 9)))
                    );
                    assert_eq!(
                        *list[1].get(),
                        SExpr::VarIdent(VarIdent(SourceRef::new(source.clone(), 1, 4)))
                    );
                }
                _ => panic!("Expected List but found {:?}", s),
            }
        }

        {
            let s = &*s_exprs[1].get();

            assert_eq!(
                *s,
                SExpr::List(
                    vec![
                        RcMut::new(SExpr::VarIdent(VarIdent(SourceRef::new(
                            source.clone(),
                            18,
                            21
                        )))), // qux
                        RcMut::new(SExpr::VarIdent(VarIdent(SourceRef::new(
                            source.clone(),
                            12,
                            15
                        )))), // baz
                        RcMut::new(SExpr::VarIdent(VarIdent(SourceRef::new(
                            source.clone(),
                            22,
                            26
                        )))) // quux
                    ]
                    .into()
                )
            )
        }
    }

    #[test]
    fn test_spo_syntax() {
        {
            let source = Rc::new("(1 Num::+ 2)".to_string());

            let mut lexer = Lexer::new(source.clone());
            let s_exprs = lexer.parse_s_exprs().unwrap();

            let s = &*s_exprs[0].get();
            match s {
                SExpr::List(list) => {
                    assert_eq!(list.len(), 3);
                    assert_eq!(
                        *list[0].get(),
                        SExpr::IdentPath(
                            vec![
                                RcMut::new(SExpr::CtorIdent(CtorIdent(SourceRef::new(
                                    source.clone(),
                                    3,
                                    6
                                )))), // Num
                                RcMut::new(SExpr::Add)
                            ]
                            .into()
                        )
                    );
                    assert_eq!(*list[1].get(), SExpr::I64(1));
                    assert_eq!(*list[2].get(), SExpr::I64(2));
                }
                _ => panic!("Expected List but found {:?}", s),
            }
        }
    }
}
