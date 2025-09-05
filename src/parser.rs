use std::rc::Rc;
use crate::{
    base::RcMut,
    lexer::SExpr,
    source_ref::SourceRef,
};

// ===========================================================
// AST Types - Core Language Constructs
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
    // Built-in operators and symbols
    Arrow,    // ->
    Quote,    // '
    Nil,      // Nil
    Hash(u8), // #2, #3, etc.
    
    // Arithmetic operators
    Add,      // +
    Sub,      // -
    Mul,      // * (Asterisk)
    Div,      // /
    Modulus,  // %
    
    // Comparison operators
    Eq,       // ==
    Ne,       // !=
    Gt,       // >
    Ge,       // >=
    Lt,       // <
    Le,       // <=
    
    // Logical operators
    And,      // &
    Or,       // |
    Not,      // !
    
    // Other operators
    Dot,      // .
    
    // Unit type/value
    Unit,     // ()
    
    // Built-in types
    BoolType, // Bool
    I64Type,  // I64
    F64Type,  // F64
    StrType,  // Str
    
    // User-defined identifiers
    CtorIdent(SourceRef),     // Constructor identifiers (uppercase)
    VarIdent(SourceRef),      // Variable identifiers (lowercase or _)
    OpIdent(SourceRef),       // Operator identifiers
    FieldAccessor(SourceRef), // Field accessors .field
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentPath(pub Vec<Ident>);

// ===========================================================
// Expressions
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literal values
    Literal(LiteralExpr),
    
    // Variable reference
    Var(IdentPath),
    
    // Function application
    App {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    
    // Lambda function
    Lambda {
        params: Vec<Pattern>,
        body: Box<Expr>,
    },
    
    // Let binding
    Let {
        pattern: Pattern,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    
    // Pattern matching
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    
    // Do notation for monadic computation
    Do {
        stmts: Vec<DoStmt>,
    },
    
    // Type annotation
    TypeAnnot {
        expr: Box<Expr>,
        type_expr: TypeExpr,
    },
    
    // Field access
    FieldAccess {
        object: Box<Expr>,
        field: Ident,
    },
    
    // Method call
    MethodCall {
        object: Box<Expr>,
        method: Ident,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Unit,
    Bool(bool),
    I64(i64),
    F64(f64),
    String(String),
    Char(char),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DoStmt {
    Bind {
        pattern: Pattern,
        expr: Expr,
    },
    Expr(Expr),
}

// ===========================================================
// Patterns
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    // Variable binding
    Var(Ident),
    
    // List variable binding (like xs..)
    ListVar(Ident),
    
    // Literal pattern
    Literal(LiteralExpr),
    
    // Constructor pattern
    Constructor {
        ctor: IdentPath,
        args: Vec<Pattern>,
    },
    
    // Tuple pattern
    Tuple(Vec<Pattern>),
    
    // List pattern
    List {
        elements: Vec<Pattern>,
        rest: Option<Box<Pattern>>, // for patterns like '(x xs..)
    },
    
    // Wildcard pattern
    Wildcard,
    
    // As pattern (pattern binding)
    As {
        pattern: Box<Pattern>,
        name: Ident,
    },
}

// ===========================================================
// Type Expressions
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    // Type variable
    Var(Ident),
    
    // Type constructor
    Constructor {
        ctor: IdentPath,
        args: Vec<TypeExpr>,
    },
    
    // Function type
    Function {
        param: Box<TypeExpr>,
        result: Box<TypeExpr>,
    },
    
    // Tuple type
    Tuple(Vec<TypeExpr>),
    
    // List type
    List(Box<TypeExpr>),
    
    // Universal quantification (forall)
    Forall {
        vars: Vec<Ident>,
        body: Box<TypeExpr>,
    },
    
    // Type with constraints
    Constrained {
        constraints: Vec<Constraint>,
        body: Box<TypeExpr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    pub trait_name: IdentPath,
    pub type_args: Vec<TypeExpr>,
}

// ===========================================================
// Declarations and Definitions
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    // Function type annotation
    TypeAnnotation {
        name: Ident,
        type_expr: TypeExpr,
    },
    
    // Function definition
    FunctionDef {
        name: Ident,
        rules: Vec<FunctionRule>,
    },
    
    // Data type definition
    DataDef {
        name: Ident,
        type_params: Vec<Ident>,
        constructors: Vec<Constructor>,
        constraints: Vec<Constraint>,
    },
    
    // Type alias
    TypeAlias {
        name: Ident,
        type_params: Vec<Ident>,
        body: TypeExpr,
        constraints: Vec<Constraint>,
    },
    
    // Trait definition
    TraitDef {
        name: Ident,
        type_params: Vec<Ident>,
        constraints: Vec<Constraint>,
        items: Vec<TraitItem>,
    },
    
    // Trait implementation
    ImplDef {
        trait_name: Option<IdentPath>, // None for inherent impl
        type_expr: TypeExpr,
        constraints: Vec<Constraint>,
        items: Vec<ImplItem>,
    },
    
    // Module definition
    ModuleDef {
        name: Ident,
        items: Vec<Decl>,
    },
    
    // Use declaration
    UseDef {
        path: IdentPath,
        items: Option<Vec<UseItem>>, // None for wildcard import
    },
    
    // Kind annotation
    KindAnnotation {
        name: Ident,
        kind: Kind,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionRule {
    pub patterns: Vec<Pattern>,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: Ident,
    pub fields: Vec<ConstructorField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstructorField {
    // Positional field
    Positional(TypeExpr),
    
    // Named field (for records)
    Named {
        name: Ident,
        type_expr: TypeExpr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    // Required type annotation
    TypeAnnotation {
        name: Ident,
        type_expr: TypeExpr,
    },
    
    // Default implementation
    FunctionDef {
        name: Ident,
        rules: Vec<FunctionRule>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplItem {
    // Function implementation
    FunctionDef {
        name: Ident,
        rules: Vec<FunctionRule>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseItem {
    Single(Ident),
    Multiple(Vec<UseItem>),
    Renamed { original: Ident, alias: Ident },
    Wildcard,
}

// ===========================================================
// Kinds
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    // Base kind *
    Star,
    
    // Function kind
    Arrow {
        param: Box<Kind>,
        result: Box<Kind>,
    },
    
    // Constraint kind
    Constraint,
}

// ===========================================================
// Top-level AST
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub declarations: Vec<Decl>,
}

// ===========================================================
// Parser Context and Functions
// ===========================================================

pub struct ParseContext {
    // Context for parsing - will be filled when implementing parser
}

impl ParseContext {
    pub fn new() -> Self {
        ParseContext {}
    }
}

// Parse a module from S-expressions
pub fn parse_module(_ctx: &mut ParseContext, _s_exprs: &[RcMut<SExpr>]) -> Result<Module, String> {
    // TODO: Implement parsing logic
    unimplemented!("parse_module not implemented yet")
}

// Parse a single declaration from an S-expression
pub fn parse_decl(_ctx: &mut ParseContext, _s_expr: &SExpr) -> Result<Decl, String> {
    // TODO: Implement parsing logic
    unimplemented!("parse_decl not implemented yet")
}

// Parse an expression from an S-expression
pub fn parse_expr(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<Expr, String> {
    match s_expr {
        // Literal values
        SExpr::I64(value) => Ok(Expr::Literal(LiteralExpr::I64(*value))),
        SExpr::F64(value) => Ok(Expr::Literal(LiteralExpr::F64(*value))),
        SExpr::Bool(value) => Ok(Expr::Literal(LiteralExpr::Bool(*value))),
        SExpr::Char(value) => Ok(Expr::Literal(LiteralExpr::Char(*value))),
        SExpr::String(source_ref) => Ok(Expr::Literal(LiteralExpr::String(source_ref.resolve().to_string()))),
        SExpr::Unit => Ok(Expr::Literal(LiteralExpr::Unit)),
        
        // Variable identifiers
        SExpr::VarIdent(var_ident) => {
            let ident_path = IdentPath(vec![Ident::VarIdent(var_ident.0.clone())]);
            Ok(Expr::Var(ident_path))
        },
        SExpr::CtorIdent(ctor_ident) => {
            // Special case for boolean literals
            let content = ctor_ident.0.resolve();
            match content {
                "True" => Ok(Expr::Literal(LiteralExpr::Bool(true))),
                "False" => Ok(Expr::Literal(LiteralExpr::Bool(false))),
                _ => {
                    let ident_path = IdentPath(vec![Ident::CtorIdent(ctor_ident.0.clone())]);
                    Ok(Expr::Var(ident_path))
                }
            }
        },
        
        _ => {
            unimplemented!("parse_expr for {:?} not implemented yet", s_expr)
        }
    }
}

// Parse a pattern from an S-expression
pub fn parse_pattern(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<Pattern, String> {
    match s_expr {
        // Literal patterns
        SExpr::I64(value) => Ok(Pattern::Literal(LiteralExpr::I64(*value))),
        SExpr::F64(value) => Ok(Pattern::Literal(LiteralExpr::F64(*value))),
        SExpr::Bool(value) => Ok(Pattern::Literal(LiteralExpr::Bool(*value))),
        SExpr::Char(value) => Ok(Pattern::Literal(LiteralExpr::Char(*value))),
        SExpr::String(source_ref) => Ok(Pattern::Literal(LiteralExpr::String(source_ref.resolve().to_string()))),
        SExpr::Unit => Ok(Pattern::Literal(LiteralExpr::Unit)),
        
        // Wildcard pattern
        SExpr::Wildcard => Ok(Pattern::Wildcard),
        
        // Variable patterns
        SExpr::VarIdent(var_ident) => Ok(Pattern::Var(Ident::VarIdent(var_ident.0.clone()))),
        
        // List variable patterns (like xs..)
        SExpr::ListIdent(list_ident) => Ok(Pattern::ListVar(Ident::VarIdent(list_ident.0.clone()))),
        
        // Constructor patterns (without args)
        SExpr::CtorIdent(ctor_ident) => {
            let ident_path = IdentPath(vec![Ident::CtorIdent(ctor_ident.0.clone())]);
            Ok(Pattern::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        
        _ => {
            unimplemented!("parse_pattern for {:?} not implemented yet", s_expr)
        }
    }
}

// Parse a type expression from an S-expression
pub fn parse_type_expr(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<TypeExpr, String> {
    match s_expr {
        // Type variables (lowercase identifiers)
        SExpr::VarIdent(var_ident) => Ok(TypeExpr::Var(Ident::VarIdent(var_ident.0.clone()))),
        
        // Type constructors (uppercase identifiers)
        SExpr::CtorIdent(ctor_ident) => {
            let ident_path = IdentPath(vec![Ident::CtorIdent(ctor_ident.0.clone())]);
            Ok(TypeExpr::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        
        // Built-in types - create dummy source refs for now
        SExpr::I64Type => {
            let dummy_source = Rc::new("i64".to_string());
            let ident_path = IdentPath(vec![Ident::CtorIdent(
                crate::source_ref::SourceRef::new(dummy_source, 0, 3)
            )]);
            Ok(TypeExpr::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        SExpr::F64Type => {
            let dummy_source = Rc::new("f64".to_string());
            let ident_path = IdentPath(vec![Ident::CtorIdent(
                crate::source_ref::SourceRef::new(dummy_source, 0, 3)
            )]);
            Ok(TypeExpr::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        SExpr::BoolType => {
            let dummy_source = Rc::new("bool".to_string());
            let ident_path = IdentPath(vec![Ident::CtorIdent(
                crate::source_ref::SourceRef::new(dummy_source, 0, 4)
            )]);
            Ok(TypeExpr::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        SExpr::StrType => {
            let dummy_source = Rc::new("str".to_string());
            let ident_path = IdentPath(vec![Ident::CtorIdent(
                crate::source_ref::SourceRef::new(dummy_source, 0, 3)
            )]);
            Ok(TypeExpr::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        SExpr::Unit => {
            let dummy_source = Rc::new("()".to_string());
            let ident_path = IdentPath(vec![Ident::CtorIdent(
                crate::source_ref::SourceRef::new(dummy_source, 0, 2)
            )]);
            Ok(TypeExpr::Constructor {
                ctor: ident_path,
                args: vec![],
            })
        },
        
        _ => {
            unimplemented!("parse_type_expr for {:?} not implemented yet", s_expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{self, SExpr};
    use std::rc::Rc;

    // Helper function to lex source code into S-expressions
    fn lex_source(source: &str) -> Rc<[RcMut<SExpr>]> {
        let source_rc = Rc::new(source.to_string());
        lexer::lex(source_rc).expect("Failed to lex source")
    }

    // ===========================================================
    // Basic Literal Tests
    // ===========================================================

    #[test]
    fn test_parse_integer_literal() {
        let s_exprs = lex_source("42");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        assert_eq!(expr, Expr::Literal(LiteralExpr::I64(42)));
    }

    #[test]
    fn test_parse_float_literal() {
        let s_exprs = lex_source("3.14");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        assert_eq!(expr, Expr::Literal(LiteralExpr::F64(3.14)));
    }

    #[test]
    fn test_parse_string_literal() {
        let s_exprs = lex_source("\"hello\"");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        assert_eq!(expr, Expr::Literal(LiteralExpr::String("hello".to_string())));
    }

    #[test]
    fn test_parse_char_literal() {
        let s_exprs = lex_source("'c'");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        assert_eq!(expr, Expr::Literal(LiteralExpr::Char('c')));
    }

    #[test]
    fn test_parse_boolean_literals() {
        let true_exprs = lex_source("True");
        let false_exprs = lex_source("False");
        let mut ctx = ParseContext::new();
        
        let true_expr = parse_expr(&mut ctx, &true_exprs[0].get()).unwrap();
        let false_expr = parse_expr(&mut ctx, &false_exprs[0].get()).unwrap();
        
        assert_eq!(true_expr, Expr::Literal(LiteralExpr::Bool(true)));
        assert_eq!(false_expr, Expr::Literal(LiteralExpr::Bool(false)));
    }

    #[test]
    fn test_parse_unit_literal() {
        let s_exprs = lex_source("()");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        assert_eq!(expr, Expr::Literal(LiteralExpr::Unit));
    }

    // ===========================================================
    // Variable and Identifier Tests
    // ===========================================================

    #[test]
    fn test_parse_variable_identifier() {
        let s_exprs = lex_source("variable_name");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Var(IdentPath(idents)) => {
                assert_eq!(idents.len(), 1);
                match &idents[0] {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "variable_name");
                    }
                    _ => panic!("Expected VarIdent"),
                }
            }
            _ => panic!("Expected Var expression"),
        }
    }

    #[test]
    fn test_parse_private_variable_identifier() {
        let s_exprs = lex_source("_private_var");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Var(IdentPath(idents)) => {
                assert_eq!(idents.len(), 1);
                match &idents[0] {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "_private_var");
                        assert!(source_ref.resolve().starts_with('_'));
                    }
                    _ => panic!("Expected VarIdent"),
                }
            }
            _ => panic!("Expected Var expression"),
        }
    }

    // ===========================================================
    // List and Tuple Tests
    // ===========================================================

    #[test]
    fn test_parse_list_literal() {
        let s_exprs = lex_source("'(1 2 3)");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Literal(LiteralExpr::List(elements)) => {
                assert_eq!(elements.len(), 3);
                assert_eq!(elements[0], Expr::Literal(LiteralExpr::I64(1)));
                assert_eq!(elements[1], Expr::Literal(LiteralExpr::I64(2)));
                assert_eq!(elements[2], Expr::Literal(LiteralExpr::I64(3)));
            }
            _ => panic!("Expected List literal"),
        }
    }

    #[test]
    fn test_parse_tuple_literal() {
        let s_exprs = lex_source("#(1 \"two\" 3.0)");
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Literal(LiteralExpr::Tuple(elements)) => {
                assert_eq!(elements.len(), 3);
                assert_eq!(elements[0], Expr::Literal(LiteralExpr::I64(1)));
                assert_eq!(elements[1], Expr::Literal(LiteralExpr::String("two".to_string())));
                assert_eq!(elements[2], Expr::Literal(LiteralExpr::F64(3.0)));
            }
            _ => panic!("Expected Tuple literal"),
        }
    }

    // ===========================================================
    // Function Application Tests
    // ===========================================================

    #[test]
    fn test_parse_simple_function_application() {
        let s_exprs = lex_source(r#"(f x)"#);
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::App { func, args } => {
                // Function should be variable 'f'
                match *func {
                    Expr::Var(IdentPath(ref idents)) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "f");
                            }
                            _ => panic!("Expected VarIdent for function"),
                        }
                    }
                    _ => panic!("Expected Var for function"),
                }
                
                // Should have one argument 'x'
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Expr::Var(IdentPath(idents)) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "x");
                            }
                            _ => panic!("Expected VarIdent for argument"),
                        }
                    }
                    _ => panic!("Expected Var for argument"),
                }
            }
            _ => panic!("Expected App expression"),
        }
    }

    #[test]
    fn test_parse_spo_syntax() {
        // Test SPO: (3 + 4) should desugar to (+ 3 4)
        let s_exprs = lex_source(r#"(3 + 4)"#);
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::App { func, args } => {
                // Function should be '+' operator
                match *func {
                    Expr::Var(IdentPath(ref idents)) => {
                        assert_eq!(idents.len(), 1);
                        assert_eq!(idents[0], Ident::Add);
                    }
                    _ => panic!("Expected Add operator"),
                }
                
                // Should have two arguments: 3 and 4
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], Expr::Literal(LiteralExpr::I64(3)));
                assert_eq!(args[1], Expr::Literal(LiteralExpr::I64(4)));
            }
            _ => panic!("Expected App expression"),
        }
    }

    // ===========================================================
    // Lambda Function Tests
    // ===========================================================

    #[test]
    fn test_parse_simple_lambda() {
        let s_exprs = lex_source(r#"(lambda x x)"#);
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Lambda { params, body } => {
                // Should have one parameter 'x'
                assert_eq!(params.len(), 1);
                match &params[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern for parameter"),
                }
                
                // Body should be variable 'x'
                match *body {
                    Expr::Var(IdentPath(ref idents)) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "x");
                            }
                            _ => panic!("Expected VarIdent for body"),
                        }
                    }
                    _ => panic!("Expected Var for body"),
                }
            }
            _ => panic!("Expected Lambda expression"),
        }
    }

    #[test]
    fn test_parse_lambda_with_multiple_params() {
        let s_exprs = lex_source(r#"(lambda (x y) (x + y))"#);
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Lambda { params, body: _ } => {
                // Should have two parameters 'x' and 'y'
                assert_eq!(params.len(), 2);
                match &params[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern for first parameter"),
                }
                match &params[1] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "y");
                    }
                    _ => panic!("Expected Var pattern for second parameter"),
                }
            }
            _ => panic!("Expected Lambda expression"),
        }
    }

    // ===========================================================
    // Pattern Tests
    // ===========================================================

    #[test]
    fn test_parse_wildcard_pattern() {
        let s_exprs = lex_source("_");
        let mut ctx = ParseContext::new();
        
        let pattern = parse_pattern(&mut ctx, &s_exprs[0].get()).unwrap();
        assert_eq!(pattern, Pattern::Wildcard);
    }

    #[test]
    fn test_parse_variable_pattern() {
        let s_exprs = lex_source("x");
        let mut ctx = ParseContext::new();
        
        let pattern = parse_pattern(&mut ctx, &s_exprs[0].get()).unwrap();
        match pattern {
            Pattern::Var(Ident::VarIdent(source_ref)) => {
                assert_eq!(source_ref.resolve(), "x");
            }
            _ => panic!("Expected Var pattern"),
        }
    }

    #[test]
    fn test_parse_constructor_pattern() {
        let s_exprs = lex_source(r#"(Just x)"#);
        let mut ctx = ParseContext::new();
        
        let pattern = parse_pattern(&mut ctx, &s_exprs[0].get()).unwrap();
        match pattern {
            Pattern::Constructor { ctor, args } => {
                // Constructor should be 'Just'
                match ctor {
                    IdentPath(ref idents) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Just");
                            }
                            _ => panic!("Expected CtorIdent"),
                        }
                    }
                }
                
                // Should have one argument pattern 'x'
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern for argument"),
                }
            }
            _ => panic!("Expected Constructor pattern"),
        }
    }

    #[test]
    fn test_parse_tuple_pattern() {
        let s_exprs = lex_source("#(a b)");
        let mut ctx = ParseContext::new();
        
        let pattern = parse_pattern(&mut ctx, &s_exprs[0].get()).unwrap();
        match pattern {
            Pattern::Tuple(elements) => {
                assert_eq!(elements.len(), 2);
                match &elements[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "a");
                    }
                    _ => panic!("Expected Var pattern for first element"),
                }
                match &elements[1] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "b");
                    }
                    _ => panic!("Expected Var pattern for second element"),
                }
            }
            _ => panic!("Expected Tuple pattern"),
        }
    }

    #[test]
    fn test_parse_list_pattern() {
        let s_exprs = lex_source(r#"'(x xs..)"#);
        let mut ctx = ParseContext::new();
        
        let pattern = parse_pattern(&mut ctx, &s_exprs[0].get()).unwrap();
        match pattern {
            Pattern::List { elements, rest } => {
                // Should have one element 'x'
                assert_eq!(elements.len(), 1);
                match &elements[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern for list element"),
                }
                
                // Should have rest pattern 'xs'
                assert!(rest.is_some());
                match rest.as_ref().unwrap().as_ref() {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "xs");
                    }
                    _ => panic!("Expected Var pattern for rest"),
                }
            }
            _ => panic!("Expected List pattern"),
        }
    }

    #[test]
    fn test_parse_list_variable_pattern() {
        let s_exprs = lex_source(r#"xs.."#);
        let mut ctx = ParseContext::new();
        
        let pattern = parse_pattern(&mut ctx, &s_exprs[0].get()).unwrap();
        match pattern {
            Pattern::ListVar(Ident::VarIdent(source_ref)) => {
                assert_eq!(source_ref.resolve(), "xs..");
            }
            _ => panic!("Expected ListVar pattern"),
        }
    }

    // ===========================================================
    // Type Expression Tests
    // ===========================================================

    #[test]
    fn test_parse_simple_type() {
        let s_exprs = lex_source("Int");
        let mut ctx = ParseContext::new();
        
        let type_expr = parse_type_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match type_expr {
            TypeExpr::Constructor { ctor, args } => {
                match ctor {
                    IdentPath(ref idents) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Int");
                            }
                            _ => panic!("Expected CtorIdent for type"),
                        }
                    }
                }
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected Constructor type"),
        }
    }

    #[test]
    fn test_parse_function_type() {
        let s_exprs = lex_source(r#"(Int -> Bool)"#);
        let mut ctx = ParseContext::new();
        
        let type_expr = parse_type_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match type_expr {
            TypeExpr::Function { param, result } => {
                // Parameter should be Int
                match *param {
                    TypeExpr::Constructor { ctor: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Int");
                            }
                            _ => panic!("Expected CtorIdent for param type"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Constructor for param type"),
                }
                
                // Result should be Bool
                match *result {
                    TypeExpr::Constructor { ctor: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Bool");
                            }
                            _ => panic!("Expected CtorIdent for result type"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Constructor for result type"),
                }
            }
            _ => panic!("Expected Function type"),
        }
    }

    #[test]
    fn test_parse_parameterized_type() {
        let s_exprs = lex_source(r#"(Maybe a)"#);
        let mut ctx = ParseContext::new();
        
        let type_expr = parse_type_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match type_expr {
            TypeExpr::Constructor { ctor, args } => {
                // Constructor should be 'Maybe'
                match ctor {
                    IdentPath(ref idents) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Maybe");
                            }
                            _ => panic!("Expected CtorIdent for Maybe"),
                        }
                    }
                }
                
                // Should have one type argument 'a'
                assert_eq!(args.len(), 1);
                match &args[0] {
                    TypeExpr::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "a");
                    }
                    _ => panic!("Expected Var type for argument"),
                }
            }
            _ => panic!("Expected Constructor type"),
        }
    }

    // ===========================================================
    // Declaration Tests
    // ===========================================================

    #[test]
    fn test_parse_type_annotation() {
        let s_exprs = lex_source(r#"
            (add : (Int -> (Int -> Int)))
        "#);
        let mut ctx = ParseContext::new();
        
        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::TypeAnnotation { name, type_expr: _ } => {
                match name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "add");
                    }
                    _ => panic!("Expected VarIdent for function name"),
                }
            }
            _ => panic!("Expected TypeAnnotation declaration"),
        }
    }

    #[test]
    fn test_parse_simple_function_definition() {
        let s_exprs = lex_source(r#"(add = (x y) (x + y))"#);
        let mut ctx = ParseContext::new();
        
        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::FunctionDef { name, rules } => {
                // Function name should be 'add'
                match name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "add");
                    }
                    _ => panic!("Expected VarIdent for function name"),
                }
                
                // Should have one rule
                assert_eq!(rules.len(), 1);
                let rule = &rules[0];
                
                // Should have two patterns: x and y
                assert_eq!(rule.patterns.len(), 2);
                match &rule.patterns[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern for first parameter"),
                }
                match &rule.patterns[1] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "y");
                    }
                    _ => panic!("Expected Var pattern for second parameter"),
                }
                
                // Body should be (x + y) application
                match &rule.body {
                    Expr::App { func: _, args } => {
                        assert_eq!(args.len(), 2);
                    }
                    _ => panic!("Expected App expression for body"),
                }
            }
            _ => panic!("Expected FunctionDef declaration"),
        }
    }

    #[test]
    fn test_parse_data_definition() {
        let s_exprs = lex_source(r#"
            (data (Maybe a) 
                Nothing 
                (Just a)
            )
        "#);
        let mut ctx = ParseContext::new();
        
        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::DataDef { name, type_params, constructors, constraints: _ } => {
                // Type name should be 'Maybe'
                match name {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Maybe");
                    }
                    _ => panic!("Expected CtorIdent for type name"),
                }
                
                // Should have one type parameter 'a'
                assert_eq!(type_params.len(), 1);
                match &type_params[0] {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "a");
                    }
                    _ => panic!("Expected VarIdent for type parameter"),
                }
                
                // Should have two constructors: Nothing and Just
                assert_eq!(constructors.len(), 2);
                match &constructors[0].name {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Nothing");
                    }
                    _ => panic!("Expected CtorIdent for Nothing constructor"),
                }
                match &constructors[1].name {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Just");
                    }
                    _ => panic!("Expected CtorIdent for Just constructor"),
                }
                
                // Nothing should have no fields, Just should have one
                assert_eq!(constructors[0].fields.len(), 0);
                assert_eq!(constructors[1].fields.len(), 1);
            }
            _ => panic!("Expected DataDef declaration"),
        }
    }

    #[test]
    fn test_parse_trait_definition() {
        let s_exprs = lex_source(r#"
            (trait (Show a) 
                (show : (a -> String)))
        "#);
        let mut ctx = ParseContext::new();
        
        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::TraitDef { name, type_params, constraints: _, items } => {
                // Trait name should be 'Show'
                match name {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Show");
                    }
                    _ => panic!("Expected CtorIdent for trait name"),
                }
                
                // Should have one type parameter 'a'
                assert_eq!(type_params.len(), 1);
                match &type_params[0] {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "a");
                    }
                    _ => panic!("Expected VarIdent for type parameter"),
                }
                
                // Should have one trait item (show method)
                assert_eq!(items.len(), 1);
                match &items[0] {
                    TraitItem::TypeAnnotation { name, type_expr: _ } => {
                        match name {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "show");
                            }
                            _ => panic!("Expected VarIdent for method name"),
                        }
                    }
                    _ => panic!("Expected TypeAnnotation trait item"),
                }
            }
            _ => panic!("Expected TraitDef declaration"),
        }
    }

    #[test]
    fn test_parse_impl_definition() {
        let s_exprs = lex_source(r#"
            (impl Show Bool 
                (show = 
                    True  "True" 
                    False "False")
                )
        "#);
        let mut ctx = ParseContext::new();
        
        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::ImplDef { trait_name, type_expr, constraints: _, items } => {
                // Trait name should be 'Show'
                assert!(trait_name.is_some());
                match trait_name.unwrap() {
                    IdentPath(ref idents) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Show");
                            }
                            _ => panic!("Expected CtorIdent for trait name"),
                        }
                    }
                }
                
                // Type should be Bool
                match type_expr {
                    TypeExpr::Constructor { ctor: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Bool");
                            }
                            _ => panic!("Expected CtorIdent for Bool"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Constructor type for Bool"),
                }
                
                // Should have implementation items
                assert_eq!(items.len(), 1);
                match &items[0] {
                    ImplItem::FunctionDef { name, rules } => {
                        match name {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "show");
                            }
                            _ => panic!("Expected VarIdent for method name"),
                        }
                        
                        // Should have pattern matching rules for True and False
                        assert_eq!(rules.len(), 2);
                    }
                }
            }
            _ => panic!("Expected ImplDef declaration"),
        }
    }

    #[test]
    fn test_parse_module_definition() {
        let s_exprs = lex_source(r#"
            (mod my_module 
                (data MyType 
                    (MyCons Int)
                )
            )
        "#);
        let mut ctx = ParseContext::new();
        
        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::ModuleDef { name, items } => {
                // Module name should be 'my_module'
                match name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "my_module");
                    }
                    _ => panic!("Expected VarIdent for module name"),
                }
                
                // Should have one item (the data definition)
                assert_eq!(items.len(), 1);
                match &items[0] {
                    Decl::DataDef { name, .. } => {
                        match name {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "MyType");
                            }
                            _ => panic!("Expected CtorIdent for data type name"),
                        }
                    }
                    _ => panic!("Expected DataDef in module"),
                }
            }
            _ => panic!("Expected ModuleDef declaration"),
        }
    }

    // ===========================================================
    // Complete Module Tests
    // ===========================================================

    #[test]
    fn test_parse_simple_module() {
        let source = r#"
            (id : (a -> a))
            (id = x x)
        "#;
        let s_exprs = lex_source(source);
        let mut ctx = ParseContext::new();
        
        let module = parse_module(&mut ctx, &*s_exprs).unwrap();
        assert_eq!(module.declarations.len(), 2);
        
        // First declaration should be type annotation
        match &module.declarations[0] {
            Decl::TypeAnnotation { name, type_expr: _ } => {
                match name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "id");
                    }
                    _ => panic!("Expected VarIdent for function name"),
                }
            }
            _ => panic!("Expected TypeAnnotation declaration"),
        }
        
        // Second declaration should be function definition
        match &module.declarations[1] {
            Decl::FunctionDef { name, rules } => {
                match name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "id");
                    }
                    _ => panic!("Expected VarIdent for function name"),
                }
                assert_eq!(rules.len(), 1);
            }
            _ => panic!("Expected FunctionDef declaration"),
        }
    }

    // ===========================================================
    // Complex Expression Tests
    // ===========================================================

    #[test]
    fn test_parse_pattern_matching_expression() {
        let s_exprs = lex_source(r#"
            (match x 
                (Nothing 0) 
                ((Just y) y)
            )
        "#);
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Match { expr, arms } => {
                // Expression should be variable 'x'
                match *expr {
                    Expr::Var(IdentPath(ref idents)) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "x");
                            }
                            _ => panic!("Expected VarIdent for match expression"),
                        }
                    }
                    _ => panic!("Expected Var for match expression"),
                }
                
                // Should have two match arms
                assert_eq!(arms.len(), 2);
                
                // First arm: Nothing -> 0
                match &arms[0].pattern {
                    Pattern::Constructor { ctor: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Nothing");
                            }
                            _ => panic!("Expected CtorIdent for Nothing"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Constructor pattern for Nothing"),
                }
                assert_eq!(arms[0].body, Expr::Literal(LiteralExpr::I64(0)));
                
                // Second arm: (Just y) -> y
                match &arms[1].pattern {
                    Pattern::Constructor { ctor: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Just");
                            }
                            _ => panic!("Expected CtorIdent for Just"),
                        }
                        assert_eq!(args.len(), 1);
                        match &args[0] {
                            Pattern::Var(Ident::VarIdent(source_ref)) => {
                                assert_eq!(source_ref.resolve(), "y");
                            }
                            _ => panic!("Expected Var pattern for y"),
                        }
                    }
                    _ => panic!("Expected Constructor pattern for Just"),
                }
                match &arms[1].body {
                    Expr::Var(IdentPath(ref idents)) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "y");
                            }
                            _ => panic!("Expected VarIdent for y"),
                        }
                    }
                    _ => panic!("Expected Var for match arm body"),
                }
            }
            _ => panic!("Expected Match expression"),
        }
    }

    #[test]
    fn test_parse_do_notation() {
        let s_exprs = lex_source(r#"
            (do 
                (x := get_value) 
                (pure x))
        "#);
        let mut ctx = ParseContext::new();
        
        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Do { stmts } => {
                assert_eq!(stmts.len(), 2);
                
                // First statement should be bind
                match &stmts[0] {
                    DoStmt::Bind { pattern, expr: _ } => {
                        match pattern {
                            Pattern::Var(Ident::VarIdent(source_ref)) => {
                                assert_eq!(source_ref.resolve(), "x");
                            }
                            _ => panic!("Expected Var pattern for bind"),
                        }
                    }
                    _ => panic!("Expected Bind statement"),
                }
                
                // Second statement should be expression
                match &stmts[1] {
                    DoStmt::Expr(expr) => {
                        match expr {
                            Expr::App { func: _, args } => {
                                assert_eq!(args.len(), 1);
                            }
                            _ => panic!("Expected App expression in do"),
                        }
                    }
                    _ => panic!("Expected Expr statement"),
                }
            }
            _ => panic!("Expected Do expression"),
        }
    }
}
