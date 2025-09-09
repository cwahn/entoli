use crate::{
    base::{HashMap, RcMut},
    lexer::SExpr,
    source_ref::SourceRef,
};

// ===========================================================
// AST Types - Core Language Constructs
// ===========================================================

/// Identifier types covering operators, keywords, types, and user-defined names; e.g. + in (+ 1 2), if in (if True x y)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    // Built-in operators and symbols
    /// Function type arrow operator; e.g. -> in (: f (Int -> Bool))
    Arrow,

    /// Quote operator for lists; e.g. ' in ('(1 2 3))
    Quote,
    /// Nil value; e.g. Nil
    Nil,
    /// Tuple constructor; e.g. #3 in (#(a b c))
    Hash(u8),

    // Comparison operators
    /// Equality operator; e.g. == in (== x y)
    Eq,
    /// Inequality operator; e.g. != in (!= x y)
    Ne,
    /// Greater than operator; e.g. > in (> x y)
    Gt,
    /// Greater than or equal operator; e.g. >= in (>= x y)
    Ge,
    /// Less than operator; e.g. < in (< x y)
    Lt,
    /// Less than or equal operator; e.g. <= in (<= x y)
    Le,

    // Logical operators
    /// Logical AND operator; e.g. & in (& x y)
    And,
    /// Logical OR operator; e.g. | in (| x y)
    Or,
    /// Logical NOT operator; e.g. ! in (! flag)
    Not,

    // Arithmetic operators
    /// Addition operator; e.g. + in (+ 2 3)
    Add,
    /// Subtraction operator; e.g. - in (- 5 2)
    Sub,
    /// Multiplication operator; e.g. * in (* 4 3)
    Mul,
    /// Division operator; e.g. / in (/ 8 2)
    Div,
    /// Modulus operator; e.g. % in (% 7 3)
    Modulus,

    // Other operators
    /// Dot operator; e.g. . in (. f g)
    Dot,
    // Unit type/value
    /// Unit type and value; e.g. ()
    Unit,

    // Built-in types
    /// Boolean type; e.g. Bool in (: x Bool)
    BoolType,
    /// 64-bit integer type; e.g. I64 in (: count I64)
    I64Type,
    /// 64-bit floating point type; e.g. F64 in (: price F64)
    F64Type,
    /// String type; e.g. Str in (: name Str)
    StrType,

    // User-defined identifiers
    /// Constructor identifier (starts with uppercase); e.g. Just in (Just x)
    CtorIdent(SourceRef),
    /// Variable identifier (starts with lowercase or underscore); e.g. x in (+ x y)
    VarIdent(SourceRef),
    /// Custom operator identifier; e.g. >>= in (>>= m f)
    OpIdent(SourceRef),
    /// Field accessor with dot prefix; e.g. .name in (.name person)
    FieldAccessor(SourceRef),
}

/// Qualified identifier path for module and type access; e.g. Module.function in (Module.function x)
#[derive(Debug, Clone, PartialEq)]
pub struct IdentPath(pub RcMut<Vec<Ident>>);

/// Tree structure for complex use imports; e.g. (collections:: Map Set) in (use (collections:: Map Set))
#[derive(Debug, Clone, PartialEq)]
pub struct IdentTree {
    /// The path prefix for the import; e.g. collections in (collections:: Map Set)
    pub head: IdentPath,
    /// Nested items to import from the path; e.g. [Map, Set] in (collections:: Map Set)
    pub tail: RcMut<Vec<IdentTree>>,
}

// ===========================================================
// Expressions (updated to match parser_draft.rs)
// ===========================================================

/// Expression types in the language; e.g. (+ x 1) in (= result (+ x 1))
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value expression; e.g. 42 in (+ 42 x)
    Literal(LiteralExpr),

    /// Variable reference expression; e.g. x in (+ x 1)
    Var(IdentPath),

    /// Untyped lambda function; e.g. (lambda x (+ x 1))
    Lambda(RcMut<Lambda>),

    /// Lambda following type annotation; e.g. (name : (-> a a)) (= name (lambda x (+ x 1))) without name
    TypedLambda(RcMut<TypedLambda>),

    /// Function application; e.g. (f x y)
    App {
        /// The function being applied; e.g. f in (f x y)
        f: RcMut<Expr>,

        /// Arguments to the function; e.g. [x, y] in (f x y)
        args: Vec<Expr>,
    },

    /// Pattern matching expression; e.g. (match x (Just y) y Nothing 0)
    /// - Will be desugared to application of lambda
    Match {
        /// Expression being matched; e.g. x in (match x ...)
        expr: RcMut<Expr>,

        /// Pattern matching rules; e.g. [(Just y) -> y, Nothing -> 0] in (match x (Just y) y Nothing 0)
        rules: Vec<Rule>,
    },

    /// Monadic do notation; e.g. (do (:= x getValue) (return x))
    /// - Will be desugared to nested applications of bind and return
    Do {
        /// Sequence of do statements; e.g. [(:= x getValue), (return x)] in (do (:= x getValue) (return x))
        stmts: Vec<DoStmt>,
    },
}

/// Literal expressions - primitive values that evaluate to themselves; e.g. 42 in (+ 42 x)
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    /// Unit value; e.g. Unit in (lambda x Unit)
    Unit,

    /// Boolean literal; e.g. True in (if True x y)
    Bool(bool),

    /// 64-bit signed integer literal; e.g. 42 in (+ 42 x)
    I64(i64),

    /// 64-bit floating point literal; e.g. 3.14 in (* 3.14 r)
    F64(f64),

    /// String literal; e.g. "hello" in (print "hello")
    String(String),

    /// Character literal; e.g. 'a' in (isLetter 'a')
    Char(char),
    // Note: List and Tuple are NOT literals - they are constructor applications
    // List: '(1 2 3) -> App { f: List, args: [1, 2, 3] }
    // Tuple: #(a b c) -> App { f: Tuple, args: [a, b, c] }
}

/// Do statement types for monadic computation; e.g. (:= x getValue) in (do (:= x getValue) (return x))
#[derive(Debug, Clone, PartialEq)]
pub enum DoStmt {
    /// Monadic bind statement; e.g. (:= x getValue) in (do (:= x getValue) (return x))
    /// - Will be desugared to application of bind
    Bind {
        /// Pattern to bind result to; e.g. x in (:= x getValue)
        pat: RcMut<Pattern>,

        /// Monadic expression to evaluate; e.g. getValue in (:= x getValue)
        expr: Expr,
    },

    /// Expression statement in do block; e.g. (putStrLn "hello") in (do (putStrLn "hello") (return Unit))
    Expr(Expr),
}

// ===========================================================
// Type Expressions
// ===========================================================

/// Type expressions for type annotations and signatures; e.g. Int in (: x Int)
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    /// Type variable; e.g. a in (: f (a -> a))
    Var(IdentPath),

    /// Type constructor application; e.g. (Maybe a) in (: x (Maybe a))
    Ctor {
        /// Type constructor identifier; e.g. Maybe in (Maybe a)
        ident: IdentPath,

        /// Type arguments; e.g. [a] in (Maybe a)
        args: Vec<TypeExpr>,
    },

    /// Universal quantification (forall); e.g. (forall a (Maybe a)) in (: x (forall a (Maybe a)))
    Forall {
        /// Quantified type parameters; e.g. [a] in (forall a (Maybe a))
        type_params: Vec<Ident>,

        /// Type expression with quantified variables; e.g. (Maybe a) in (forall a (Maybe a))
        expr: RcMut<TypeExpr>,
    },
}

// ===========================================================
// Module Structure (following parser_draft.rs exactly)
// ===========================================================

/// Module item types - top-level definitions in modules; e.g. (= f x (+ x 1)) in module scope
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
    /// Function definition with optional type annotation; e.g. (= factorial (n : Int) ...)
    TypedLambda(RcMut<TypedLambda>),

    /// Data type definition; e.g. (data (Maybe a) Nothing (Just a))
    TypeCtor(RcMut<TypeCtor>),

    /// Trait definition; e.g. (trait (Show a) (show : (a -> String)))
    Trait(RcMut<Trait>),

    /// Nested module definition; e.g. (mod Utils ...)
    Module(RcMut<Module>),

    /// Import/use declaration; e.g. (use std.io)
    Use(UseItem),
}

/// Type constructor item - type pattern for constructor; e.g. Just in (data (Maybe a) Nothing (Just a))
pub type TypeCtorItem = RcMut<TypePattern>;

/// Trait item types for trait definitions; e.g. (show : (a -> String)) in trait definition
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    /// Required associated type with constraints; e.g. (type Key) in trait definition
    RequiredTypeCtor(Vec<TypeExpr>),

    /// Required method signature; e.g. (show : (a -> String)) in trait definition
    RequiredLambda(RcMut<TypePattern>),

    /// Default method implementation; e.g. (= show x (defaultShow x)) in trait definition
    TypedLambda(RcMut<TypedLambda>),

    /// Associated type definition with default; e.g. (type Key = String) in trait definition
    TypeCtor(RcMut<TypeCtor>),
}

// ===========================================================
// Core Structures (following parser_draft.rs design exactly)
// ===========================================================

/// Module containing named items with scope resolution; e.g. (mod MyMod ...) creates Module with scope mapping names to items
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub scope: HashMap<Ident, ModuleItem>, // Name -> Item mapping
}

/// Function with optional explicit type annotation; e.g. (: add (-> Int (-> Int Int) (= add (x y) (+ x y)))
#[derive(Debug, Clone, PartialEq)]
pub struct TypedLambda {
    pub type_pattern: RcMut<TypePattern>, // Optional type annotation
    pub lambda: Lambda,                   // The actual function
}

/// Data type constructor definition; e.g. (data (Maybe a) Nothing (Just a))
#[derive(Debug, Clone, PartialEq)]
pub struct TypeCtor {
    pub type_params_pattern: RcMut<TypePattern>, // Type parameters with constraints
    pub scope: HashMap<Ident, TypeCtorItem>,     // Constructor name -> Pattern mapping
}

/// Trait definition with required and provided items; e.g. (trait (Show a) (show : (a -> String)))
#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub type_params_pattern: RcMut<TypePattern>, // Type parameters with constraints
    pub scope: HashMap<Ident, TraitItem>,        // Item name -> TraitItem mapping
}

// ===========================================================
// Lambda and Rule System (from parser_draft.rs)
// ===========================================================

/// Function implementation - either single rule or multiple rules; e.g. (lambda x (+ x 1)) uses Mono, pattern matching uses Poly
#[derive(Debug, Clone, PartialEq)]
pub enum Lambda {
    Mono(RcMut<Rule>), // Single pattern-matching rule
    Poly(Vec<Rule>),   // Multiple pattern-matching rules
}

/// Single pattern-matching rule in a function; e.g. x (+ x 1) in lambda definition
#[derive(Debug, Clone, PartialEq)]
pub struct Rule {
    pub pattern: RcMut<Pattern>, // Pattern to match
    pub expr: RcMut<Expr>,       // Expression to evaluate
}

/// Pattern with optional guard conditions; e.g. x in (lambda x (+ x 1)) or (x | (> x 0)) with guard
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub pats: RcMut<Pats>, // The actual patterns (may be unparsed initially)
    pub guard: Vec<Expr>,  // Guard conditions
}

/// Pattern container - unparsed until arity information available; e.g. Unparsed((x y z)) or Parsed([x, y, z])
#[derive(Debug, Clone, PartialEq)]
pub enum Pats {
    Unparsed(RcMut<SExpr>), // Unparsed S-expression (need arity info to parse)
    Parsed(Vec<Pattern>),   // Parsed pattern list
}

/// Individual pattern element in value-level matching; e.g. x in (lambda x (+ x 1)) or (Just x) in pattern match
#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Lit(LiteralExpr),                          // Literal pattern
    Var(Ident),                                // Variable pattern
    ListVar(Ident),                            // List variable pattern like xs..
    Ctor { ident: IdentPath, args: Vec<Pat> }, // Constructor pattern
    Wildcard,                                  // _ pattern
    Ellipsis,                                  // .. pattern
}

/// Type-level pattern with constraints; e.g. (a : Show) in trait definitions or (Maybe a) in type signatures
#[derive(Debug, Clone, PartialEq)]
pub struct TypePattern {
    pub pats: RcMut<TypePats>, // Type patterns
    pub guard: Vec<TypeExpr>,  // Type constraints
}

/// Type pattern container - unparsed until context available; e.g. Unparsed((Maybe a)) or Parsed([Maybe, a])
#[derive(Debug, Clone, PartialEq)]
pub enum TypePats {
    Unparsed(RcMut<SExpr>), // Unparsed S-expression, Since airity of type patterns might not be known yet
    Parsed(Vec<TypePattern>), // Parsed type pattern list
}

/// Individual type pattern element; e.g. a in type variables or (Maybe a) in type constructor applications
#[derive(Debug, Clone, PartialEq)]
pub enum TypePat {
    Var(Ident), // Type variable
    Ctor {
        ident: IdentPath,
        args: Vec<TypePat>,
    }, // Type constructor
    Wildcard,   // _ in types
}

// ===========================================================
// Use/Import System (following syntax-design.lisp)
// ===========================================================

/// Import declaration supporting simple names, paths, and selective imports; e.g. (use math) or (use (collections:: Map Set))
#[derive(Debug, Clone, PartialEq)]
pub enum UseItem {
    Ident(Ident),         // math in (use math)
    IdentPath(IdentPath), // math::statistics in (use math::statistics)
    IdentTree(IdentTree), // (collections:: Map Set) in (use (collections:: Map Set))
}

/// Constructor definition with name and positional fields; e.g. (Person String Int) in data type definition
#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: Ident,
    pub fields: Vec<ConstructorField>,
}

/// Constructor field with name and type; e.g. (name : String) in constructor definition
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorField {
    pub name: Ident,
    pub type_expr: TypeExpr,
}

// ===========================================================
// Kinds
// ===========================================================

/// Kind system for type-level computation; e.g. * for base types or (* -> *) for type constructors
#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    /// Base kind for concrete types; e.g. * for Int, Bool
    Star,
    /// Function kind for type constructors; e.g. (* -> *) for Maybe, List
    Arrow { param: Box<Kind>, result: Box<Kind> },
    /// Constraint kind for type class constraints; e.g. for Show, Eq constraints
    Constraint,
}

// ===========================================================
// Parser Context and Functions
// ===========================================================

/// Parser context for maintaining state during parsing; e.g. used in parse_module function
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

// Parse a single module item from an S-expression
pub fn parse_module_item(_ctx: &mut ParseContext, _s_expr: &SExpr) -> Result<ModuleItem, String> {
    // TODO: Implement parsing for new AST structure
    // For now, return error to get code compiling
    Err("parse_module_item not implemented for new AST structure".to_string())
}

// Parse a literal expression - assumes s_expr is already known to be a literal
pub fn parse_literal_expr(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<LiteralExpr, String> {
    match s_expr {
        SExpr::Unit => Ok(LiteralExpr::Unit),
        SExpr::Bool(value) => Ok(LiteralExpr::Bool(*value)),
        SExpr::Char(value) => Ok(LiteralExpr::Char(*value)),
        SExpr::I64(value) => Ok(LiteralExpr::I64(*value)),
        SExpr::F64(value) => Ok(LiteralExpr::F64(*value)),
        SExpr::String(src_ref) => Ok(LiteralExpr::String(src_ref.resolve().to_string())),
        _ => Err(format!("Expected literal, got {s_expr:?}")),
    }
}

pub fn parse_var_expr(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<IdentPath, String> {
    match s_expr {
        // Variable identifiers
        SExpr::VarIdent(var_ident) => Ok(IdentPath(RcMut::new(vec![Ident::VarIdent(
            var_ident.0.clone(),
        )]))),

        // Constructor identifiers (including special True/False which we handle elsewhere)
        SExpr::CtorIdent(ctor_ident) => Ok(IdentPath(RcMut::new(vec![Ident::CtorIdent(
            ctor_ident.0.clone(),
        )]))),

        // Operators as identifiers
        SExpr::Add => Ok(IdentPath(RcMut::new(vec![Ident::Add]))),
        SExpr::Sub => Ok(IdentPath(RcMut::new(vec![Ident::Sub]))),
        SExpr::Asterisk => Ok(IdentPath(RcMut::new(vec![Ident::Mul]))),
        SExpr::Div => Ok(IdentPath(RcMut::new(vec![Ident::Div]))),
        SExpr::Modulus => Ok(IdentPath(RcMut::new(vec![Ident::Modulus]))),
        SExpr::Eq => Ok(IdentPath(RcMut::new(vec![Ident::Eq]))),
        SExpr::Ne => Ok(IdentPath(RcMut::new(vec![Ident::Ne]))),
        SExpr::Lt => Ok(IdentPath(RcMut::new(vec![Ident::Lt]))),
        SExpr::Le => Ok(IdentPath(RcMut::new(vec![Ident::Le]))),
        SExpr::Gt => Ok(IdentPath(RcMut::new(vec![Ident::Gt]))),
        SExpr::Ge => Ok(IdentPath(RcMut::new(vec![Ident::Ge]))),
        SExpr::And => Ok(IdentPath(RcMut::new(vec![Ident::And]))),
        SExpr::Or => Ok(IdentPath(RcMut::new(vec![Ident::Or]))),
        SExpr::Not => Ok(IdentPath(RcMut::new(vec![Ident::Not]))),
        SExpr::Arrow => Ok(IdentPath(RcMut::new(vec![Ident::Arrow]))),

        // Identifier paths
        SExpr::IdentPath(path_elems) => {
            let mut idents = Vec::new();
            for elem in path_elems.iter() {
                let ident_path = parse_var_expr(_ctx, &elem.get())?;
                // Extract the single ident from the path (since individual path components are single idents)
                idents.extend(ident_path.0.get().iter().cloned());
            }
            Ok(IdentPath(RcMut::new(idents)))
        }

        _ => Err(format!("Expected variable/identifier, got {:?}", s_expr)),
    }
}

pub fn parse_pat(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<Pat, String> {
    match s_expr {
        // Literal patterns - follows Pat enum order
        // Try parsing as literal first
        _ if parse_literal_expr(_ctx, s_expr).is_ok() => {
            let literal = parse_literal_expr(_ctx, s_expr)?;
            Ok(Pat::Lit(literal))
        }

        // Variable patterns
        SExpr::VarIdent(var_ident) => Ok(Pat::Var(Ident::VarIdent(var_ident.0.clone()))),

        // List variable patterns (like xs..)
        SExpr::ListIdent(list_ident) => Ok(Pat::ListVar(Ident::VarIdent(list_ident.0.clone()))),

        // Constructor patterns
        SExpr::CtorIdent(ctor_ident) => {
            // Simple constructor with no arguments
            let ident_path = IdentPath(RcMut::new(vec![Ident::CtorIdent(ctor_ident.0.clone())]));
            Ok(Pat::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }

        // Constructor patterns with arguments
        SExpr::List(elems) => {
            if elems.is_empty() {
                return Err("Empty list in pattern".to_string());
            }

            match &*elems[0].get() {
                SExpr::CtorIdent(ctor_ident) => {
                    let ident_path =
                        IdentPath(RcMut::new(vec![Ident::CtorIdent(ctor_ident.0.clone())]));
                    let mut args = Vec::new();

                    for i in 1..elems.len() {
                        args.push(parse_pat(_ctx, &elems[i].get())?);
                    }

                    Ok(Pat::Ctor {
                        ident: ident_path,
                        args,
                    })
                }
                _ => Err(format!(
                    "Expected constructor in pattern list, got {:?}",
                    elems[0].get()
                )),
            }
        }

        // Wildcard pattern
        SExpr::Wildcard => Ok(Pat::Wildcard),

        // Ellipsis pattern
        SExpr::Ellipsis => Ok(Pat::Ellipsis),

        _ => Err(format!("Expected pattern, got {:?}", s_expr)),
    }
}

// Parse a type expression from an S-expression
pub fn parse_type_expr(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<TypeExpr, String> {
    match s_expr {
        // Type variables (lowercase identifiers)
        SExpr::VarIdent(var_ident) => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::VarIdent(var_ident.0.clone())]));
            Ok(TypeExpr::Var(ident_path))
        }

        // Type constructors (uppercase identifiers)
        SExpr::CtorIdent(ctor_ident) => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::CtorIdent(ctor_ident.0.clone())]));
            Ok(TypeExpr::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }

        // Built-in types
        SExpr::I64Type => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::I64Type]));
            Ok(TypeExpr::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }
        SExpr::F64Type => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::F64Type]));
            Ok(TypeExpr::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }
        SExpr::BoolType => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::BoolType]));
            Ok(TypeExpr::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }
        SExpr::StrType => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::StrType]));
            Ok(TypeExpr::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }
        SExpr::Unit => {
            let ident_path = IdentPath(RcMut::new(vec![Ident::Unit]));
            Ok(TypeExpr::Ctor {
                ident: ident_path,
                args: vec![],
            })
        }

        // List types - function types (arrow), parameterized types
        SExpr::List(elems) => {
            if elems.is_empty() {
                return Err("Empty list type not allowed".to_string());
            }

            let first = &*elems[0].get();
            match first {
                // Arrow types represent function types, but in the simplified model
                // we treat them as constructor applications
                SExpr::Arrow => {
                    let ident_path = IdentPath(RcMut::new(vec![Ident::Arrow]));
                    let mut args = Vec::new();

                    // Parse type arguments
                    for i in 1..elems.len() {
                        args.push(parse_type_expr(_ctx, &elems[i].get())?);
                    }

                    Ok(TypeExpr::Ctor {
                        ident: ident_path,
                        args,
                    })
                }

                // Parameterized type constructors
                SExpr::CtorIdent(ctor_ident) => {
                    let ident_path =
                        IdentPath(RcMut::new(vec![Ident::CtorIdent(ctor_ident.0.clone())]));
                    let mut args = Vec::new();

                    // Parse type arguments
                    for i in 1..elems.len() {
                        args.push(parse_type_expr(_ctx, &elems[i].get())?);
                    }

                    Ok(TypeExpr::Ctor {
                        ident: ident_path,
                        args,
                    })
                }

                _ => {
                    return Err(format!("Unsupported list type starting with {:?}", first));
                }
            }
        }

        _ => {
            unimplemented!("parse_type_expr for {:?} not implemented yet", s_expr)
        }
    }
}

/// Parse a type pattern following TypePat enum variant order
fn parse_type_pat(elems: &[RcMut<SExpr>]) -> Result<TypePat, String> {
    if elems.is_empty() {
        return Err("Expected type pattern".to_string());
    }

    match &*elems[0].get() {
        // TypePat::Wildcard - underscore
        SExpr::Wildcard if elems.len() == 1 => Ok(TypePat::Wildcard),

        // TypePat::Var - variable identifier as type variable
        SExpr::VarIdent(var_ident) if elems.len() == 1 => {
            Ok(TypePat::Var(Ident::VarIdent(var_ident.0.clone())))
        }

        // TypePat::Ctor - constructor identifier with optional arguments
        SExpr::CtorIdent(ctor_ident) => {
            if elems.len() == 1 {
                // Simple constructor without arguments
                Ok(TypePat::Ctor {
                    ident: IdentPath(RcMut::new(vec![Ident::CtorIdent(ctor_ident.0.clone())])),
                    args: vec![],
                })
            } else {
                // Constructor with arguments
                let mut args = vec![];
                for elem in &elems[1..] {
                    match &*elem.get() {
                        SExpr::List(arg_elems) => {
                            args.push(parse_type_pat(arg_elems)?);
                        }
                        _ => {
                            // Single element argument
                            args.push(parse_type_pat(&[elem.clone()])?);
                        }
                    }
                }
                Ok(TypePat::Ctor {
                    ident: IdentPath(RcMut::new(vec![Ident::CtorIdent(ctor_ident.0.clone())])),
                    args,
                })
            }
        }

        _ => Err("Invalid type pattern".to_string()),
    }
}

/// Parse patterns (Pats) - the pattern list part of Pattern struct
fn parse_pats(elems: &[RcMut<SExpr>]) -> Result<Pats, String> {
    if elems.is_empty() {
        return Err("Expected patterns".to_string());
    }

    if elems.len() == 1 {
        // Single pattern element - keep as unparsed for later arity resolution
        Ok(Pats::Unparsed(elems[0].clone()))
    } else {
        // Multiple pattern elements - keep as unparsed list for later arity resolution
        Ok(Pats::Unparsed(RcMut::new(SExpr::List(
            elems.to_vec().into(),
        ))))
    }
}

/// Parse a pattern with optional guards following Pattern struct requirements
/// Syntax: pattern elements may include (where guard-expr)
fn parse_pattern_with_guards(elems: &[RcMut<SExpr>]) -> Result<Pattern, String> {
    if elems.is_empty() {
        return Err("Expected pattern".to_string());
    }

    // Look for guard - a list starting with "where"
    let mut guard_index = None;
    for (i, elem) in elems.iter().enumerate() {
        match &*elem.get() {
            SExpr::List(guard_elems) if !guard_elems.is_empty() => {
                if let SExpr::Where = &*guard_elems[0].get() {
                    guard_index = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }

    let (pattern_elems, guard_elem) = if let Some(idx) = guard_index {
        // Split at guard: pattern parts before guard, guard element
        (&elems[..idx], Some(&elems[idx]))
    } else {
        // No guards, all elements are patterns
        (elems, None)
    };

    // Parse the patterns part
    let pats = RcMut::new(parse_pats(pattern_elems)?);

    // Parse guard expression if present
    let guard = if let Some(guard_elem) = guard_elem {
        match &*guard_elem.get() {
            SExpr::List(guard_list) if !guard_list.is_empty() => {
                if let SExpr::Where = &*guard_list[0].get() {
                    // Parse the guard expression(s) after "where"
                    let mut guards = vec![];
                    for expr_elem in &guard_list[1..] {
                        // For now, create placeholder expressions for guards
                        // In a full implementation, we'd call parse_expr here
                        match &*expr_elem.get() {
                            SExpr::I64(n) => guards.push(Expr::Literal(LiteralExpr::I64(*n))),
                            SExpr::Bool(b) => guards.push(Expr::Literal(LiteralExpr::Bool(*b))),
                            SExpr::VarIdent(var_ident) => {
                                guards.push(Expr::Var(IdentPath(RcMut::new(vec![
                                    Ident::VarIdent(var_ident.0.clone()),
                                ]))));
                            }
                            _ => {
                                // Complex guard expressions - placeholder for now
                                guards.push(Expr::Literal(LiteralExpr::Bool(true)));
                                // Placeholder
                            }
                        }
                    }
                    guards
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    } else {
        vec![]
    };

    Ok(Pattern { pats, guard })
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
    // New ModuleItem Tests
    // ===========================================================

    #[test]
    fn test_basic_compilation() {
        // Just test that we can create basic AST nodes
        let _module = Module {
            scope: HashMap::default(),
        };

        // Test TypedLambda creation
        let type_pattern = TypePattern {
            pats: RcMut::new(TypePats::Parsed(vec![])),
            guard: vec![],
        };

        let rule = Rule {
            pattern: RcMut::new(Pattern {
                pats: RcMut::new(Pats::Parsed(vec![])),
                guard: vec![],
            }),
            expr: RcMut::new(Expr::Literal(LiteralExpr::I64(42))),
        };

        let lambda = Lambda::Mono(RcMut::new(rule));

        let _typed_lambda = TypedLambda {
            type_pattern: RcMut::new(type_pattern),
            lambda,
        };

        // If we get here, the basic structure compiles
        assert!(true);
    }

    #[test]
    fn test_parse_literal_expr() {
        let mut ctx = ParseContext::new();

        // Test Unit - follows LiteralExpr order
        let unit_sexprs = lex_source("()");
        let unit_res = parse_literal_expr(&mut ctx, &unit_sexprs[0].get());
        assert_eq!(unit_res, Ok(LiteralExpr::Unit));

        // Test Bool true
        let bool_true_sexprs = lex_source("true");
        let bool_true_res = parse_literal_expr(&mut ctx, &bool_true_sexprs[0].get());
        assert_eq!(bool_true_res, Ok(LiteralExpr::Bool(true)));

        // Test Bool false
        let bool_false_sexprs = lex_source("false");
        let bool_false_res = parse_literal_expr(&mut ctx, &bool_false_sexprs[0].get());
        assert_eq!(bool_false_res, Ok(LiteralExpr::Bool(false)));

        // Test I64
        let i64_sexprs = lex_source("42");
        let i64_res = parse_literal_expr(&mut ctx, &i64_sexprs[0].get());
        assert_eq!(i64_res, Ok(LiteralExpr::I64(42)));

        // Test F64
        let f64_sexprs = lex_source("3.14");
        let f64_res = parse_literal_expr(&mut ctx, &f64_sexprs[0].get());
        assert_eq!(f64_res, Ok(LiteralExpr::F64(3.14)));

        // Test String
        let string_sexprs = lex_source("\"hello\"");
        let string_res = parse_literal_expr(&mut ctx, &string_sexprs[0].get());
        assert_eq!(string_res, Ok(LiteralExpr::String("hello".to_string())));

        // Test Char
        let char_sexprs = lex_source("'x'");
        let char_res = parse_literal_expr(&mut ctx, &char_sexprs[0].get());
        assert_eq!(char_res, Ok(LiteralExpr::Char('x')));

        // Test error case - non-literal
        let non_literal_sexprs = lex_source("foo");
        let error_res = parse_literal_expr(&mut ctx, &non_literal_sexprs[0].get());
        assert!(error_res.is_err());

        println!("All literal expression tests passed!");
    }

    #[test]
    fn test_parse_var_expr() {
        let mut ctx = ParseContext::new();

        // Test variable identifier (snake_case)
        let var_exprs = lex_source("some_var");
        let var_res = parse_var_expr(&mut ctx, &var_exprs[0].get());
        match var_res {
            Ok(IdentPath(idents)) => {
                let idents_vec = idents.get();
                assert_eq!(idents_vec.len(), 1);
                match &idents_vec[0] {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "some_var");
                    }
                    _ => panic!("Expected VarIdent"),
                }
            }
            _ => panic!("Expected successful IdentPath"),
        }

        // Test constructor identifier (PascalCase)
        let ctor_exprs = lex_source("SomeType");
        let ctor_res = parse_var_expr(&mut ctx, &ctor_exprs[0].get());
        match ctor_res {
            Ok(IdentPath(idents)) => {
                let idents_vec = idents.get();
                assert_eq!(idents_vec.len(), 1);
                match &idents_vec[0] {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "SomeType");
                    }
                    _ => panic!("Expected CtorIdent"),
                }
            }
            _ => panic!("Expected successful IdentPath"),
        }

        // Test operator
        let add_exprs = lex_source("+");
        let add_res = parse_var_expr(&mut ctx, &add_exprs[0].get());
        match add_res {
            Ok(IdentPath(idents)) => {
                let idents_vec = idents.get();
                assert_eq!(idents_vec.len(), 1);
                assert_eq!(idents_vec[0], Ident::Add);
            }
            _ => panic!("Expected successful IdentPath for Add"),
        }

        // Test error case - literal should not parse as variable
        let literal_exprs = lex_source("42");
        let error_res = parse_var_expr(&mut ctx, &literal_exprs[0].get());
        assert!(error_res.is_err());

        println!("All variable expression tests passed!");
    }

    #[test]
    fn test_parse_pat() {
        let mut ctx = ParseContext::new();

        // Test literal patterns - follows Pat enum order
        let i64_exprs = lex_source("42");
        let i64_res = parse_pat(&mut ctx, &i64_exprs[0].get());
        assert_eq!(i64_res, Ok(Pat::Lit(LiteralExpr::I64(42))));

        let string_exprs = lex_source("\"hello\"");
        let string_res = parse_pat(&mut ctx, &string_exprs[0].get());
        assert_eq!(
            string_res,
            Ok(Pat::Lit(LiteralExpr::String("hello".to_string())))
        );

        // Test variable patterns
        let var_exprs = lex_source("some_var");
        let var_res = parse_pat(&mut ctx, &var_exprs[0].get());
        match var_res {
            Ok(Pat::Var(Ident::VarIdent(source_ref))) => {
                assert_eq!(source_ref.resolve(), "some_var");
            }
            _ => panic!("Expected Var pattern"),
        }

        // Test list variable patterns
        let list_var_exprs = lex_source("xs..");
        let list_var_res = parse_pat(&mut ctx, &list_var_exprs[0].get());
        match list_var_res {
            Ok(Pat::ListVar(Ident::VarIdent(source_ref))) => {
                assert_eq!(source_ref.resolve(), "xs..");
            }
            _ => panic!("Expected ListVar pattern"),
        }

        // Test simple constructor patterns
        let ctor_exprs = lex_source("Nothing");
        let ctor_res = parse_pat(&mut ctx, &ctor_exprs[0].get());
        match ctor_res {
            Ok(Pat::Ctor { ident, args }) => {
                let idents = ident.0.get();
                assert_eq!(idents.len(), 1);
                match &idents[0] {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Nothing");
                    }
                    _ => panic!("Expected CtorIdent"),
                }
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected Ctor pattern"),
        }

        // Test constructor patterns with arguments
        let ctor_with_args_sexprs = lex_source("(Just x)");
        let ctor_with_args_res = parse_pat(&mut ctx, &ctor_with_args_sexprs[0].get());
        match ctor_with_args_res {
            Ok(Pat::Ctor { ident, args }) => {
                let idents = ident.0.get();
                assert_eq!(idents.len(), 1);
                match &idents[0] {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Just");
                    }
                    _ => panic!("Expected CtorIdent"),
                }
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Pat::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern argument"),
                }
            }
            _ => panic!("Expected Ctor pattern with args"),
        }

        // Test wildcard pattern
        let wildcard_exprs = lex_source("_");
        let wildcard_res = parse_pat(&mut ctx, &wildcard_exprs[0].get());
        assert_eq!(wildcard_res, Ok(Pat::Wildcard));

        // Test ellipsis pattern
        let ellipsis_exprs = lex_source("..");
        let ellipsis_res = parse_pat(&mut ctx, &ellipsis_exprs[0].get());
        assert_eq!(ellipsis_res, Ok(Pat::Ellipsis));

        println!("All pattern tests passed!");
    }

    #[test]
    fn test_parse_type_pat() {
        // Test TypePat::Wildcard
        let wildcard_exprs = lex_source("_");
        let wildcard_result = parse_type_pat(&[wildcard_exprs[0].clone()]);
        assert_eq!(wildcard_result, Ok(TypePat::Wildcard));

        // Test TypePat::Var (type variable)
        let var_exprs = lex_source("a");
        let var_result = parse_type_pat(&[var_exprs[0].clone()]);
        match var_result {
            Ok(TypePat::Var(Ident::VarIdent(source_ref))) => {
                assert_eq!(source_ref.resolve(), "a");
            }
            _ => panic!("Expected TypePat::Var"),
        }

        // Test TypePat::Ctor without arguments
        let ctor_simple_exprs = lex_source("Maybe");
        let ctor_simple_result = parse_type_pat(&[ctor_simple_exprs[0].clone()]);
        match ctor_simple_result {
            Ok(TypePat::Ctor { ident, args }) => {
                let idents = ident.0.get();
                assert_eq!(idents.len(), 1);
                match &idents[0] {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Maybe");
                    }
                    _ => panic!("Expected CtorIdent"),
                }
                assert!(args.is_empty());
            }
            _ => panic!("Expected TypePat::Ctor"),
        }

        // Test TypePat::Ctor with arguments (nested type patterns)
        let ctor_with_args_exprs = lex_source("(Maybe a)");
        let ctor_with_args_list = match &*ctor_with_args_exprs[0].get() {
            SExpr::List(elems) => elems.clone(),
            _ => panic!("Expected list"),
        };
        let ctor_with_args_result = parse_type_pat(&ctor_with_args_list);
        match ctor_with_args_result {
            Ok(TypePat::Ctor { ident, args }) => {
                let idents = ident.0.get();
                assert_eq!(idents.len(), 1);
                match &idents[0] {
                    Ident::CtorIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "Maybe");
                    }
                    _ => panic!("Expected CtorIdent"),
                }
                assert_eq!(args.len(), 1);
                match &args[0] {
                    TypePat::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "a");
                    }
                    _ => panic!("Expected TypePat::Var argument"),
                }
            }
            _ => panic!("Expected TypePat::Ctor with args"),
        }

        println!("All type pattern tests passed!");
    }

    #[test]
    fn test_parse_pats() {
        // Test nullary (empty) - should error
        let empty_result = parse_pats(&[]);
        assert!(empty_result.is_err());

        // Test unary Pats (single pattern) - no parentheses
        let unary_exprs = lex_source("x");
        let unary_result = parse_pats(&unary_exprs);
        match unary_result {
            Ok(Pats::Unparsed(_)) => {
                // Expected - single patterns remain unparsed for arity resolution
            }
            _ => panic!("Expected Unparsed for unary pattern"),
        }

        // Test binary Pats (pattern list in parentheses)
        let binary_exprs = lex_source("(x y)");
        let binary_list = match &*binary_exprs[0].get() {
            SExpr::List(elems) => elems.clone(),
            _ => panic!("Expected list for binary pattern"),
        };
        let binary_result = parse_pats(&binary_list);
        match binary_result {
            Ok(Pats::Unparsed(_)) => {
                // Expected - multiple patterns remain unparsed for arity resolution
            }
            _ => panic!("Expected Unparsed for binary patterns"),
        }

        // Test nested Pats (constructor pattern)
        let nested_exprs = lex_source("(Just x)");
        let nested_result = parse_pats(&[nested_exprs[0].clone()]);
        match nested_result {
            Ok(Pats::Unparsed(_)) => {
                // Expected - constructor patterns remain unparsed for arity resolution
            }
            _ => panic!("Expected Unparsed for nested patterns"),
        }

        println!("All Pats tests passed!");
    }

    #[test]
    fn test_parse_guards() {
        // Test one guard predicate
        let one_guard_exprs = lex_source("x (where condition)");
        let one_guard_result = parse_pattern_with_guards(&one_guard_exprs);
        match one_guard_result {
            Ok(Pattern { pats: _, guard }) => {
                assert_eq!(guard.len(), 1);
                match &guard[0] {
                    Expr::Var(IdentPath(idents)) => {
                        let idents_vec = idents.get();
                        assert_eq!(idents_vec.len(), 1);
                        match &idents_vec[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "condition");
                            }
                            _ => panic!("Expected VarIdent for guard"),
                        }
                    }
                    _ => panic!("Expected Var expression for single guard"),
                }
            }
            _ => panic!("Expected successful Pattern with one guard"),
        }

        // Test two guard predicates
        let two_guard_exprs = lex_source("x (where pred1 pred2)");
        let two_guard_result = parse_pattern_with_guards(&two_guard_exprs);
        match two_guard_result {
            Ok(Pattern { pats: _, guard }) => {
                assert_eq!(guard.len(), 2);
                // First guard
                match &guard[0] {
                    Expr::Var(IdentPath(idents)) => {
                        let idents_vec = idents.get();
                        assert_eq!(idents_vec.len(), 1);
                        match &idents_vec[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "pred1");
                            }
                            _ => panic!("Expected VarIdent for first guard"),
                        }
                    }
                    _ => panic!("Expected Var expression for first guard"),
                }
                // Second guard
                match &guard[1] {
                    Expr::Var(IdentPath(idents)) => {
                        let idents_vec = idents.get();
                        assert_eq!(idents_vec.len(), 1);
                        match &idents_vec[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "pred2");
                            }
                            _ => panic!("Expected VarIdent for second guard"),
                        }
                    }
                    _ => panic!("Expected Var expression for second guard"),
                }
            }
            _ => panic!("Expected successful Pattern with two guards"),
        }

        println!("All guard tests passed!");
    }

    #[test]
    fn test_parse_pattern_with_guards() {
        // Test one expr without guard (unary pattern)
        let one_expr_no_guard = lex_source("x");
        let one_result = parse_pattern_with_guards(&one_expr_no_guard);
        match one_result {
            Ok(Pattern { pats, guard }) => {
                match &*pats.get() {
                    Pats::Unparsed(_) => {
                        // Expected - single pattern
                    }
                    _ => panic!("Expected Unparsed for single expr"),
                }
                assert_eq!(guard.len(), 0);
            }
            _ => panic!("Expected successful one expr Pattern without guard"),
        }

        // Test one expr with guard (unary pattern with guard)
        let one_expr_with_guard = lex_source("x (where condition)");
        let one_guard_result = parse_pattern_with_guards(&one_expr_with_guard);
        match one_guard_result {
            Ok(Pattern { pats, guard }) => {
                match &*pats.get() {
                    Pats::Unparsed(_) => {
                        // Expected - single pattern
                    }
                    _ => panic!("Expected Unparsed for single expr with guard"),
                }
                assert_eq!(guard.len(), 1);
            }
            _ => panic!("Expected successful one expr Pattern with guard"),
        }

        // Test two expr without guard (binary pattern)
        let two_expr_no_guard = lex_source("(x y)");
        let two_expr_list = match &*two_expr_no_guard[0].get() {
            SExpr::List(elems) => elems.clone(),
            _ => panic!("Expected list for binary pattern"),
        };
        let two_result = parse_pattern_with_guards(&two_expr_list);
        match two_result {
            Ok(Pattern { pats, guard }) => {
                match &*pats.get() {
                    Pats::Unparsed(_) => {
                        // Expected - binary pattern
                    }
                    _ => panic!("Expected Unparsed for two expr"),
                }
                assert_eq!(guard.len(), 0);
            }
            _ => panic!("Expected successful two expr Pattern without guard"),
        }

        // Test two expr with guard (binary pattern with guard)
        let two_expr_with_guard = lex_source("(x y) (where condition)");
        let two_guard_result = parse_pattern_with_guards(&two_expr_with_guard);
        match two_guard_result {
            Ok(Pattern { pats, guard }) => {
                match &*pats.get() {
                    Pats::Unparsed(_) => {
                        // Expected - binary pattern
                    }
                    _ => panic!("Expected Unparsed for two expr with guard"),
                }
                assert_eq!(guard.len(), 1);
            }
            _ => panic!("Expected successful two expr Pattern with guard"),
        }

        println!("All pattern with guards tests passed!");
    }
}
