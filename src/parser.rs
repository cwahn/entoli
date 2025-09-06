use crate::{
    base::{HashMap, RcMut},
    lexer::SExpr,
    source_ref::SourceRef,
};

// ===========================================================
// AST Types - Core Language Constructs
// ===========================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    // Built-in operators and symbols
    Arrow,    // -> in (Int -> Bool)
    Quote,    // ' in '(1 2 3)
    Nil,      // Nil value
    Hash(u8), // #2 in #(a b) tuples

    // Arithmetic operators
    Add,     // + in (2 + 3)
    Sub,     // - in (5 - 2)
    Mul,     // * in (4 * 3)
    Div,     // / in (8 / 2)
    Modulus, // % in (7 % 3)

    // Comparison operators
    Eq, // == in (x == y)
    Ne, // != in (x != y)
    Gt, // > in (x > y)
    Ge, // >= in (x >= y)
    Lt, // < in (x < y)
    Le, // <= in (x <= y)

    // Logical operators
    And, // & in (x & y)
    Or,  // | in (x | y)
    Not, // ! in (!flag)

    // Other operators
    Dot, // . in obj.field

    // Unit type/value
    Unit, // () unit value

    // Built-in types
    BoolType, // Bool type
    I64Type,  // I64 type
    F64Type,  // F64 type
    StrType,  // Str type

    // User-defined identifiers
    CtorIdent(SourceRef),     // Just, Nothing, Person
    VarIdent(SourceRef),      // x, myVar, _private
    OpIdent(SourceRef),       // custom operators
    FieldAccessor(SourceRef), // .name, .age
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentPath(pub RcMut<Vec<Ident>>); // Module.function, MyType.field

#[derive(Debug, Clone, PartialEq)]
pub struct IdentTree {
    pub head: IdentPath,
    pub tail: RcMut<Vec<IdentTree>>,
} // (collections:: Map Set) in use statements

// ===========================================================
// Expressions (updated to match parser_draft.rs)
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Core expressions from parser_draft
    Literal(LiteralExpr), // 42, "hello", True
    Var(IdentPath),       // x, myFunc, Module.function

    Lambda(RcMut<Lambda>),           // Untyped lambda
    TypedLambda(RcMut<TypedLambda>), // Typed lambda

    App {
        f: RcMut<Expr>,
        args: Vec<Expr>,
    }, // (f x y), (+ 1 2)

    // Extended expressions (improvements over parser_draft)
    // Note: parser_draft suggests these can be desugared:
    // - Match can be expressed as application of lambda
    // - Do can be desugared to regular monadic expressions

    // Pattern matching (can be desugared to lambda application)
    // Uses Rule from parser_draft instead of MatchArm
    Match {
        expr: RcMut<Expr>,
        rules: Vec<Rule>, // Use Rule instead of MatchArm
    }, // (match x (Just y) y Nothing 0)

    // Do notation (can be desugared to monadic expressions)
    Do {
        stmts: Vec<DoStmt>,
    }, // (do (:= x getValue) (return (+ x 1)))
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Unit,           // ()
    Bool(bool),     // True, False
    I64(i64),       // 42, -17
    F64(f64),       // 3.14, -2.5
    String(String), // "hello world"
    Char(char),     // 'a', '\n'
                    // Note: List and Tuple are NOT literals - they are constructor data
                    // List: '(1 2 3) -> App { f: List, args: [1, 2, 3] }
                    // Tuple: #(a b c) -> App { f: Tuple, args: [a, b, c] }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DoStmt {
    Bind { pat: RcMut<Pattern>, expr: Expr }, // (:= x getValue)
    Expr(Expr),                               // (putStrLn "hello")
}

// ===========================================================
// Type Expressions
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Var(IdentPath), // a, b (simple type variables, NOT field access)
    Ctor {
        ident: IdentPath,
        args: Vec<TypeExpr>,
    }, // Int, (Maybe a), (List String)
    Forall {
        type_params: Vec<Ident>,
        expr: RcMut<TypeExpr>,
    }, // (forall a (Maybe a))
}

// ===========================================================
// Module Structure (following parser_draft.rs exactly)
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
    TypedLambda(RcMut<TypedLambda>), // Function definitions with optional type annotations
    TypeCtor(RcMut<TypeCtor>),       // (data Name ...) - data type definition
    Trait(RcMut<Trait>),             // (trait Name ...) - trait definition
    Module(RcMut<Module>),           // (mod Name ...) - nested module
    Use(UseItem),                    // (use ...) - use declaration (improvement over parser_draft)
}

pub type TypeCtorItem = RcMut<TypePattern>;

#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    // Required items (no implementation)
    RequiredTypeCtor(Vec<TypeExpr>),    // Required associated type
    RequiredLambda(RcMut<TypePattern>), // Required method signature

    // Provided items (with implementation)
    TypedLambda(RcMut<TypedLambda>), // Default method implementation
    TypeCtor(RcMut<TypeCtor>),       // Associated type definition
}

// ===========================================================
// Core Structures (following parser_draft.rs design exactly)
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub scope: HashMap<Ident, ModuleItem>, // Name -> Item mapping
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedLambda {
    pub type_pattern: RcMut<TypePattern>, // Optional type annotation
    pub lambda: Lambda,                   // The actual function
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCtor {
    pub type_params_pattern: RcMut<TypePattern>, // Type parameters with constraints
    pub scope: HashMap<Ident, TypeCtorItem>,     // Constructor name -> Pattern mapping
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub type_params_pattern: RcMut<TypePattern>, // Type parameters with constraints
    pub scope: HashMap<Ident, TraitItem>,        // Item name -> TraitItem mapping
}

// ===========================================================
// Lambda and Rule System (from parser_draft.rs)
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Lambda {
    Mono(RcMut<Rule>), // Single pattern-matching rule
    Poly(Vec<Rule>),   // Multiple pattern-matching rules
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rule {
    pub pattern: RcMut<Pattern>, // Pattern to match
    pub expr: RcMut<Expr>,       // Expression to evaluate
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub pats: RcMut<Pats>, // The actual patterns (may be unparsed initially)
    pub guard: Vec<Expr>,  // Guard conditions
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pats {
    Unparsed(RcMut<SExpr>), // Unparsed S-expression (need arity info to parse)
    Parsed(Vec<Pattern>),   // Parsed pattern list
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Var(Ident),                                // Variable pattern
    Lit(LiteralExpr),                          // Literal pattern
    Ctor { ident: IdentPath, args: Vec<Pat> }, // Constructor pattern
    Wildcard,                                  // _ pattern
    Ellipsis,                                  // ... pattern
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypePattern {
    pub pats: RcMut<TypePats>, // Type patterns
    pub guard: Vec<TypeExpr>,  // Type constraints
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypePats {
    Unparsed(RcMut<SExpr>),   // Unparsed S-expression
    Parsed(Vec<TypePattern>), // Parsed type pattern list
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum UseItem {
    Ident(Ident),         // math in (use math)
    IdentPath(IdentPath), // math::statistics in (use math::statistics)
    IdentTree(IdentTree), // (collections:: Map Set) in (use (collections:: Map Set))
} // follows design: (use database), (use prelude::..), (use (collections:: Map Set))

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
    Positional(TypeExpr), // Int in (Point Int Int)

    // Named field (for records)
    Named { name: Ident, type_expr: TypeExpr }, // { name: String, age: Int }
}

// ===========================================================
// Kinds
// ===========================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    // Base kind *
    Star, // * for types like Int, Bool

    // Function kind
    Arrow { param: Box<Kind>, result: Box<Kind> }, // (* -> *) for Maybe, List

    // Constraint kind
    Constraint, // for type class constraints
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

// Parse a single module item from an S-expression
pub fn parse_module_item(ctx: &mut ParseContext, s_expr: &SExpr) -> Result<ModuleItem, String> {
    // TODO: Implement parsing for new AST structure
    // For now, return error to get code compiling
    Err("parse_module_item not implemented for new AST structure".to_string())
}

/*
// Old parsing code - needs to be rewritten for new AST structure
pub fn parse_module_item_old(ctx: &mut ParseContext, s_expr: &SExpr) -> Result<ModuleItem, String> {
    match s_expr {
        SExpr::List(elements) => {
            if elements.is_empty() {
                return Err("Empty module item list".to_string());
            }

            let first = &*elements[0].get();
            match first {
                // Type annotation: (: name type_expr)
                SExpr::TypeAnnot => {
                    if elements.len() != 3 {
                        return Err(format!("Type annotation requires exactly 3 elements, got {}", elements.len()));
                    }

                    // Parse the function name
                    let name = match &*elements[1].get() {
                        SExpr::VarIdent(var_ident) => Ident::VarIdent(var_ident.0.clone()),
                        _ => return Err("Type annotation name must be a variable identifier".to_string()),
                    };

                    // Parse the type expression
                    let type_expr = parse_type_expr(ctx, &elements[2].get())?;

                    let type_annot = TypeAnnot { name, type_expr };
                    Ok(ModuleItem::TypeAnnot(type_annot))
                }

                // Function/data binding: (= name patterns body) or (= name body)
                SExpr::Binding => {
                    if elements.len() < 3 || elements.len() > 4 {
                        return Err(format!("Binding requires 3 or 4 elements, got {}", elements.len()));
                    }

                    // Parse binding name
                    let name = match &*elements[1].get() {
                        SExpr::VarIdent(var_ident) => Ident::VarIdent(var_ident.0.clone()),
                        _ => return Err("Binding name must be a variable identifier".to_string()),
                    };

                    if elements.len() == 3 {
                        // Simple data binding: (= name value)
                        let body = parse_expr(ctx, &elements[2].get())?;

                        let binding = Binding {
                            name,
                            type_annotation: None,
                            patterns: vec![], // empty patterns = simple data binding
                            body,
                        };

                        Ok(ModuleItem::Binding(binding))
                    } else {
                        // Function binding: (= name patterns body)
                        let mut patterns = Vec::new();
                        match &*elements[2].get() {
                            SExpr::List(pattern_elements) => {
                                for pattern_element in pattern_elements.iter() {
                                    patterns.push(parse_pattern(ctx, &pattern_element.get())?);
                                }
                            }
                            _ => return Err("Expected list of patterns for function parameters".to_string()),
                        }

                        // Parse body from the fourth element
                        let body = parse_expr(ctx, &elements[3].get())?;

                        let binding = Binding {
                            name,
                            type_annotation: None, // Type annotation would be set during semantic analysis
                            patterns,
                            body,
                        };

                        Ok(ModuleItem::Binding(binding))
                    }
                }

                // TODO: Add other module item types (data definitions, traits, etc.)
                _ => {
                    Err(format!("Unsupported module item type starting with {:?}", first))
                }
            }
        }
        _ => {
            Err(format!("Expected list for module item, got {:?}", s_expr))
        }
    }
}

// Parse a do statement from an S-expression
pub fn parse_do_stmt(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<DoStmt, String> {
    match s_expr {
        // List forms: either binding or expression
        SExpr::List(elems) => {
            if elems.len() == 3 {
                // Check if it's a binding in SPO form (:= pattern expr)
                if let SExpr::DoBinding = &*elems[0].get() {
                    let pat = parse_pattern(_ctx, &elems[1].get())?;
                    let expr = parse_expr(_ctx, &elems[2].get())?;
                    return Ok(DoStmt::Bind { pat, expr });
                }
            }
            // Regular expression statement
            let expr = parse_expr(_ctx, s_expr)?;
            Ok(DoStmt::Expr(expr))
        }
        _ => {
            // Single expression statement
            let expr = parse_expr(_ctx, s_expr)?;
            Ok(DoStmt::Expr(expr))
        }
    }
}

// Parse an expression from an S-expression
pub fn parse_expr(_ctx: &mut ParseContext, s_expr: &SExpr) -> Result<Expr, String> {
    match s_expr {
        // Literal values
        SExpr::I64(value) => Ok(Expr::Literal(LiteralExpr::I64(*value))),
        SExpr::F64(value) => Ok(Expr::Literal(LiteralExpr::F64(*value))),
        SExpr::Bool(value) => Ok(Expr::Literal(LiteralExpr::Bool(*value))),
        SExpr::Char(value) => Ok(Expr::Literal(LiteralExpr::Char(*value))),
        SExpr::String(src_ref) => Ok(Expr::Literal(LiteralExpr::String(
            src_ref.resolve().to_string(),
        ))),
        SExpr::Unit => Ok(Expr::Literal(LiteralExpr::Unit)),

        // Variable identifiers
        SExpr::VarIdent(var_ident) => {
            let ident_path = IdentPath(vec![Ident::VarIdent(var_ident.0.clone())]);
            Ok(Expr::Var(ident_path))
        }
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
        }

        // Arithmetic and other operators
        SExpr::Add
        | SExpr::Sub
        | SExpr::Asterisk
        | SExpr::Div
        | SExpr::Modulus
        | SExpr::Eq
        | SExpr::Ne
        | SExpr::Lt
        | SExpr::Le
        | SExpr::Gt
        | SExpr::Ge
        | SExpr::And
        | SExpr::Or
        | SExpr::Not => {
            // Convert operators to their proper Ident variants
            let ident = match s_expr {
                SExpr::Add => Ident::Add,
                SExpr::Sub => Ident::Sub,
                SExpr::Asterisk => Ident::Mul,
                SExpr::Div => Ident::Div,
                SExpr::Modulus => Ident::Modulus,
                SExpr::Eq => Ident::Eq,
                SExpr::Ne => Ident::Ne,
                SExpr::Lt => Ident::Lt,
                SExpr::Le => Ident::Le,
                SExpr::Gt => Ident::Gt,
                SExpr::Ge => Ident::Ge,
                SExpr::And => Ident::And,
                SExpr::Or => Ident::Or,
                SExpr::Not => Ident::Not,
                _ => unreachable!(),
            };
            let ident_path = IdentPath(vec![ident]);
            Ok(Expr::Var(ident_path))
        }

        // List expressions - function applications, special forms, etc.
        SExpr::List(elems) => {
            if elems.is_empty() {
                return Err("Empty list expression not allowed".to_string());
            }

            let first = &*elems[0].get();
            match first {
                // Lambda expressions
                SExpr::Lambda => {
                    if elems.len() < 3 {
                        return Err(
                            "Lambda expression requires at least parameters and body".to_string()
                        );
                    }

                    // Parse parameters
                    let params = match &*elems[1].get() {
                        // Multiple parameters: (lambda (x y) body)
                        SExpr::List(param_list) => {
                            let mut params = Vec::new();
                            for param in param_list.iter() {
                                params.push(parse_pattern(_ctx, &param.get())?);
                            }
                            params
                        }
                        // Single parameter: (lambda x body)
                        _ => {
                            vec![parse_pattern(_ctx, &elems[1].get())?]
                        }
                    };

                    // Parse body (could be multiple expressions, treat as sequence)
                    let body = if elems.len() == 3 {
                        parse_expr(_ctx, &elems[2].get())?
                    } else {
                        // Multiple expressions - for now, just take the last one
                        // TODO: Handle proper sequential expressions
                        parse_expr(_ctx, &elems[elems.len() - 1].get())?
                    };

                    Ok(Expr::Lambda {
                        params,
                        body: Box::new(body),
                    })
                }

                // Match expressions
                SExpr::Match => {
                    if elems.len() < 3 {
                        return Err(
                            "Match expression requires expression and at least one arm".to_string()
                        );
                    }

                    let expr = parse_expr(_ctx, &elems[1].get())?;
                    let mut arms = Vec::new();

                    // Parse match arms (each arm is a list with pattern and body)
                    for i in 2..elems.len() {
                        match &*elems[i].get() {
                            SExpr::List(arm_elems) => {
                                if arm_elems.len() < 2 {
                                    return Err("Match arm requires pattern and body".to_string());
                                }
                                let pattern = parse_pattern(_ctx, &arm_elems[0].get())?;
                                let body = parse_expr(_ctx, &arm_elems[1].get())?;
                                arms.push(MatchArm {
                                    pattern,
                                    guard: None, // TODO: Handle guards
                                    body,
                                });
                            }
                            _ => return Err("Match arm must be a list".to_string()),
                        }
                    }

                    Ok(Expr::Match {
                        expr: Box::new(expr),
                        arms,
                    })
                }

                // Do expressions
                SExpr::Do => {
                    let mut stmts = Vec::new();

                    for i in 1..elems.len() {
                        let stmt = parse_do_stmt(_ctx, &elems[i].get())?;
                        stmts.push(stmt);
                    }

                    Ok(Expr::Do { stmts })
                }

                // Tuple construction with hash
                SExpr::Hash(_) => {
                    let mut tuple_elems = Vec::new();
                    // Skip the hash element, parse the rest as tuple elems
                    for i in 1..elems.len() {
                        tuple_elems.push(parse_expr(_ctx, &elems[i].get())?);
                    }
                    Ok(Expr::Literal(LiteralExpr::Tuple(tuple_elems)))
                }

                // Function application or operator application
                _ => {
                    let func = parse_expr(_ctx, &elems[0].get())?;
                    let mut args = Vec::new();

                    for i in 1..elems.len() {
                        args.push(parse_expr(_ctx, &elems[i].get())?);
                    }

                    Ok(Expr::App {
                        func: Box::new(func),
                        args,
                    })
                }
            }
        }

        // Quoted lists (list literals)
        SExpr::QuoteList(elems) => {
            let mut list_elems = Vec::new();
            for element in elems.iter() {
                list_elems.push(parse_expr(_ctx, &element.get())?);
            }
            Ok(Expr::Literal(LiteralExpr::List(list_elems)))
        }

        SExpr::DoBinding => Err("DoBinding (:=) should only appear inside do blocks".to_string()),

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
        SExpr::String(source_ref) => Ok(Pattern::Literal(LiteralExpr::String(
            source_ref.resolve().to_string(),
        ))),
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
        }

        // List patterns - constructor patterns, tuple patterns, list patterns
        SExpr::List(elems) => {
            if elems.is_empty() {
                return Err("Empty list pattern not allowed".to_string());
            }

            let first = &*elems[0].get();
            match first {
                // Tuple patterns with hash
                SExpr::Hash(_) => {
                    let mut tuple_elems = Vec::new();
                    // Skip the hash element, parse the rest as tuple elems
                    for i in 1..elems.len() {
                        tuple_elems.push(parse_pattern(_ctx, &elems[i].get())?);
                    }
                    Ok(Pattern::Tuple(tuple_elems))
                }

                // Constructor patterns
                SExpr::CtorIdent(ctor_ident) => {
                    let ident_path = IdentPath(vec![Ident::CtorIdent(ctor_ident.0.clone())]);
                    let mut args = Vec::new();

                    // Parse constructor arguments
                    for i in 1..elems.len() {
                        args.push(parse_pattern(_ctx, &elems[i].get())?);
                    }

                    Ok(Pattern::Constructor {
                        ctor: ident_path,
                        args,
                    })
                }

                _ => {
                    return Err(format!(
                        "Unsupported list pattern starting with {:?}",
                        first
                    ));
                }
            }
        }

        // Quoted list patterns
        SExpr::QuoteList(elems) => {
            let mut list_elems = Vec::new();
            let mut rest = None;

            for element in elems.iter() {
                match &*element.get() {
                    SExpr::ListIdent(list_ident) => {
                        // This is a rest pattern (like xs..)
                        rest = Some(Box::new(Pattern::ListVar(Ident::VarIdent(
                            list_ident.0.clone(),
                        ))));
                        break;
                    }
                    _ => {
                        list_elems.push(parse_pattern(_ctx, &element.get())?);
                    }
                }
            }

            Ok(Pattern::List {
                elems: list_elems,
                rest,
            })
        }

        _ => {
            unimplemented!("parse_pattern for {s_expr:?} not implemented yet")
        }
    }
}
*/

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
            scope: HashMap::new(),
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

    /*
    // Old tests that need to be updated for new AST structure

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
        assert_eq!(
            expr,
            Expr::Literal(LiteralExpr::String("hello".to_string()))
        );
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
            Expr::Literal(LiteralExpr::List(elems)) => {
                assert_eq!(elems.len(), 3);
                assert_eq!(elems[0], Expr::Literal(LiteralExpr::I64(1)));
                assert_eq!(elems[1], Expr::Literal(LiteralExpr::I64(2)));
                assert_eq!(elems[2], Expr::Literal(LiteralExpr::I64(3)));
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
            Expr::Literal(LiteralExpr::Tuple(elems)) => {
                assert_eq!(elems.len(), 3);
                assert_eq!(elems[0], Expr::Literal(LiteralExpr::I64(1)));
                assert_eq!(
                    elems[1],
                    Expr::Literal(LiteralExpr::String("two".to_string()))
                );
                assert_eq!(elems[2], Expr::Literal(LiteralExpr::F64(3.0)));
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
            Pattern::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                match &elems[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "a");
                    }
                    _ => panic!("Expected Var pattern for first element"),
                }
                match &elems[1] {
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
            Pattern::List { elems, rest } => {
                // Should have one element 'x'
                assert_eq!(elems.len(), 1);
                match &elems[0] {
                    Pattern::Var(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected Var pattern for list element"),
                }

                // Should have rest pattern 'xs..' as ListVar
                assert!(rest.is_some());
                match rest.as_ref().unwrap().as_ref() {
                    Pattern::ListVar(Ident::VarIdent(source_ref)) => {
                        assert_eq!(source_ref.resolve(), "xs..");
                    }
                    other => {
                        panic!("Expected ListVar pattern for rest, got: {:?}", other);
                    }
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
            TypeExpr::Ctor { ident, args } => {
                match ident {
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
            _ => panic!("Expected Ctor type"),
        }
    }

    #[test]
    fn test_parse_function_type() {
        let s_exprs = lex_source(r#"(Int -> Bool)"#);
        let mut ctx = ParseContext::new();

        let type_expr = parse_type_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match type_expr {
            TypeExpr::Ctor { ident, args } => {
                // Should be Arrow constructor
                match ident {
                    IdentPath(ref idents) => {
                        assert_eq!(idents.len(), 1);
                        assert_eq!(idents[0], Ident::Arrow);
                    }
                }

                // Should have two arguments: Int and Bool
                assert_eq!(args.len(), 2);

                // First argument should be Int
                match &args[0] {
                    TypeExpr::Ctor { ident: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Int");
                            }
                            _ => panic!("Expected CtorIdent for first argument"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Ctor for first argument"),
                }

                // Second argument should be Bool
                match &args[1] {
                    TypeExpr::Ctor { ident: IdentPath(ref idents), args } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Bool");
                            }
                            _ => panic!("Expected CtorIdent for second argument"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Ctor for second argument"),
                }
            }
            _ => panic!("Expected Ctor type for arrow"),
        }
    }

    #[test]
    fn test_parse_parameterized_type() {
        let s_exprs = lex_source(r#"(Maybe a)"#);
        let mut ctx = ParseContext::new();

        let type_expr = parse_type_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match type_expr {
            TypeExpr::Ctor { ident, args } => {
                // Constructor should be 'Maybe'
                match ident {
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
                    TypeExpr::Var(IdentPath(ref idents)) => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::VarIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "a");
                            }
                            _ => panic!("Expected VarIdent for type argument"),
                        }
                    }
                    _ => panic!("Expected Var type for argument"),
                }
            }
            _ => panic!("Expected Ctor type"),
        }
    }

    // ===========================================================
    // Module Item Tests - Updated for new structure
    // ===========================================================

    #[test]
    fn test_parse_type_annotation_new() {
        let s_exprs = lex_source(r#"(add : (Int -> Int))"#);
        let mut ctx = ParseContext::new();

        let module_item = parse_module_item(&mut ctx, &s_exprs[0].get()).unwrap();
        match module_item {
            ModuleItem::TypeAnnot(type_annot) => {
                match type_annot.name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "add");
                    }
                    _ => panic!("Expected VarIdent for function name"),
                }
            }
            _ => panic!("Expected TypeAnnot module item"),
        }
    }

    #[test]
    fn test_parse_simple_data_binding() {
        let s_exprs = lex_source(r#"(x = 42)"#);
        let mut ctx = ParseContext::new();

        let module_item = parse_module_item(&mut ctx, &s_exprs[0].get()).unwrap();
        match module_item {
            ModuleItem::Binding(binding) => {
                match binding.name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "x");
                    }
                    _ => panic!("Expected VarIdent for binding name"),
                }
                assert_eq!(binding.patterns.len(), 0); // Simple data binding has no patterns
                match binding.body {
                    Expr::Literal(LiteralExpr::I64(42)) => {} // Correct
                    _ => panic!("Expected I64(42) literal"),
                }
            }
            _ => panic!("Expected Binding module item"),
        }
    }

    #[test]
    fn test_parse_function_binding() {
        let s_exprs = lex_source(r#"(add = (x y) (x + y))"#);
        let mut ctx = ParseContext::new();

        let module_item = parse_module_item(&mut ctx, &s_exprs[0].get()).unwrap();
        match module_item {
            ModuleItem::Binding(binding) => {
                match binding.name {
                    Ident::VarIdent(source_ref) => {
                        assert_eq!(source_ref.resolve(), "add");
                    }
                    _ => panic!("Expected VarIdent for function name"),
                }
                assert_eq!(binding.patterns.len(), 2); // Two parameters: x and y
                // Verify body is an application expression
                match binding.body {
                    Expr::App { func: _, args } => {
                        assert_eq!(args.len(), 2);
                    }
                    _ => panic!("Expected App expression for body"),
                }
            }
            _ => panic!("Expected Binding module item"),
        }
    }

    // Old test that needs to be updated for new AST structure
    // #[test]
    // fn test_parse_type_annotation() {
        let s_exprs = lex_source(
            r#"
            (add : (Int -> (Int -> Int)))
        "#,
        );
        let mut ctx = ParseContext::new();

        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::TypeAnnot { name, type_expr: _ } => match name {
                Ident::VarIdent(source_ref) => {
                    assert_eq!(source_ref.resolve(), "add");
                }
                _ => panic!("Expected VarIdent for function name"),
            },
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
        let s_exprs = lex_source(
            r#"
            (data (Maybe a)
                Nothing
                (Just a)
            )
        "#,
        );
        let mut ctx = ParseContext::new();

        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::DataDef {
                name,
                type_params,
                constructors,
            } => {
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
        let s_exprs = lex_source(
            r#"
            (trait (Show a)
                (show : (a -> String)))
        "#,
        );
        let mut ctx = ParseContext::new();

        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::TraitDef {
                name,
                type_params,
                items,
            } => {
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
                    TraitItem::TypeAnnotation { name, type_expr: _ } => match name {
                        Ident::VarIdent(source_ref) => {
                            assert_eq!(source_ref.resolve(), "show");
                        }
                        _ => panic!("Expected VarIdent for method name"),
                    },
                    _ => panic!("Expected TypeAnnotation trait item"),
                }
            }
            _ => panic!("Expected TraitDef declaration"),
        }
    }

    #[test]
    fn test_parse_impl_definition() {
        let s_exprs = lex_source(
            r#"
            (impl Show Bool
                (show =
                    True  "True"
                    False "False")
                )
        "#,
        );
        let mut ctx = ParseContext::new();

        let decl = parse_decl(&mut ctx, &s_exprs[0].get()).unwrap();
        match decl {
            Decl::ImplDef {
                trait_name,
                type_expr,
                items,
            } => {
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
                    TypeExpr::Ctor {
                        ident: IdentPath(ref idents),
                        args,
                    } => {
                        assert_eq!(idents.len(), 1);
                        match &idents[0] {
                            Ident::CtorIdent(source_ref) => {
                                assert_eq!(source_ref.resolve(), "Bool");
                            }
                            _ => panic!("Expected CtorIdent for Bool"),
                        }
                        assert_eq!(args.len(), 0);
                    }
                    _ => panic!("Expected Ctor type for Bool"),
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
        let s_exprs = lex_source(
            r#"
            (mod my_module
                (data MyType
                    (MyCons Int)
                )
            )
        "#,
        );
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
                    Decl::DataDef { name, .. } => match name {
                        Ident::CtorIdent(source_ref) => {
                            assert_eq!(source_ref.resolve(), "MyType");
                        }
                        _ => panic!("Expected CtorIdent for data type name"),
                    },
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
            Decl::TypeAnnot { name, type_expr: _ } => match name {
                Ident::VarIdent(source_ref) => {
                    assert_eq!(source_ref.resolve(), "id");
                }
                _ => panic!("Expected VarIdent for function name"),
            },
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
        let s_exprs = lex_source(
            r#"
            (match x
                (Nothing 0)
                ((Just y) y)
            )
        "#,
        );
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
                    Pattern::Constructor {
                        ctor: IdentPath(ref idents),
                        args,
                    } => {
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
                    Pattern::Constructor {
                        ctor: IdentPath(ref idents),
                        args,
                    } => {
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
        let s_exprs = lex_source(
            r#"
            (do
                (x := get_value)
                (pure x))
        "#,
        );
        let mut ctx = ParseContext::new();

        let expr = parse_expr(&mut ctx, &s_exprs[0].get()).unwrap();
        match expr {
            Expr::Do { stmts } => {
                assert_eq!(stmts.len(), 2);

                // First statement should be bind
                match &stmts[0] {
                    DoStmt::Bind {
                        pat: pattern,
                        expr: _,
                    } => match pattern {
                        Pattern::Var(Ident::VarIdent(source_ref)) => {
                            assert_eq!(source_ref.resolve(), "x");
                        }
                        _ => panic!("Expected Var pattern for bind"),
                    },
                    _ => panic!("Expected Bind statement"),
                }

                // Second statement should be expression
                match &stmts[1] {
                    DoStmt::Expr(expr) => match expr {
                        Expr::App { func: _, args } => {
                            assert_eq!(args.len(), 1);
                        }
                        _ => panic!("Expected App expression in do"),
                    },
                    _ => panic!("Expected Expr statement"),
                }
            }
            _ => panic!("Expected Do expression"),
        }
    }
    */
}
