// use std::{
//     collections::BTreeMap,
//     path::{Path, PathBuf},
//     rc::Rc,
// };

// use crate::{
//     base::RcMut,
//     lexer::{self, lex, SExpr},
//     source_ref::SourceRef,
// };

// #[derive(Debug, PartialEq, Clone)]
// enum Ident {
//     // BuiltIn,
//     Arrow, // e.g. "->"

//     Quote,    // e.g. "`"
//     Nil,      // e.g. "Nil"
//     Hash(u8), // e.g. "#2", "#3", ... "#12"

//     Asterisk, // e.g. "*"

//     Eq, // e.g. "=="
//     Ne, // e.g. "!="
//     Gt, // e.g. ">"
//     Ge, // e.g. ">="
//     Lt, // e.g. "<"
//     Le, // e.g. "<="

//     And, // e.g. "&"
//     Or,  // e.g. "|"
//     Not, // e.g. "!"

//     Add, // e.g. "+"
//     Sub, // e.g. "-"
//     // Asterisk is used for multiplication
//     Div,     // e.g. "/"
//     Modulus, // e.g. "%"

//     Dot, // e.g. "."

//     Unit, // e.g. "()", Refers both to the unit type and the unit value

//     BoolType, // e.g. "Bool"
//     I64Type,  // e.g. "I64"
//     F64Type,  // e.g. "F64"
//     StrType,  // e.g. "Str"

//     CtorIdent(SourceRef),     // e.g. "SomeType", "AnotherType"
//     VarIdent(SourceRef),      // e.g. "x", "y", "z"
//     OpIdent(SourceRef),       // e.g. ">>=", "++",
//     FieldAccessor(SourceRef), // e.g. ".some_field", ".another_field" (VarIdent Does not include dot)
// }

// #[derive(Debug, PartialEq, Clone)]
// struct IdPath(RcMut<Vec<Ident>>);

// #[derive(Debug, PartialEq, Clone)]
// struct IdTree {
//     head: IdPath,
//     tail: RcMut<Vec<IdTree>>,
// }

// /* -------------------------------------------------------------------------- */
// /*                                  level 0¸                                  */
// /* -------------------------------------------------------------------------- */

// pub struct Root(RcMut<Module>);

// /* -------------------------------------------------------------------------- */
// /*                                   Level 1                                  */
// /* -------------------------------------------------------------------------- */

// #[derive(Debug, PartialEq, Clone)]
// enum ModuleItem {
//     TypedLambda(RcMut<TypedLambda>),
//     TypeCtor(RcMut<TypeCtor>),
//     Trait(RcMut<Trait>),
//     Module(RcMut<Module>),
// }

// type TypeCtorItem = RcMut<TypePattern>;

// #[derive(Debug, PartialEq, Clone)]
// enum TraitItem {
//     // There could be required type and functions
//     // RequiredTypeCtor have name and type constraints only, and name is saved in the scope of the trait
//     // RequiredLambda have name and type annotation only, type annotation is a single type pattern and name is saved in the scope of the trait
//     RequiredTypeCtor(Vec<TypeExpr>),
//     RequiredLambda(RcMut<TypePattern>),

//     TypedLambda(RcMut<TypedLambda>),
//     TypeCtor(RcMut<TypeCtor>),
// }

// /* -------------------------------------------------------------------------- */
// /*                                   Level 2                                  */
// /* -------------------------------------------------------------------------- */

// #[derive(Debug, PartialEq, Clone)]
// struct Module {
//     scope: BTreeMap<Ident, ModuleItem>,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct TypedLambda {
//     type_pattern: RcMut<TypePattern>,
//     lambda: Lambda,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct TypeCtor {
//     type_params_pattern: RcMut<TypePattern>,
//     scope: BTreeMap<Ident, TypeCtorItem>,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Trait {
//     type_params_pattern: RcMut<TypePattern>,
//     scope: BTreeMap<Ident, TraitItem>,
// }

// /* -------------------------------------------------------------------------- */
// /*                                  Level 3¸                                  */
// /* -------------------------------------------------------------------------- */

// #[derive(Debug, PartialEq, Clone)]
// enum Lambda {
//     Mono(RcMut<Rule>),
//     Poly(Vec<Rule>),
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Rule {
//     pattern: RcMut<Pattern>,
//     expr: RcMut<Expr>,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Pattern {
//     // Generally, pattern in declaring position could be parsed at the first place.
//     // However, one in the refering context (e.g. rule) could be parsed only after all the Ctor's airitys are known.
//     pats: RcMut<Pats>,
//     guard: Vec<Expr>,
// }

// #[derive(Debug, PartialEq, Clone)]
// enum Pats {
//     // It is certain that Pats are a single SExpr,
//     // However, airity of pattern should be known in order to split them.
//     // Which means, one could not make Unparsed Pat hold it's SExpr but Pats should
//     Unparsed(RcMut<SExpr>),
//     Parsed(Vec<Pattern>),
// }

// #[derive(Debug, PartialEq, Clone)]
// enum Pat {
//     Var(Ident),
//     Lit(LiteralExpr),
//     Ctor { ident: IdPath, args: Vec<Pat> },
//     Wildcard,
//     Ellipsis,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct TypePattern {
//     pats: RcMut<TypePats>,
//     guard: Vec<TypeExpr>,
// }

// #[derive(Debug, PartialEq, Clone)]
// enum TypePats {
//     // It is certain that TypePats are a single SExpr,
//     // However, airity of pattern should be known in order to split them.
//     // Which means, one could not make Unparsed Pat hold it's SExpr but Pats should
//     Unparsed(RcMut<SExpr>),
//     Parsed(Vec<TypePattern>),
// }

// #[derive(Debug, PartialEq, Clone)]
// enum TypePat {
//     Var(Ident),
//     Ctor { ident: IdPath, args: Vec<TypePat> },
//     Wildcard,
// }

// /* -------------------------------------------------------------------------- */
// /*                                   Level 4                                  */
// /* -------------------------------------------------------------------------- */

// #[derive(Debug, PartialEq, Clone)]
// enum Expr {
//     Literal(LiteralExpr),
//     Var(IdPath),

//     Lambda(RcMut<Lambda>),
//     TypedLambda(RcMut<TypedLambda>),

//     App { f: RcMut<Expr>, args: Vec<Expr> },
//     // Block could be expressed as a application of a lambda taking Unit
//     // DoBlock could be desugard to regular monadic expression

//     // Type annotation could be expressed as a application of a typed lambda
//     // Match could be expressed as a application of a lambda
// }

// #[derive(Debug, PartialEq, Clone)]
// enum TypeExpr {
//     Var(IdPath),
//     Ctor {
//         ident: IdPath,
//         args: Vec<TypeExpr>,
//     },
//     Forall {
//         type_params: Vec<Ident>,
//         expr: RcMut<TypeExpr>,
//     },
// }

// /* -------------------------------------------------------------------------- */
// /*                                   Level 5                                  */
// /* -------------------------------------------------------------------------- */

// #[derive(Debug, PartialEq, Clone)]
// enum LiteralExpr {
//     Unit,
//     Bool(bool),
//     I64(i64),
//     F64(f64),
//     Str(String),
// }

// /* -------------------------------------------------------------------------- */
// /*                                   Context                                  */
// /* -------------------------------------------------------------------------- */

// struct Context {
//     config: Config,

//     accepted_srcs: BTreeMap<Rc<String>, Rc<String>>,
//     accepted_warnings: Vec<String>,

//     module_stack: Vec<RcMut<Module>>,

//     staged_src_name: Rc<String>,
//     staged_src: Rc<String>,

//     staged_warning: Vec<String>,
//     staged_s_exprs: Rc<[RcMut<SExpr>]>,
//     staged_module: RcMut<Module>,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Config {
//     root_path: PathBuf,
// }

// /* -------------------------------------------------------------------------- */
// /*                               Implementations                              */
// /* -------------------------------------------------------------------------- */

// impl Context {
//     /// Import and stage a source file
//     fn import_src(&mut self, path: &PathBuf) -> Result<(), String> {
//         let src_name = path.to_string_lossy().to_string();

//         self.staged_src_name = Rc::new(src_name.clone());
//         self.staged_src = self.config.import_src(path)?;

//         todo!();
//     }

//     fn initial_pass(&mut self) -> Result<(), String> {
//         let s_exprs = lex(self.staged_src.clone())?;
//         self.staged_s_exprs = s_exprs;

//         // Collect all the item and parse them as far as possible.
//         // Refering pats will be parsed after this pass.

//         for s_expr in self.staged_s_exprs {
//             self.collect_item(s_expr.clone())?;
//         }

//         Ok(())
//     }
// }

// impl Config {
//     pub fn new(root_path: PathBuf) -> Self {
//         Config { root_path }
//     }

//     fn import_src(&self, path: &PathBuf) -> Result<Rc<String>, String> {
//         let src = std::fs::read_to_string(&path).map_err(|_| {
//             format!(
//                 "Failed to read file: {}",
//                 path.to_string_lossy().to_string()
//             )
//         })?;
//         Ok(Rc::new(src))
//     }
// }
