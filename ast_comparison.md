# AST Comparison: Current parser.rs vs parser_draft.rs

## Summary
This comparison identifies differences between the current AST implementation and the reference parser_draft.rs to determine which differences should be fixed and which are improvements to keep.

## Key Structural Differences

### 1. **ModuleItem Enum Structure**

**parser_draft.rs:**
```rust
#[derive(Debug, PartialEq, Clone)]
enum ModuleItem {
    TypedLambda(RcMut<TypedLambda>),
    TypeCtor(RcMut<TypeCtor>),
    Trait(RcMut<Trait>),
    Module(RcMut<Module>),
}
```

**Current parser.rs:**
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
    Binding(Binding),     
    TypeAnnot(TypeAnnot), 
    TypeCtor(TypeCtor),   
    Trait(Trait),         
    Module(Module),       
    Use(UseItem),         
}
```

**Analysis:**
- ❌ **SHOULD FIX**: Missing `TypedLambda` - this is important for the design
- ✅ **KEEP**: `Binding` and `TypeAnnot` separation is better than having them combined in `TypedLambda`
- ✅ **KEEP**: `Use(UseItem)` is an improvement for import system
- ❌ **SHOULD FIX**: Types should use `RcMut<T>` for consistency with parser_draft

### 2. **TraitItem Structure**

**parser_draft.rs:**
```rust
#[derive(Debug, PartialEq, Clone)]
enum TraitItem {
    RequiredTypeCtor(Vec<TypeExpr>),
    RequiredLambda(RcMut<TypePattern>),
    TypedLambda(RcMut<TypedLambda>),
    TypeCtor(RcMut<TypeCtor>),
}
```

**Current parser.rs:**
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    TypeAnnot(TypeAnnot), 
    Binding(Binding),     
    TypeCtor(TypeCtor),   
}
```

**Analysis:**
- ❌ **SHOULD FIX**: Missing `RequiredTypeCtor` and `RequiredLambda` for trait requirements
- ❌ **SHOULD FIX**: Missing `TypedLambda` for trait implementations
- ✅ **KEEP**: `TypeAnnot` and `Binding` are clearer than `RequiredLambda` and `TypedLambda`

### 3. **Missing Core Types**

**parser_draft.rs has but current doesn't:**
- `TypedLambda` struct
- `Lambda` enum (Mono/Poly)
- `Rule` struct  
- `Pattern` with `Pats` enum (Unparsed/Parsed)
- `TypePattern` with `TypePats` enum
- `Pat` enum
- `TypePat` enum

**Analysis:**
- ❌ **SHOULD ADD**: `TypedLambda` - core concept
- ❌ **SHOULD ADD**: `Lambda` enum for function definitions  
- ❌ **SHOULD ADD**: `Rule` struct for pattern matching rules
- ⚠️ **CONSIDER**: Current `Pattern` is simpler but may need the more complex structure later
- ⚠️ **CONSIDER**: Unparsed/Parsed pattern approach may be needed for complex parsing

### 4. **Data Structures and Memory Management**

**parser_draft.rs:**
- Uses `RcMut<T>` extensively for shared mutable references
- Uses `BTreeMap<Ident, ModuleItem>` for scopes

**Current parser.rs:**
- Uses direct ownership with `Box<T>` and `Vec<T>`
- Uses `Vec<ModuleItem>` for module items

**Analysis:**
- ❌ **SHOULD FIX**: `RcMut<T>` is likely needed for the parser's needs
- ⚠️ **CONSIDER**: `BTreeMap` vs `Vec` for module items - BTreeMap enables name lookup

### 5. **Ident and Path Structures**

**parser_draft.rs:**
```rust
struct IdPath(RcMut<Vec<Ident>>);
struct IdTree {
    head: IdPath,
    tail: RcMut<Vec<IdTree>>,
}
```

**Current parser.rs:**
```rust
pub struct IdentPath(pub Vec<Ident>);
pub struct IdentTree {
    pub head: IdentPath,
    pub tail: Vec<IdentTree>,
}
```

**Analysis:**
- ❌ **SHOULD FIX**: Use `RcMut` for consistency
- ✅ **KEEP**: Public fields are reasonable for this context

### 6. **Expr Structure Differences**

**Current has additional features:**
- `Let` expressions
- `Match` expressions  
- `Do` notation
- More literal types (Char, List, Tuple)

**Analysis:**
- ✅ **KEEP**: These are legitimate language features not in parser_draft
- ✅ **IMPROVEMENT**: More complete expression system

### 7. **Type System**

**Current has additional features:**
- `Kind` enum for kind system
- More complete `ConstructorField` system
- Better `UseItem` structure

**Analysis:**
- ✅ **KEEP**: These are improvements over parser_draft

## Recommendations

### High Priority Fixes (Align with parser_draft):
1. Add `TypedLambda` struct and integrate into `ModuleItem`
2. Add `Lambda` enum (Mono/Poly) 
3. Add `Rule` struct for pattern matching
4. Switch to using `RcMut<T>` for shared references
5. Use `BTreeMap` for module scopes instead of `Vec`

### Medium Priority Considerations:
1. Evaluate if `Pats` (Unparsed/Parsed) pattern system is needed
2. Add `RequiredTypeCtor` and `RequiredLambda` to `TraitItem`
3. Consider `TypePattern` with constraints system

### Keep Current Improvements:
1. `Binding` and `TypeAnnot` separation
2. `UseItem` import system
3. Extended `Expr` with Let/Match/Do
4. `Kind` system
5. Better literal types
6. More complete constructor system

## Action Plan

1. **Phase 1**: Add missing core types (`TypedLambda`, `Lambda`, `Rule`)
2. **Phase 2**: Switch to `RcMut<T>` memory management
3. **Phase 3**: Convert module storage to `BTreeMap`
4. **Phase 4**: Enhance `TraitItem` with required items
5. **Phase 5**: Evaluate need for `Pats` system
