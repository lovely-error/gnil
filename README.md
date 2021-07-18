# gnil

Very alpha parser in haskell that wont be working 99% of thr time. Doing it to impress girls |:={

___
**topDecl** := _modDef_ `+`

**modDef** := `module` _id_ `{` _stmt_ (`\n` | `;`) `}`

**id** := `regular name` | `wrapped name` | `special function name`

**stmt** := _funDel_ | _recDecl_ | _enumDecl_ | _constDecl_

**funDecl** := `fn` id (`:` _typeSig_)? `=` _expr_

```
fn 'identity of #' = { val in return val }`
id := 'identity of ('identity of _')'
```

**recDecl** := `rec` id `{` _members_ `}`
```
rec Person { name: String, age: Int } 
```
**enumDecl** := `enum` id `{` _members_ `}`
```
enum Exception { deffered, canceled, custom: Error }
```