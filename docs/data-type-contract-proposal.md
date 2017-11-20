# Data Type Contract Proposal

Suppose we have a Go project, `Prj`, that depends on packages `PkgA` and
`PkgB`. Furthermore, suppose that `PkgB` depends on packages `PkgC` and `PkgD`.
Now suppose that `PkgC` changes. The following four situations can arise:
1. compiling `Prj` fails; this may be caused by:
   * the syntax error inside `PkgC`
   * the semantic error inside `PkgC`
   * the semantic error inside `PkgB`
     * the API of `PkgC` changes
     * the data type of a symbol exported from `PkgC` that is used inside
       `PkgB` changes and this change is not data type compatible (e.g. type
       mismatch)
   * the semantic error inside `Prj`
1. `Prj` compiles, but running tests fails; this may be caused by:
   * tests that test a type compatibility (e.g. type assertions of generic
     types) fails
   * tests that test a project behavior based on comparison of the expected and
     produced values fails
1. `Prj` CI succeeds, but there exist a (hidden) bug(s)
   * there is a hidden type incompatibility (e.g. assignment of a value of
     wrong type to a generic interface variable) that may cause a panic during
     runtime
   * the project makes a wrong decision on input data and produces a wrong
     output
1. `Prj` CI succeeds and there are no bugs, everything is OK

Our mission will be design and implement an analyzer that will help us to
detect the mentioned risks. Since the comprehensive analysis is a Herculean
task, we focus only on a data type propagation analysis only.

We introduce a notion of *data type contracts*. Let *X* be a Go construct and
*C* be a partial mapping that has a set of all Go constructs as its domain and
a set of all Boolean values as its range. We say that *X makes a contract* iff
*C(X)* is true. The definition of *C* will be given later in this proposal.

Data type contracts are stored in data type contract table, where they can be
kept for later use. A data type contract is created once a data type is used.
To demonstrate this, consider the following code snippet:
```go
func sum(x, y int) int {
	return x + y
}
```
Here, `sum` is a new symbol that has a type `func (x, y int) int`. Once a
declaration of `sum` is reached, a contract is created and it is stored to the
contract table. The implementation of a data type contract table must deal with
several issues derived from the nature of Go, e.g. a symbol can be used before
it is defined, same names of symbols from different scopes etc.

## General Type for Data Type Contract Storage

A general data type that describes a data type contract is an interface
```go
type Contract interface {
	Type() string
	Package() string
}
```

Some information are common for all contracts, e.g. the name of the package
where a contract has been made
```go
type BaseContract struct {
	Package string
}
```

## Data Type Contract Table

Actually, data type contract table and symbol table are same (contracts are
stored to symbol table).

## Data Type Contracts for Expressions

First, we recall the grammar for *Go* expressions up to the lexical elements
```
Expression = UnaryExpr | Expression binary_op Expression .
UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .

binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
add_op     = "+" | "-" | "|" | "^" .
mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .

unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .

PrimaryExpr =
	Operand |
	Conversion |
	PrimaryExpr Selector |
	PrimaryExpr Index |
	PrimaryExpr Slice |
	PrimaryExpr TypeAssertion |
	PrimaryExpr Arguments .

Selector       = "." identifier .
Index          = "[" Expression "]" .
Slice          = "[" [ Expression ] ":" [ Expression ] "]" |
                 "[" [ Expression ] ":" Expression ":" Expression "]" .
TypeAssertion  = "." "(" Type ")" .
Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .

ExpressionList = Expression { "," Expression } .

Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
Literal     = BasicLit | CompositeLit | FunctionLit .
BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
OperandName = identifier | QualifiedIdent.

CompositeLit  = LiteralType LiteralValue .
LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
                SliceType | MapType | TypeName .
LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
ElementList   = KeyedElement { "," KeyedElement } .
KeyedElement  = [ Key ":" ] Element .
Key           = FieldName | Expression | LiteralValue .
FieldName     = identifier .
Element       = Expression | LiteralValue .

FunctionLit  = "func" Function .
Function     = Signature FunctionBody .
FunctionBody = Block .

Conversion = Type "(" Expression [ "," ] ")" .

MethodExpr    = ReceiverType "." MethodName .
ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
```
and, for the completeness, the grammar for *Go* types:
```
Type      = TypeName | TypeLit | "(" Type ")" .
TypeName  = identifier | QualifiedIdent .
TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
	    SliceType | MapType | ChannelType .

ArrayType   = "[" ArrayLength "]" ElementType .
ArrayLength = Expression .
ElementType = Type .

StructType    = "struct" "{" { FieldDecl ";" } "}" .
FieldDecl     = (IdentifierList Type | EmbeddedField) [ Tag ] .
EmbeddedField = [ "*" ] TypeName .
Tag           = string_lit .

PointerType = "*" BaseType .
BaseType    = Type .

FunctionType   = "func" Signature .
Signature      = Parameters [ Result ] .
Result         = Parameters | Type .
Parameters     = "(" [ ParameterList [ "," ] ] ")" .
ParameterList  = ParameterDecl { "," ParameterDecl } .
ParameterDecl  = [ IdentifierList ] [ "..." ] Type .

InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
MethodSpec         = MethodName Signature | InterfaceTypeName .
MethodName         = identifier .
InterfaceTypeName  = TypeName .

SliceType = "[" "]" ElementType .

MapType     = "map" "[" KeyType "]" ElementType .
KeyType     = Type .

ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .

IdentifierList = identifier { "," identifier } .
QualifiedIdent = PackageName "." identifier .
PackageName    = identifier .
```

### Primary expressions

Let `x` be `Literal`. `x` makes a contract and
* `x` has a type of one of build-in types if `x` is `BasicLit`;
* `x` has a type of a composite literal if `x` is `CompositeLit`;
* `x` has a type of a function literal if `x` is `FunctionLit`.

The data type of a value that represents a data type contract for literal is
```go
type LiteralContract struct {
	// Type of the result
	r DataType
}
```

Let `x` be `OperandName`. `x` has either stored its contract in the contract
table or has no contract yet.

Let `x` be `MethodExpr`. We say that `x` makes a contract if
* `x` has a form `T.M` and `M` is in a method set of `T` (and `*T`); in this
  case, `x` has a type of function `M` with an additional argument of a type
  `T` added to the signature of `M` as its first argument;
* `x` has a form `(*T).M` and `M` is in a method set of `*T` (or `T`); in this
  case, `x` has a type of function `M` with an additional argument of a type
  `*T` added to the signature of `M` as its first argument.

The data type of a value that represents a data type contract for method
expression is
```go
type MethodExprContract struct {
	// Type of receiver
	receiver DataType
	// Method name
	method   string
	// Type of the result
	r        DataType
}
```
**Note**: Compiler knows whether `T.M` is valid or not, but we must find out
the type of `T.M`. That means, we must search in the symbol table for a method
`M` with the receiver type `T` (or `*T`, see Golang specification to get more
about this).

Let `x` be of a form `(y)` where `y` is `Expression`. The data type contract of
`x` is the data type contract of `y`.

Let `x` be `Conversion`. We say that `x` makes a contract if `x` is of a form
`T(y)`, where `T` is a type and `y` is `Expression` convertible to `T`; in this
case, `x` has a type `T`.

The data type of a value that represents a data type contract for conversion is
```go
type ConversionContract struct {
	// Expected type (type to convert to)
	// Also the type of result
	expected DataType
	// Operand
	x        Contract
}
```

Let `x` be `PrimaryExpr`. We say that `x.y` makes a contract if `x` (or `*x`)
has the field or method `y` or if `x` (or `*x`) has embedded field that has the
field or method `y`; in this case, `x.y` has a type that has `y`.

The data type of a value that represents a data type contract for selectors is
```go
type SelectorContract struct {
	// Operand
	x     Contract
	// Field name
	field string
	// The type of result
	r     DataType
}
```

Let `x` be `PrimaryExpr`. We say that `x[y]`, where `[y]` denotes the slice or
index operation, makes a contract if
* `x` is indexable and `[y]` is an index operation; in this case, `x[y]`'s type
  is `x`'s underlying type (base element's type for arrays, slices, pointers,
  `byte` for strings, and value type for maps);
* `x` is array, slice, pointer to array, or string and `[y]` is a slicing
  operation; in this case, `x[y]` is a string if `x` is a string, otherwise
  `x[y]` is a slice.

The data types of a values that represent a data type contracts for indexing
and slicing are
```go
type IndexContract struct {
	// Operand
	x Contract
	// Index
	y Contract
	// The type of result
	r DataType
}

type SliceContract struct {
	// Operand
	x          Contract
	// Boundaries
	y1, y2, y3 Contract
	// The type of result
	r          DataType
}
```

Let `x` be `PrimaryExpr`. We say that `x.(T)` makes a contract if `T` is a
type; in this case, `x.(T)` has type `T`.

The data type of a value that represents a data type contract for type
assertion is
```go
type TypeAssertionContract struct {
	// Operand
	x Contract
	// Expected data type (same as result type)
	t DataType
}
```

Let `x` be `PrimaryExpr`. We say that `x(a, b, c, ...)` makes a contract if
`x` has a function type `F` and `a, b, c, ...` match with `F`'s arguments; in
this case, `x(a, b, c, ...)`'s type is a `F`'s result type.

The data type of a value that represents a data type contract for call is
```go
type CallContract struct {
	// Operand
	x    Contract
	// Arguments
	args []Contract
	// The type of result
	r    DataType
}
```

### Unary expressions

Let `x` be `PrimaryExpr` and let `O` be one of `+ -`. We say that `O x` makes a
contract if `x` has a numeric type or is an untyped numeric constant; in this
case, `O x` has the same type as `x`.

Let `x` be `PrimaryExpr`. We say that `!x` makes a contract if `x` has a
Boolean type or is an untyped Boolean constant; in this case, `!x` has the same
type as `x`.

Let `x` be `PrimaryExpr`. We say that `^x` makes a contract if `x` has an
integer type or is an untyped integer constant; in this case, `^x` has the same
type as `x`.

Let `x` be `PrimaryExpr`. We say that `&x` makes a contract if `x` is
addressable; if `x` has a type `T`, `&x` has a type `*T`.

Let `x` be `PrimaryExpr`. We say that `*x` makes a contract if `x` has a type
`*T`; in this case, `*x` has a type `T`.

Let `x` be `PrimaryExpr`. We say that `<-x` makes a contract if `x` is of the
type of channel; in this case, `<-x` has an underlying type of `x`.

The data type of a value that represents a data type contract for unary
expressions is
```go
type UnaryOpContract struct {
	// Operand
	x Contract
	// Type of the result
	r DataType
}
```

### Binary expressions

[`GetConstBinExprType(x, y)`] If `x`, `y` are both of untyped numeric
constants (untyped integer, rune, floating-point, or complex), `O` is a binary
operator except `<<` and `>>`, and `x O y` is a valid expression, the type of
`x O y` is resolved as follows:
1. if one of `x`, `y` is an untyped complex, `x O y` is also untyped complex;
1. otherwise, if one of `x`, `y` is an untyped floating-point, `x O y` is also
   untyped floating-point;
1. otherwise, if one of `x`, `y` is an untyped rune, `x O y` is also untyped
   rune;
1. `x O y` is untyped integer.

Let `x`, `y` be `Expression`, let `O` be one of `|| &&`. We say that `x O y`
makes a contract if both `x` and `y` are of a (untyped) Boolean. If one of `x`,
`y` is `bool`, `x O y` is also `bool`. Otherwise, `x O y` is untyped Boolean.

Let `x`, `y` be `Expression`, let `O` be one of `== != < <= > >=`. We say that
`x O y` makes a contract if `x O y` is semantically valid. `x O y` is untyped
Boolean.

Let `x`, `y` be `Expression`. We say that `x + y` makes a contract if
* both `x`, `y` are both untyped numeric constants; in this case, `x + y` is
  `GetConstBinExprType(x, y)`;
* both `x`, `y` are untyped strings; in this case, `x + y` is also untyped
  string;
* one of `x`, `y` is an untyped constant, other one has a numeric or string
  type; an untyped constant is converted (must be convertible) to the type of
  the typed one, and `x + y` has a type of its first operand (after
  conversion), which is a type of a typed operand;
* both `x`, `y` have identical types, that are numeric or string; in this case,
  `x + y` has a type of `x`.

Let `x`, `y` be `Expression`. We say that `x - y` makes a contract if
* both `x`, `y` are both untyped numeric constants; in this case, `x - y` is
  `GetConstBinExprType(x, y)`;
* one of `x`, `y` is an untyped constant, other one has a numeric type; an
  untyped constant is converted (must be convertible) to the type of the typed
  one, and `x - y` has a type of its first operand (after conversion), which is
  a type of a typed operand;
* both `x`, `y` have identical types, that are numeric; in this case, `x - y`
  has a type of `x`.

Let `x`, `y` be `Expression`. We say that `x * y` makes a contract if
* both `x`, `y` are both untyped numeric constants; in this case, `x * y` is
  `GetConstBinExprType(x, y)`;
* one of `x`, `y` is an untyped constant, other one has a numeric type; an
  untyped constant is converted (must be convertible) to the type of the typed
  one, and `x * y` has a type of its first operand (after conversion), which is
  a type of a typed operand;
* both `x`, `y` have identical types, that are numeric; in this case, `x * y`
  has a type of `x`.

Let `x`, `y` be `Expression`. We say that `x / y` makes a contract if
* both `x`, `y` are both untyped numeric constants; in this case, `x / y` is
  `GetConstBinExprType(x, y)`;
* one of `x`, `y` is an untyped constant, other one has a numeric type; an
  untyped constant is converted (must be convertible) to the type of the typed
  one, and `x / y` has a type of its first operand (after conversion), which is
  a type of a typed operand;
* both `x`, `y` have identical types, that are numeric; in this case, `x / y`
  has a type of `x`.

Let `x`, `y` be `Expression`. We say that `x % y` makes a contract if
* both `x`, `y` are both untyped integer constants; in this case, `x % y` is
  `GetConstBinExprType(x, y)`;
* one of `x`, `y` is an untyped constant, other one has an integer type; an
  untyped constant is converted (must be convertible) to the type of the typed
  one, and `x % y` has a type of its first operand (after conversion), which is
  a type of a typed operand;
* both `x`, `y` have identical types, that are integers; in this case, `x % y`
  has a type of `x`.

Let `x`, `y` be `Expression` and let `O` be one of `& | ^ &^`. We say that
`x O y` makes a contract if
* both `x`, `y` are both untyped integer constants; in this case, `x O y` is
  `GetConstBinExprType(x, y)`;
* one of `x`, `y` is an untyped constant, other one has an integer type; an
  untyped constant is converted (must be convertible) to the type of the typed
  one, and `x O y` has a type of its first operand (after conversion), which is
  a type of a typed operand;
* both `x`, `y` have identical types, that are integers; in this case, `x O y`
  has a type of `x`.

Let `x`, `y` be `Expression` and let `O` be one of `<< >>`. We say that `x O y`
makes a contract if
* `y` is of unsigned integer type or is an untyped constant representable by
  `uint`;
* both `x`, `y` are constants, `x` is an untyped (numeric) constant; in this
  case, `x O y` is an (untyped) integer constant;
* both `x`, `y` are constants, `x` is a typed integer constant; in this case,
  `x O y` has a type of `x`;
* at least one of `x`, `y` is non-constant, `x` is an untyped (numeric)
  constant; in this case, `x` is first converted to the type it would assume if
  the shift expression were replaced by its left operand alone and `x O y` has
  a type of a such converted `x`;
* at least one of `x`, `y` is non-constant, `x` is a typed; in this case,
  `x O y` has a type of `x`.

The data type of a value that represents a data type contract for binary
expressions is
```go
type BinaryOpContract struct {
	// Operands
	x, y Contract
	// Type of the result
	r    DataType
}
```
