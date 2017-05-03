package statement

import (
	"fmt"
	"go/ast"
	"go/token"

	"github.com/gofed/symbols-extractor/pkg/parser/types"
	gotypes "github.com/gofed/symbols-extractor/pkg/types"
	"github.com/golang/glog"
)

// Parser parses go statements, e.g. block, declaration and definition of a function/method
type Parser struct {
	*types.Config
}

// New creates an instance of a statement parser
func New(config *types.Config) types.StatementParser {
	return &Parser{
		Config: config,
	}
}

func (ep *Parser) parseReceiver(receiver ast.Expr, skip_allocated bool) (gotypes.DataType, error) {
	glog.Infof("Processing Receiver: %#v\n", receiver)
	// Receiver's type must be of the form T or *T (possibly using parentheses) where T is a type name.
	switch typedExpr := receiver.(type) {
	case *ast.Ident:
		// search the identifier in the symbol table
		def, _, err := ep.SymbolTable.Lookup(typedExpr.Name)
		if err != nil {
			// Return an error so the function body processing can be postponed
			// TODO(jchaloup): return more information about the missing symbol so the
			// body can be re-processed right after the symbol is stored into the symbol table.
			return nil, err
		}

		if !skip_allocated {
			ep.AllocatedSymbolsTable.AddSymbol(def.Package, typedExpr.Name)
		}

		return &gotypes.Identifier{
			Def: typedExpr.Name,
		}, nil
	case *ast.StarExpr:
		switch idExpr := typedExpr.X.(type) {
		case *ast.Ident:
			// search the identifier in the symbol table
			def, _, err := ep.SymbolTable.Lookup(idExpr.Name)
			if err != nil {
				// Return an error so the function body processing can be postponed
				// TODO(jchaloup): return more information about the missing symbol so the
				// body can be re-processed right after the symbol is stored into the symbol table.
				return nil, err
			}

			if !skip_allocated {
				ep.AllocatedSymbolsTable.AddSymbol(def.Package, idExpr.Name)
			}

			return &gotypes.Pointer{
				Def: &gotypes.Identifier{
					Def: idExpr.Name,
				},
			}, nil
		default:
			return nil, fmt.Errorf("Method receiver %#v is not a pointer to an identifier", idExpr)
		}
	default:
		return nil, fmt.Errorf("Method receiver %#v is not a pointer to an identifier not an identifier", typedExpr)
	}
}

func (sp *Parser) ParseFuncDecl(d *ast.FuncDecl) (gotypes.DataType, error) {
	glog.Infof("Processing function %q declaration: %#v\n", d.Name.Name, d)
	// parseFunction does not store name of params, resp. results
	// as the names are not important. Just params, resp. results ordering is.
	// Thus, this method is used to parse function's signature only.
	funcDef, err := sp.TypeParser.Parse(d.Type)
	if err != nil {
		return nil, err
	}

	// Names of function params, resp. results are needed only when function's body is processed.
	// Thus, we can collect params, resp. results definition from symbol table and get names from
	// function's AST. (the ast is processed twice but in the second case, only params, resp. results names are read).

	// The function/method signature belongs to a package/file level symbol table.
	// The functonn/methods's params, resp. results, resp. receiver identifiers belong to
	// multi-level symbol table that is function/method's scoped. Once the body is left,
	// the multi-level symbol table is dropped.

	if d.Recv == nil {
		// Empty receiver => Function
		return funcDef, nil
	}

	// Receiver has a single parametr
	// https://golang.org/ref/spec#Receiver
	if len(d.Recv.List) != 1 || len(d.Recv.List[0].Names) > 2 {
		if len(d.Recv.List) != 1 {
			return nil, fmt.Errorf("Method %q has no receiver", d.Name.Name)
		}
		return nil, fmt.Errorf("Receiver is not a single parameter: %#v, %#v", d.Recv, d.Recv.List[0].Names)
	}

	//fmt.Printf("Rec Name: %#v\n", (*d.Recv).List[0].Names[0].Name)

	recDef, err := sp.parseReceiver(d.Recv.List[0].Type, false)
	if err != nil {
		return nil, err
	}

	methodDef := &gotypes.Method{
		Def:      funcDef,
		Receiver: recDef,
	}

	return methodDef, nil
}

func (sp *Parser) ParseFuncBody(funcDecl *ast.FuncDecl) error {
	glog.Infof("Processing function body: %#v\n", funcDecl)
	// Function/method signature is already stored in a symbol table.
	// From function/method's AST get its receiver, parameters and results,
	// construct a first level of a multi-level symbol table stack..
	// For each new block (including the body) push another level into the stack.
	sp.SymbolTable.Push()
	if err := sp.parseFuncHeadVariables(funcDecl); err != nil {
		return nil
	}
	sp.SymbolTable.Push()
	defer func() {
		sp.SymbolTable.Pop()
		sp.SymbolTable.Pop()
	}()
	// if funcDecl.Body is nil, then the function/method is declared only and its definition
	// is most likely in .s file(s)
	if funcDecl.Body == nil {
		return nil
	}

	// The stack will always have at least one symbol table (with receivers, resp. parameters, resp. results)
	for _, statement := range funcDecl.Body.List {
		if err := sp.Parse(statement); err != nil {
			return err
		}
	}

	return nil
}

func (sp *Parser) ParseValueSpec(spec *ast.ValueSpec) ([]*gotypes.SymbolDef, error) {
	glog.Infof("Processing value spec: %#v\n", spec)

	nLen := len(spec.Names)
	vLen := len(spec.Values)

	if nLen < vLen {
		return nil, fmt.Errorf("ValueSpec %#v has less number of identifieries on LHS (%v) than a number of expressions on RHS (%v)", spec, nLen, vLen)
	}

	var typeDef gotypes.DataType
	if spec.Type != nil {
		def, err := sp.TypeParser.Parse(spec.Type)
		if err != nil {
			return nil, err
		}
		typeDef = def
	}

	var symbolsDef = make([]*gotypes.SymbolDef, 0)

	for i := 0; i < vLen; i++ {
		if typeDef == nil && spec.Values[i] == nil {
			return nil, fmt.Errorf("No type nor value in ValueSpec declaration")
		}
		// TODO(jchaloup): if the variable type is an interface and the variable value type is a concrete type
		//                 note somewhere the concrete type must implemented the interface
		valueExpr, err := sp.ExprParser.Parse(spec.Values[i])
		if err != nil {
			return nil, err
		}
		if len(valueExpr) != 1 {
			return nil, fmt.Errorf("Expecting a single expression. Got a list instead: %#v", valueExpr)
		}
		// Put the variables/consts into the symbol table
		if spec.Names[i].Name == "_" {
			continue
		}
		if typeDef != nil {
			symbolsDef = append(symbolsDef, &gotypes.SymbolDef{
				Name:    spec.Names[i].Name,
				Package: sp.PackageName,
				Def:     typeDef,
			})
		} else {
			symbolsDef = append(symbolsDef, &gotypes.SymbolDef{
				Name:    spec.Names[i].Name,
				Package: sp.PackageName,
				Def:     valueExpr[0],
			})
		}
	}

	for i := vLen; i < nLen; i++ {
		if typeDef == nil {
			return nil, fmt.Errorf("No type in ValueSpec declaration for identifier at pos %v (starting index from 1)", i+1)
		}
		if spec.Names[i].Name == "_" {
			continue
		}
		symbolsDef = append(symbolsDef, &gotypes.SymbolDef{
			Name:    spec.Names[i].Name,
			Package: sp.PackageName,
			Def:     typeDef,
		})
	}

	return symbolsDef, nil
}

func (sp *Parser) parseDeclStmt(statement *ast.DeclStmt) error {
	switch decl := statement.Decl.(type) {
	case *ast.GenDecl:
		for _, spec := range decl.Specs {
			switch genDeclSpec := spec.(type) {
			case *ast.ValueSpec:
				glog.Infof("Processing value spec declaration %#v\n", genDeclSpec)
				defs, err := sp.ParseValueSpec(genDeclSpec)
				if err != nil {
					return err
				}
				for _, def := range defs {
					// TPDP(jchaloup): we should store all variables or non.
					// Given the error is set only if the variable already exists, it should not matter so much.
					if err := sp.SymbolTable.AddVariable(def); err != nil {
						return nil
					}
				}
			case *ast.TypeSpec:
				glog.Infof("Processing type spec declaration %#v\n", genDeclSpec)

				if err := sp.SymbolTable.AddDataType(&gotypes.SymbolDef{
					Name:    genDeclSpec.Name.Name,
					Package: "",
					Def:     nil,
				}); err != nil {
					return err
				}

				// TODO(jchaloup): capture the current state of the allocated symbol table
				// JIC the parsing ends with end error. Which can result into re-parsing later on.
				// Which can result in re-allocation. It should be enough two-level allocated symbol table.
				typeDef, err := sp.TypeParser.Parse(genDeclSpec.Type)
				if err != nil {
					return err
				}

				if err := sp.SymbolTable.AddDataType(&gotypes.SymbolDef{
					Name:    genDeclSpec.Name.Name,
					Package: "",
					Def:     typeDef,
				}); err != nil {
					return err
				}

			default:
				panic(fmt.Errorf("Unrecognized Gen declaration: %#v", decl))
			}
		}
	default:
		panic(fmt.Errorf("Unrecognized declaration: %#v", statement.Decl))
	}
	return nil
}

func (sp *Parser) parseLabeledStmt(statement *ast.LabeledStmt) error {
	glog.Infof("Processing labeled statement  %#v\n", statement)
	// the label is typeless
	return sp.Parse(statement.Stmt)
}

func (sp *Parser) parseExprStmt(statement *ast.ExprStmt) error {
	glog.Infof("Processing expression statement  %#v\n", statement)
	_, err := sp.ExprParser.Parse(statement.X)
	return err
}

func (sp *Parser) parseSendStmt(statement *ast.SendStmt) error {
	glog.Infof("Processing statement statement  %#v\n", statement)
	if _, err := sp.ExprParser.Parse(statement.Chan); err != nil {
		return err
	}
	// TODO(jchaloup): should check the statement.Chan type is really a channel.
	if _, err := sp.ExprParser.Parse(statement.Value); err != nil {
		return err
	}
	return nil
}

func (sp *Parser) parseIncDecStmt(statement *ast.IncDecStmt) error {
	glog.Infof("Processing inc dec statement  %#v\n", statement)
	// both --,++ has no type information
	// TODO(jchaloup): check the --/++ can be carried over the statement.X
	_, err := sp.ExprParser.Parse(statement.X)
	return err
}

func (sp *Parser) parseAssignStmt(statement *ast.AssignStmt) error {
	glog.Infof("Processing assignment statement  %#v\n", statement)
	// expr.Lhs = expr.Rhs
	// left-hand sice expression must be an identifier or a selector
	exprsSize := len(statement.Lhs)
	// Some assignments are of a different number of expression on both sides.
	// E.g. value, ok := somemap[key]
	// TODO(jchaloup): cover all the cases as well
	if exprsSize != len(statement.Rhs) {
		return fmt.Errorf("Number of expression of the left-hand side differs from ones on the right-hand side for: %#v vs. %#v", statement.Lhs, statement.Rhs)
	}

	// If the assignment token is token.DEFINE a variable gets stored into the symbol table.
	// If it is already there and has the same type, do not do anything. Error if the type is different.
	// If it is not there yet, add it into the table
	// If the token is token.ASSIGN the variable must be in the symbol table.
	// If it is and has the same type, do not do anything, Error, if the type is different.
	// If it is not there yet, error.
	for i := 0; i < exprsSize; i++ {
		// If the left-hand side id a selector (e.g. struct.field), we alredy know data type of the id.
		// So, just store the field's data type into the allocated symbol table
		//switch lhsExpr := expr.
		glog.Infof("Assignment LHs[%v]: %#v\n", i, statement.Lhs[i])
		glog.Infof("Assignment RHs[%v]: %#v\n", i, statement.Rhs[i])

		def, err := sp.ExprParser.Parse(statement.Rhs[i])
		if err != nil {
			return fmt.Errorf("Error when parsing Rhs[%v] expression of %#v: %v", i, statement, err)
		}

		if len(def) != 1 {
			return fmt.Errorf("Assignment element at pos %v does not return a single result: %#v", i, def)
		}

		switch lhsExpr := statement.Lhs[i].(type) {
		case *ast.Ident:
			// skip the anonymous variables
			if lhsExpr.Name == "_" {
				continue
			}

			// TODO(jchaloup): If the statement.Tok is not token.DEFINE, don't add the variable to the symbol table.
			//                 Instead, check the varible is of the same type (or compatible) as the already stored one.
			if statement.Tok == token.DEFINE {
				sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name:    lhsExpr.Name,
					Package: sp.PackageName,
					Def:     def[0],
				})
			}
		case *ast.SelectorExpr, *ast.StarExpr:
			_, err := sp.ExprParser.Parse(statement.Lhs[i])
			if err != nil {
				return nil
			}
		default:
			return fmt.Errorf("Lhs[%v] of an assignment type %#v is not recognized", i, statement.Lhs[i])
		}
	}
	return nil
}

func (sp *Parser) parseGoStmt(statement *ast.GoStmt) error {
	glog.Infof("Processing Go statement  %#v\n", statement)
	_, err := sp.ExprParser.Parse(statement.Call)
	return err
}

func (sp *Parser) parseBranchStmt(statement *ast.BranchStmt) error {
	glog.Infof("Processing branch statement  %#v\n", statement)
	return nil
}

func (sp *Parser) parseSwitchStmt(statement *ast.SwitchStmt) error {
	glog.Infof("Processing switch statement  %#v\n", statement)
	// ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
	// ExprCaseClause = ExprSwitchCase ":" StatementList .
	// ExprSwitchCase = "case" ExpressionList | "default" .
	if statement.Init != nil {
		sp.SymbolTable.Push()
		defer sp.SymbolTable.Pop()

		if err := sp.StmtParser.Parse(statement.Init); err != nil {
			return err
		}
	}

	if statement.Tag != nil {
		_, err := sp.ExprParser.Parse(statement.Tag)
		if err != nil {
			return err
		}
	}

	for _, stmt := range statement.Body.List {
		caseStmt, ok := stmt.(*ast.CaseClause)
		if !ok {
			return fmt.Errorf("Expected *ast.CaseClause in switch's body. Got %#v\n", stmt)
		}
		for _, caseExpr := range caseStmt.List {
			if _, err := sp.ExprParser.Parse(caseExpr); err != nil {
				return nil
			}
		}

		if caseStmt.Body != nil {
			if err := sp.Parse(&ast.BlockStmt{
				List: caseStmt.Body,
			}); err != nil {
				return err
			}
		}
	}

	return nil
}

func (sp *Parser) parseTypeSwitchStmt(statement *ast.TypeSwitchStmt) error {
	glog.Infof("Processing type switch statement  %#v\n", statement)
	if statement.Init != nil {
		sp.SymbolTable.Push()
		defer sp.SymbolTable.Pop()

		if err := sp.StmtParser.Parse(statement.Init); err != nil {
			return err
		}
	}

	sp.SymbolTable.Push()
	defer sp.SymbolTable.Pop()

	var rhsIdentifier *gotypes.Identifier

	switch assignStmt := statement.Assign.(type) {
	case *ast.AssignStmt:
		sp.SymbolTable.Push()
		defer sp.SymbolTable.Pop()

		if len(assignStmt.Lhs) != 1 {
			return fmt.Errorf("LHS of the type assert assignment must be a 'single' assignable expression")
		}

		if len(assignStmt.Rhs) != 1 {
			return fmt.Errorf("RHS of the type assert assignment must be a 'single' expression")
		}

		typeAssert, ok := assignStmt.Rhs[0].(*ast.TypeAssertExpr)
		if !ok {
			return fmt.Errorf("Expecting type assert in type switch assignemnt statement")
		}
		if typeAssert.Type != nil {
			fmt.Printf("type assert type is not set to literal 'type'. It is %#v instead\n", typeAssert.Type)
		}
		if _, err := sp.ExprParser.Parse(typeAssert.X); err != nil {
			return nil
		}

		ident, ok := assignStmt.Lhs[0].(*ast.Ident)
		if !ok {
			return fmt.Errorf("Expecting identifier on the RHS of the type switch assigment. Got %#v instead", assignStmt.Lhs[0])
		}

		if ident.Name != "_" {
			rhsIdentifier = &gotypes.Identifier{
				Def: ident.Name,
			}
		}
	case *ast.ExprStmt:
		typeAssert, ok := assignStmt.X.(*ast.TypeAssertExpr)
		if !ok {
			return fmt.Errorf("Expecting type assert in type switch")
		}
		if typeAssert.Type != nil {
			fmt.Printf("type assert type is not set to literal 'type'. It is %#v instead\n", typeAssert.Type)
		}
		if _, err := sp.ExprParser.Parse(typeAssert.X); err != nil {
			return nil
		}
	default:
		return fmt.Errorf("Unsupported statement in type switch")
	}

	for _, stmt := range statement.Body.List {
		caseStmt, ok := stmt.(*ast.CaseClause)
		if !ok {
			return fmt.Errorf("Expected *ast.CaseClause in switch's body. Got %#v\n", stmt)
		}
		if err := func() error {
			sp.SymbolTable.Push()
			defer sp.SymbolTable.Pop()
			// in case there are at least two data types (or none = default case), the type is interface{}
			if len(caseStmt.List) != 1 {
				for _, caseExpr := range caseStmt.List {
					if _, err := sp.TypeParser.Parse(caseExpr); err != nil {
						return err
					}
				}
				sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name:    rhsIdentifier.Def,
					Package: "",
					Def:     &gotypes.Interface{},
				})
			} else {
				rhsType, err := sp.TypeParser.Parse(caseStmt.List[0])
				if err != nil {
					return err
				}

				sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name:    rhsIdentifier.Def,
					Package: "",
					Def:     rhsType,
				})
			}

			if caseStmt.Body != nil {
				if err := sp.Parse(&ast.BlockStmt{
					List: caseStmt.Body,
				}); err != nil {
					return err
				}
			}

			return nil
		}(); err != nil {
			return nil
		}
	}

	return nil
}

// SelectStmt = "select" "{" { CommClause } "}" .
// CommClause = CommCase ":" StatementList .
// CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
// RecvStmt   = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
// RecvExpr   = Expression .
func (sp *Parser) parseSelectStmt(statement *ast.SelectStmt) error {
	glog.Infof("Processing select statement  %#v\n", statement)
	for _, stmt := range statement.Body.List {
		if err := func() error {
			sp.SymbolTable.Push()
			defer sp.SymbolTable.Pop()

			commClause, ok := stmt.(*ast.CommClause)
			if !ok {
				return fmt.Errorf("Select must be a list of commClause statements")
			}

			switch clause := commClause.Comm.(type) {
			case *ast.ExprStmt:
				if _, err := sp.ExprParser.Parse(clause.X); err != nil {
					return err
				}
			case *ast.SendStmt:
				if _, err := sp.ExprParser.Parse(clause.Chan); err != nil {
					return err
				}
				if _, err := sp.ExprParser.Parse(clause.Value); err != nil {
					return err
				}
			case *ast.AssignStmt:
				if len(clause.Rhs) != 1 {
					return fmt.Errorf("Expecting a single expression on the RHS of a clause assigment")
				}

				chExpr, ok := clause.Rhs[0].(*ast.UnaryExpr)
				if !ok {
					return fmt.Errorf("Expecting unary expression in a form of <-chan. Got %#v instead", clause.Rhs[0])
				}

				if chExpr.Op != token.ARROW {
					return fmt.Errorf("Expecting unary expression in a form of <-chan. Got %#v instead", chExpr)
				}

				rhsExpr, err := sp.ExprParser.Parse(chExpr.X)
				if err != nil {
					return err
				}

				if len(rhsExpr) != 1 {
					return fmt.Errorf("Expecting unary expression in a form of <-chan. Got %#v instead", rhsExpr)
				}

				rhsChannel, ok := rhsExpr[0].(*gotypes.Channel)
				if !ok {
					return fmt.Errorf("Expecting unary expression in a form of <-chan. Got %#v instead", rhsExpr[0])
				}

				switch lhsLen := len(clause.Lhs); lhsLen {
				case 0:
					return fmt.Errorf("Expecting at least one expression on the LHS of a clause assigment")
				case 1:
					if clause.Tok == token.DEFINE {
						// All LHS expressions must be identifiers
						// TODO(jchaloup): the expression can be surrounded by parenthesis!!! Make sure they are checked as well
						ident, ok := clause.Lhs[0].(*ast.Ident)
						if !ok {
							return fmt.Errorf("Expecting an identifier in select clause due to := assignment")
						}
						sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
							Name:    ident.Name,
							Package: "",
							Def:     rhsChannel,
						})
					}
				case 2:
					// Given a case is an implicit block, there is no need to check if any of to-be-declared variables is already declared
					// See http://www.tapirgames.com/blog/golang-block-and-scope
					// If an ordinary assignment (with = symbol) is used, all variables/selectors(and other assignable expression) must be already defined,
					// so they are not included as new variables.
					if clause.Tok == token.DEFINE {
						// All LHS expressions must be identifiers
						// TODO(jchaloup): the expression can be surrounded by parenthesis!!! Make sure they are checked as well
						ident1, ok := clause.Lhs[0].(*ast.Ident)
						if !ok {
							return fmt.Errorf("Expecting an identifier in select clause due to := assignment")
						}
						ident2, ok := clause.Lhs[1].(*ast.Ident)
						if !ok {
							return fmt.Errorf("Expecting an identifier in select clause due to := assignment")
						}

						sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
							Name:    ident1.Name,
							Package: "",
							Def:     rhsChannel,
						})

						sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
							Name:    ident2.Name,
							Package: "",
							Def:     &gotypes.Builtin{},
						})
					}
				default:
					return fmt.Errorf("Expecting at most two expression on the LHS of a clause assigment")
				}
			default:
				return fmt.Errorf("Unable to recognize selector CommClause CommCase. Got %#v\n", commClause.Comm)
			}

			if commClause.Body != nil {
				if err := sp.Parse(&ast.BlockStmt{
					List: commClause.Body,
				}); err != nil {
					return err
				}
			}
			return nil
		}(); err != nil {
			return err
		}
	}
	return nil
}

// ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
// Condition = Expression .
func (sp *Parser) parseForStmt(statement *ast.ForStmt) error {
	glog.Infof("Processing for statement  %#v\n", statement)
	sp.SymbolTable.Push()
	defer sp.SymbolTable.Pop()

	if statement.Init != nil {
		sp.StmtParser.Parse(statement.Init)
	}

	if statement.Cond != nil {
		if _, err := sp.ExprParser.Parse(statement.Cond); err != nil {
			return err
		}
	}

	if statement.Post != nil {
		sp.StmtParser.Parse(statement.Post)
	}

	return sp.Parse(statement.Body)
}

// RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
func (sp *Parser) parseRangeStmt(statement *ast.RangeStmt) error {
	glog.Infof("Processing range statement  %#v\n", statement)
	// If the assignment is = all expression on the LHS are already defined.
	// If the assignment is := all expression on the LHS are identifiers
	xExpr, err := sp.ExprParser.Parse(statement.X)
	if err != nil {
		return err
	}

	sp.SymbolTable.Push()
	defer sp.SymbolTable.Pop()

	if statement.Tok == token.DEFINE {

		var key, value gotypes.DataType
		// From https://golang.org/ref/spec#For_range
		//
		// Range expression                          1st value          2nd value
		//
		// array or slice  a  [n]E, *[n]E, or []E    index    i  int    a[i]       E
		// string          s  string type            index    i  int    see below  rune
		// map             m  map[K]V                key      k  K      m[k]       V
		// channel         c  chan E, <-chan E       element  e  E
		switch xExprType := xExpr[0].(type) {
		case *gotypes.Array:
			key = &gotypes.Builtin{Def: "int"}
			value = xExprType.Elmtype
		case *gotypes.Slice:
			key = &gotypes.Builtin{Def: "int"}
			value = xExprType.Elmtype
		case *gotypes.Builtin:
			if xExprType.Def != "string" {
				fmt.Errorf("Expecting string in range expression. Got %#v instead.", xExpr[0])
			}
			key = &gotypes.Builtin{Def: "int"}
			value = &gotypes.Builtin{Def: "rune"}
		case *gotypes.Map:
			key = xExprType.Keytype
			value = xExprType.Valuetype
		case *gotypes.Channel:
			key = xExprType.Value
		default:
			panic(fmt.Errorf("Unknown type of range expression: %#v", xExpr[0]))
		}

		if statement.Key != nil {
			keyIdent, ok := statement.Key.(*ast.Ident)
			if !ok {
				return fmt.Errorf("Expected key as an identifier in the for range statement. Got %#v instead.", statement.Key)
			}

			if keyIdent.Name != "_" {
				if err := sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name:    keyIdent.Name,
					Package: "",
					Def:     key,
				}); err != nil {
					return err
				}
			}
		}

		if statement.Value != nil {
			valueIdent, ok := statement.Value.(*ast.Ident)
			if !ok {
				return fmt.Errorf("Expected value as an identifier in the for range statement. Got %#v instead.", statement.Value)
			}

			if valueIdent.Name != "_" && value != nil {
				if err := sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name:    valueIdent.Name,
					Package: "",
					Def:     value,
				}); err != nil {
					return err
				}
			}
		}
	} else {
		if _, err := sp.ExprParser.Parse(statement.Key); err != nil {
			return nil
		}
		if _, err := sp.ExprParser.Parse(statement.Value); err != nil {
			return nil
		}
	}

	return sp.Parse(statement.Body)
}

func (sp *Parser) parseDeferStmt(statement *ast.DeferStmt) error {
	glog.Infof("Processing defer statement  %#v\n", statement)
	_, err := sp.ExprParser.Parse(statement.Call)
	return err
}

func (sp *Parser) parseIfStmt(statement *ast.IfStmt) error {
	glog.Infof("Processing if statement  %#v\n", statement)
	// If Init; Cond { Body } Else

	// The Init part is basically another block
	if statement.Init != nil {
		// The Init part must be an assignment statement
		if _, ok := statement.Init.(*ast.AssignStmt); !ok {
			return fmt.Errorf("If Init part must by an assignment statement if set")
		}
		sp.SymbolTable.Push()
		defer sp.SymbolTable.Pop()
		if err := sp.parseAssignStmt(statement.Init.(*ast.AssignStmt)); err != nil {
			return err
		}
	}

	_, err := sp.ExprParser.Parse(statement.Cond)
	if err != nil {
		return err
	}

	// Process the If-body
	sp.parseBlockStmt(statement.Body)

	return nil
}

func (sp *Parser) parseBlockStmt(statement *ast.BlockStmt) error {
	glog.Infof("Processing block statement  %#v\n", statement)
	sp.SymbolTable.Push()
	defer sp.SymbolTable.Pop()

	for _, blockItem := range statement.List {
		if err := sp.Parse(blockItem); err != nil {
			return err
		}
	}
	return nil
}

// Statement grammer at https://golang.org/ref/spec#Statements
//
// Statement =
// 	Declaration | LabeledStmt | SimpleStmt |
// 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
// 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
// 	DeferStmt .
//
// SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .

func (sp *Parser) Parse(statement ast.Stmt) error {
	switch stmtExpr := statement.(type) {
	case *ast.DeclStmt:
		return sp.parseDeclStmt(stmtExpr)
	case *ast.LabeledStmt:
		return sp.parseLabeledStmt(stmtExpr)
	case *ast.ExprStmt:
		return sp.parseExprStmt(stmtExpr)
	case *ast.SendStmt:
		return sp.parseSendStmt(stmtExpr)
	case *ast.IncDecStmt:
		return sp.parseIncDecStmt(stmtExpr)
	case *ast.AssignStmt:
		return sp.parseAssignStmt(stmtExpr)
	case *ast.GoStmt:
		return sp.parseGoStmt(stmtExpr)
	case *ast.ReturnStmt:
		for _, result := range stmtExpr.Results {
			_, err := sp.ExprParser.Parse(result)
			if err != nil {
				return err
			}
		}
	case *ast.BranchStmt:
		return sp.parseBranchStmt(stmtExpr)
	case *ast.BlockStmt:
		return sp.parseBlockStmt(stmtExpr)
	case *ast.IfStmt:
		return sp.parseIfStmt(stmtExpr)
	case *ast.SwitchStmt:
		return sp.parseSwitchStmt(stmtExpr)
	case *ast.TypeSwitchStmt:
		return sp.parseTypeSwitchStmt(stmtExpr)
	case *ast.SelectStmt:
		return sp.parseSelectStmt(stmtExpr)
	case *ast.ForStmt:
		return sp.parseForStmt(stmtExpr)
	case *ast.RangeStmt:
		return sp.parseRangeStmt(stmtExpr)
	case *ast.DeferStmt:
		return sp.parseDeferStmt(stmtExpr)
	default:
		panic(fmt.Errorf("Unknown statement %#v", statement))
	}

	return nil
}

func (sp *Parser) parseFuncHeadVariables(funcDecl *ast.FuncDecl) error {
	sp.AllocatedSymbolsTable.Lock()
	defer sp.AllocatedSymbolsTable.Unlock()
	if funcDecl.Recv != nil {
		// Receiver has a single parametr
		// https://golang.org/ref/spec#Receiver
		if len(funcDecl.Recv.List) != 1 || len(funcDecl.Recv.List[0].Names) > 2 {
			if len(funcDecl.Recv.List) != 1 {
				return fmt.Errorf("Method %q has no receiver", funcDecl.Name.Name)
			}
			return fmt.Errorf("Receiver is not a single parameter: %#v, %#v", funcDecl.Recv, funcDecl.Recv.List[0].Names)
		}

		def, err := sp.parseReceiver(funcDecl.Recv.List[0].Type, true)
		if err != nil {
			return err
		}
		// the receiver can be typed only
		if funcDecl.Recv.List[0].Names != nil {
			sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
				Name: (*funcDecl.Recv).List[0].Names[0].Name,
				Def:  def,
			})
		}
	}

	if funcDecl.Type.Params != nil {
		for _, field := range funcDecl.Type.Params.List {
			def, err := sp.TypeParser.Parse(field.Type)
			if err != nil {
				return err
			}

			// field.Names is always non-empty if param's datatype is defined
			for _, name := range field.Names {
				sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name: name.Name,
					Def:  def,
				})
			}
		}
	}

	if funcDecl.Type.Results != nil {
		for _, field := range funcDecl.Type.Results.List {
			def, err := sp.TypeParser.Parse(field.Type)
			if err != nil {
				return err
			}

			for _, name := range field.Names {
				sp.SymbolTable.AddVariable(&gotypes.SymbolDef{
					Name: name.Name,
					Def:  def,
				})
			}
		}
	}
	return nil
}
