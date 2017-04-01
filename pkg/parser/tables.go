package parser

import (
	"fmt"
	"strings"
)

// - count a number of each symbol used (to watch how intensively is a given symbol used)
// - each AS table is per file, AS package is a union of file AS tables
//

type AllocatedSymbolsTable struct {
	File    string
	Package string
	// symbol's name is in a PACKAGE.ID form
	// if the PACKAGE is empty, the ID is considired as the embedded symbol
	symbols map[string]int
}

func NewAllocatableSymbolsTable() *AllocatedSymbolsTable {
	return &AllocatedSymbolsTable{
		symbols: make(map[string]int),
	}
}

func (ast *AllocatedSymbolsTable) AddSymbol(origin, id string) {
	var key string
	if origin != "" {
		key = strings.Join([]string{origin, id}, ".")
	} else {
		key = id
	}

	count, ok := ast.symbols[key]
	if !ok {
		ast.symbols[key] = 1
	} else {
		ast.symbols[key] = count + 1
	}
}

func (ast *AllocatedSymbolsTable) Print() {
	for key := range ast.symbols {
		fmt.Printf("%v:\t%v\n", key, ast.symbols[key])
	}
}