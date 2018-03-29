// This work is subject to the CC0 1.0 Universal (CC0 1.0) Public Domain Dedication
// license. Its contents can be found at:
// http://creativecommonc.org/publicdomain/zero/1.0/

package asm

import (
	"zisc"
	"zisc/ast"
)

func (a *Assembler) buildData(t *ast.AST) (err error) {
	mem := a.chip.Mem
	eom := mem.Len()
	a.addr = mem.Data

	if a.addr >= eom {
		return error_(errMemOverflow, "Data")
	}

	for i := range t.Data {
		if _, ok := a.labels[t.Data[i].Key]; ok {
			return error_(errDuplicateLabel, t.Data[i].Key)
		}

		a.labels[t.Data[i].Key] = a.addr

		switch tt := t.Data[i].Value.(type) {
		case ast.Number:
			mem.D[a.addr] = tt.Word()
			if a.addr++; a.addr == 0 || a.addr >= eom {
				return error_(errMemOverflow, "Data")
			}

		case ast.Char:
			mem.D[a.addr] = tt.Word()
			if a.addr++; a.addr == 0 || a.addr >= eom {
				return error_(errMemOverflow, "Data")
			}

		case ast.String:
			words := tt.Words()
			copy(mem.D[a.addr:], words)
			a.addr += zisc.Word(len(words))

			if a.addr < zisc.Word(len(words)) || a.addr >= eom {
				return error_(errMemOverflow, "Data")
			}
		}
	}
	return
}
