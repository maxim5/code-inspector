// Implementation of an echo server based on LSP

package main

import (
	"flag"
	"fmt"
	"strings"
	"os"
	// This will compile using your implementation of LSP
	"P1-f12/contrib/lsp12"
	// Use proxied version of UDP
	"P1-f12/official/lspnet"
	// Get access to debugging messages
	"P1-f12/official/lsplog"
)

func runserver(srv *lsp12.LspServer) {
	for {
		// Read from client
		id, payload, rerr := srv.Read()
		if rerr != nil  {
			fmt.Printf("Connection %d has died.  Error message %s\n", id, rerr.Error())
		} else {
			s := string(payload)
			lsplog.Vlogf(6, "Connection %d.  Received '%s'\n", id, s)
			payload = []byte(strings.ToUpper(s))
			// Echo back to client
			werr := srv.Write(id, payload)
			if werr != nil {
				fmt.Printf("Connection %d.  Write failed.  Error message %s\n", id, werr.Error())
			}
		}
	}
}

func main() {
	var ihelp *bool = flag.Bool("h", false, "Print help information")
	var iport *int = flag.Int("p", 6666, "Port number")
	var iverb *int = flag.Int("v", 1, "Verbosity (0-6)")
	var idrop *int = flag.Int("r", 0, "Network packet drop percentage")
	var elim *int = flag.Int("k", 5, "Epoch limit")
	var ems *int = flag.Int("d", 2000, "Epoch duration (millisecconds)")
	flag.Parse()
	if *ihelp {
		flag.Usage()
		os.Exit(0)
	}
	var port int = *iport
	if flag.NArg() > 0 {
		nread, _ := fmt.Sscanf(flag.Arg(0), "%d", &port)
		if nread != 1 {
			flag.Usage()
			os.Exit(0)
		}
	}
	params := &lsp12.LspParams{*elim,*ems}

	lsplog.SetVerbose(*iverb)
	lspnet.SetWriteDropPercent(*idrop)
	fmt.Printf("Establishing server on port %d\n", port)
	srv, err := lsp12.NewLspServer(port, params)
	if err != nil {
		fmt.Printf("... failed.  Error message %s\n", err.Error())
	} else {
		runserver(srv)
	}
}
