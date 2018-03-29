package gaia

import (
    "io/ioutil"
    "regexp"
    "strings"
)

type Data struct {
    contents string
}

/**
 *  High-level abstraction for simplifying work
 *  with Lisp source files
 */
func (data *Data) loadSource (fileToParse string) {
    data.parseFile(fileToParse)
    data.clearContents()
}


/**
 *  Reading and fetching data from Lisp Source file
 */
func (data *Data) parseFile (file string) {
    fileContents, _ := ioutil.ReadFile(file)
    data.contents = string(fileContents)
}


/**
 *  Removing unneded (for parsing and building AST) parts of
 *  Lisp source file
 */
func (data *Data) clearContents () {
    lineSplittedData := strings.Split(data.contents, string('\n'))

    var clearedData string

    inlineCommentRemover := regexp.MustCompile(";(.*)$")

    // removing empty lines and comments
    for _, line := range lineSplittedData {
        if len(line) > 0 && line[0] != ';' {
            clearedData += inlineCommentRemover.ReplaceAllString(line, "") + "\n"
        }
    }

    // '() -> (list )
    r := regexp.MustCompile("'\\(")
    clearedData = r.ReplaceAllString(clearedData, "(list ")

    data.contents = clearedData
}


/**
 *  Feteching legal statemets at the top level
 *  of parsed Lisp source file
 */
func (data *Data) searchStatements () []string {
    splittedBytes := strings.Split(data.contents, "")

    r := regexp.MustCompile("\\s{2,}")
    statements := make([]string, 0)
    openBrackets := 0
    statement := ""

    for index := range splittedBytes {

        switch splittedBytes[index] {
        case string('('):
            openBrackets += 1
            statement = statement + splittedBytes[index]

        case string(')'):
            openBrackets -= 1
            statement = statement + splittedBytes[index]

            if openBrackets == 0 {
                statements = append(statements, r.ReplaceAllString(statement, " "))
                statement = ""
            }
        case string('\n'):
        default:
            statement = statement + splittedBytes[index]
        }
    }

    return statements
}

// vim: noai:ts=4:sw=4
