## A very simple Algebraic Binary Expression Tree equation evaluator.
# Based on the Shunting-Yard algorithm and ASTs.
# This library will work if you need basic integral arithmetic
# (although it can be modified for floating point)
# for operators such as add/sub mul/div mod and exponents.
#
# https://github.com/paulmoore/BET
# http://paulmoore.mit-license.org

# TODO
# * better handling of unary operators that can be pre or post fixed
# * variable argument functions and operators
# * preprocessing hooks for before/after/during AST generation
# * more built in functions and operatos, perhaps

{pow, sqrt, floor, ceil, min, max} = Math

# Each operator must define the following properties:
# * assoc: Associativity (fixity), can be 'left' or 'right'.  Describes how operators are grouped with same precedence.
# * prec: Precedence, higher number means precedence. e.g. multiplication has higher precedence than addition.
# * argc: Argument count, the number of arguments required to execute the operation.
# * fix: Can be one of ['pre', 'post', 'in'] which indicates the operator is prefix (sqrt 1), infix (2 + 3), or postfix (4!)
# * exec: A function which takes an array of numeric arguments, in order, and returns the result of the operation.
operators =
    '+':
        assoc: 'left'
        prec: 1
        argc: 2
        fix: 'in'
        exec: (args) -> args[0] + args[1]
    '-':
        assoc: 'left'
        prec: 1
        argc: 2
        fix: 'in'
        exec: (args) -> args[0] - args[1]
    '*':
        assoc: 'left'
        prec: 2
        argc: 2
        fix: 'in'
        exec: (args) -> args[0] * args[1]
    '/':
        assoc: 'left'
        prec: 2
        argc: 2
        fix: 'in'
        exec: (args) -> args[0] / args[1]
    'i/':
        assoc: 'left'
        prec: 2
        argc: 2
        fix: 'in'
        exec: (args) -> floor args[0] / args[1]
    '%':
        assoc: 'left'
        prec: 2
        argc: 2
        fix: 'in'
        exec: (args) -> args[0] % args[1]
    'mod':
        assoc: 'left'
        prec: 2
        argc: 2
        fix: 'in'
        exec: (args) -> (args[0] % args[1] + args[1]) % args[1]
    'neg':
        assoc: 'right'
        prec: 3
        argc: 1
        fix: 'pre'
        exec: (args) -> -args[0]
    '^':
        assoc: 'right'
        prec: 3
        argc: 2
        fix: 'in'
        exec: (args) -> pow args[0], args[1]
    '^2':
        assoc: 'left'
        prec: 4
        argc: 1
        fix: 'post'
        exec: (args) -> args[0] * args[0]
    '^3':
        assoc: 'left'
        prec: 4
        argc: 1
        fix: 'post'
        exec: (args) -> args[0] * args[0] * args[0]
    '!':
        assoc: 'right'
        prec: 4
        fix: 'post'
        argc: 1
        exec: (args) ->
            r = 1
            i = 2
            while i <= args[0]
                r *= i
                i++
            r
    '++':
        assoc: 'left'
        prec: 5
        fix: 'in'
        argc: 1
        exec: (args) -> ++args[0]
    '--':
        assoc: 'left'
        prec: 5
        fix: 'in'
        argc: 1
        exec: (args) -> --args[0]

# Functions are like operators, which take the form fn(arg0,arg1,arg2,...)
# Each function must define:
# * argc: The number of arguments the function takes
# * exec: A function which executes the function given the in order arguments.
functions =
    'sqrt':
        argc: 1
        exec: (args) -> sqrt args[0]
    'isqrt':
        argc: 1
        exec: (args) -> floor sqrt args[0]
    'floor':
        argc: 1
        exec: (args) -> floor args[0]
    'ceil':
        argc: 1
        exec: (args) -> ceil args[0]
    'min':
        argc: 2
        exec: (args) -> min args[0], args[1]
    'max':
        argc: 2
        exec: (args) -> max args[0], args[1]

## A straight forward implementation of Dijkstra's Shunting-Yard algorithm
#   http://en.wikipedia.org/wiki/Shunting-yard_algorithm
#
# @param input [array] An infix order equation, whose tokens are broken into an array.
# @return [mixed] An array which is a queue representation of the resulting AST.  Returns an Error on fail.
shuntingYard = (input) ->
    output = []
    stack = []
    for token, i in input
        if operators[token]?
            # token is an operator
            op1 = operators[token]
            switch op1.fix
                when 'pre' then stack.push token
                when 'post' then output.push token
                when 'in'
                    while stack.length > 0
                        token2 = stack[stack.length - 1]
                        # token may be a right or left paren, in which case we ignore
                        if operators[token2]?
                            op2 = operators[token2]
                            # the previous operator gets flushed to output if it has a higher precedence
                            if op1.assoc is 'left' and op1.prec <= op2.prec or op1.prec < op2.prec
                                output.push stack.pop()
                                continue
                        break
                    # finally, the operator is pushed onto the stack
                    stack.push token
                else return new Error "Operator #{token} at index #{i} has invalid fix property: #{op1.fix}, found in: #{input.join ''}"
        else if functions[token]?
            stack.push token
        else if token is ','
            while stack.length > 0
                token = stack[stack.length - 1]
                if token isnt '('
                    output.push token
                    stack.pop()
                else
                    matched = true
                    break
            if not matched
                return new Error "Parse error, no matching left paren for function at index #{i} of #{input.join ''}"
        else if token is '('
            # left parens are just placed directly onto the stack
            stack.push token
        else if token is ')'
            # for right parens, we must search the stack for a pairing left paren
            while stack.length > 0
                token = stack.pop()
                if token is '('
                    matched = true
                    break
                else
                    output.push token
            if not matched
                return new Error "Parse error, no matching left paren at index #{i} of #{input.join ''}"
            if stack.length > 0 and functions[stack[stack.length - 1]]?
                output.push stack.pop()
        else if typeof token is 'number'
            # token is a number, can be outputted directly
            output.push token
        else
            return new Error "Parse error, token #{token} is not a known operator, paren, or number type at index #{i} of #{input.join ''}"
    while stack.length > 0
        token = stack.pop()
        if token in ['(', ')']
            return new Error "Parse error, mismatched parens, found extra #{token} in #{input.join ''}, operators left in stack: #{stack.join ' '}"
        else
            output.push token
    output

## Evaluates an equation for a numerical result.
#
# @param input [array] The equation in infix order broken into individual tokens.
# @return [mixed] Returns the result of the equation as a number on success, or an instance of Error on fail.
evaluate = (input, next) ->
    result = NaN
    error = null
    if not Array.isArray input
        error = new Error "Input must be array but got #{input?.toString() or 'null'}"
    else if input.length is 1 and typeof input[0] is 'number'
        result = input[0]
    else
        # generate the AST
        output = shuntingYard(input)
        if output instanceof Error
            error = output
        else
            queue = output
            stack = []
            while queue.length + stack.length > 0
                if queue.length > 0
                    # first, we always push one more op/num onto the stack if we have one
                    token = queue.shift()
                    stack.push token
                if stack.length > 0
                    # check the stack for a top level operator
                    top = stack[stack.length - 1]
                    fnop = operators[top] or functions[top]
                    # do we have enough arguments to execute it?
                    if fnop? and stack.length > fnop.argc
                        stack.pop()
                        args = []
                        # pop the operator and it's arguments
                        i = fnop.argc
                        while i > 0
                            args.unshift stack.pop()
                            i--
                        result = fnop.exec args
                        # push the result of the operation back onto the stack
                        if queue.length + stack.length > 0
                            stack.push result
                    else if queue.length is 0
                        error = new Error "Cannot make any progress on equation, did you misplace a unary operator? stack: #{stack.toString()}"
                        result = NaN
                        break
    if isNaN(result) and not error?
        error = new Error 'Calculation error, check equation syntax'
    next? error, result

# Module Exports
BET = {}

BET.operators = operators
BET.functions = functions
if process?.nextTick?
    BET.evaluate = (input, next) -> process.nextTick -> evaluate input, next
BET.evaluateSync = (input) ->
    ret = NaN
    evaluate input, (err, res) ->
        throw err if err?
        ret = res
    ret

module.exports = BET if module?
window.BET = BET if window?
