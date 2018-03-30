% -------- Data types definitions (utilized by dialyzer) -----------
-type lex_state()   :: atom().

-type token()       :: {
                         TableName ::atom(), 
                         TableEntry::pos_integer(),
                         LineNo    ::pos_integer()
                    }.


% Holds the lexer data structures.
-record(lexer,
     {
          % 10 - Keywords table
          keywords  = []           ::list(),
          % 20 - Delimiters table
          delims    = []           ::list(),
          % 30 - Identificators table
          ids       = {0, []}      ::tuple(),
          % 40 - Constants table
          consts    = {0, []}      ::tuple(),
          % token buffer
          tokens    = []           ::list(token()),
          % State Transition Table
          table                    ::list(),
          curline   = 1            ::pos_integer(),
          % State names
          states                   ::list(lex_state()),
          % Accepting state names
          accepting                ::list(lex_state())
     }).

-record(parser,
     {
          keywords  = []           ::list(),
          delims    = []           ::list(),
          ids       = []           ::list(),
          consts    = []           ::list(integer()),
          tokens    = []           ::list(token())
     }).

