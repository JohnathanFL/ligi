import ast

makeTags {
  iColon: ":",
  iComma: ",",
  iLBrace: "{",
  iRBrace: "}",
  iLParen: "(",
  iRParen: ")",
  iLBracket: "[",
  iRBracket: "]",
  iStoreIn: "->",

  iAssg: "=",
  iAddAssg: "+=",
  iSubAssg: "-=",
  iMulAssg: "*=",
  iDivAssg: "/=",

  iLambda: "=>",

  iSpaceship: "<=>",

  iAnd: "and",
  iOr: "or",
  iXor: "xor",

  iEq: "==",
  iNeq: "!=",
  iLt: "<",
  iGt: ">",
  iLtEq: "<=",
  iGtEq: ">=",

  iIn: "in",
  iNotIn: "notin",

  iAdd: "+",
  iSub: "-",
  iMul: "*",
  iPtr: "*", # One benefit of the StrID setup is it's wonderfully easy to alias keywords/sigils
  iDiv: "/",
  iMod: "mod",
  iExpand: "...",

  iAccess: ".",
  iAccessPipe: ".>",
  iOptAccess: ".?",
  iOptAccessPipe: ".?>",

  iFn: "fn",
  iMacro: "macro",

  iLet: "let",
  iVar: "var",
  iCVar: "cvar",
  iField: "field",
  iCase: "case",

  iIf: "if",
  iElIf: "elif",
  iWhen: "when",
  iWhile: "while",
  iLoop: "loop",
  iFor: "for",

  iElse: "else",
  iFinally: "finally",
  iIs: "is",

  iAssert: "assert",
  iExpect: "expect",
  iBreak: "break",
  iReturn: "return",
  iDelete: "delete",
  iContinue: "continue",

  iSink: "_",

  # Constants
  iTrue: "true",
  iFalse: "false",


  # Purely AST ids - ([i]d of [b]uiltin)
  # Because these are just words, this also means Ligi can be homoiconic
  #
  # ###################
  # # Core statements #
  # ###################
  #
  # For these three, children[1] is always the typedesc. If no desc was present, it's `_`
  ibBlock: "@block", # a series of statements, with the last one yielding the value
  ibTuple: "@tuple", # a set of values
  ibArray: "@array", # a series of values
  # ibCall: "@()",       # No such thing. Lists are always function calls
  ibAt: "@at", # an indexing, with [1] being what to index and [2.._] being the arguments
  ibArm: "@arm", # a normal arm of a control statement. Exact meaning dependant upon which.
  ibIf: "@if", # an if statement. Each arm is a sequential if/elif. Last two may be @else/@finally
  ibWhen: "@when", # a when statement.
  ibWhile: "@while", # a while loop
  ibFor: "@for", # a for loop
  ibLoop: "@loop", # a loop-de-loop
  ibBind: "@bind", # a bind statement. [1] is the spec, [2.._] are the expressions to bind
  ibCompileLog: "@compileLog", # Comptime logging
  ibCompileError: "@compileError", # Comptime error
  ibEval: "@eval", # Evaluate a string of ligi
  ibEmbedFile: "@embedFile", # Embed a file as a string

  #
  # ##################
  # # Ext statements #
  # ##################
  #
  ibFunc: "@func", # a result-location function or macro definition. [1] is either fn or macro
}

