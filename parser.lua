-- require "strict"

local ffi = require "ffi"

local lexer = ffi.load("./libligi.so")

ffi.cdef [[
  typedef size_t StrID;
  enum TagType {
    EOF = 0,

    Newline = 1,
    Indent = 2,
    Dedent = 3,

    LParen = 4,
    RParen = 5,
    LBrace = 6,
    RBrace = 7,
    LBracket = 8,
    RBracket = 9,

    Semicolon = 10,
    Comma = 11,
    Tag = 12,

    Str = 13,
    Word = 14,
    Sigil = 15,
    Strop = 15,
  };

  typedef struct {
    enum TagType tag;
    StrID str;
  } Token;

  typedef void File;
  typedef void StringCache;

  File* newFile(StringCache* cache, const char* input, size_t len);
  StringCache* newCache();
  
  Token lex(void* self);
  size_t strID(void* self, const char* str, size_t len);
  const char* idStr(void* self, size_t id);
]]

local newFile, newCache, lex, strID, idStr = lexer.newFile, lexer.newCache, lexer.lex, lexer.strID, lexer.idStr
local string = ffi.string

local cache = newCache()
local input = [[a = 1]]
local file = newFile(cache, input, #input)

local tok = lex(file)
print(string(idStr(file, tok.str)))
