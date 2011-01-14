require "dump"
require "generator"

local code = io.input(arg[1]):read("*a")

--code = [[abc : A1; ]]
--print(code)

local out = true

local ret, tokens, idens = generator.parse(code)

if out then
print[[
%code top  {
#include "parser.inc.h"
}

%code provides {

  /* exports node names */
  const char **names;
  const unsigned char *translate;

  int yyparse(parser_t *);
  int yylex(YYSTYPE *, parser_t *);
  int yyerror(parser_t *, const char *);
}

%pure_parser
%parse-param {parser_t * parser}
%lex-param {parser_t * parser}
]]

print("/* tokens */")
for i,v in pairs(tokens) do
  print(string.format("%%token <ref> %s", tostring(v)))
end

print("/* idens */")
for i,v in pairs(idens) do
  print(string.format("%%type <ref> %s", tostring(v)))
end

print[[
%token <ref> END 0 "end-of-file"
%token <ref> error ERROR

%token <ref> LINE_COMMENT
%token <ref> MULTI_LINE_COMMENT

%union {
  int ref;
}

%start source
%%

]]
print("/* rules */")
end

local tabmet = {
  __index = function(s,k)
    if k == "res" then
      local iden = s[1]
      --dump(s,nil,nil,true)
      assert(iden.tag == "iden")
      return iden.value
    elseif k == "rules" then
      --dump(s,nil,nil,true)
      return s[2]
    end
  end
}

local tabmut = {
  __tostring = function(s,k)
    local str = ""
    --dump(s,nil,nil,true)
    for i,v in ipairs(s) do
      str = str .. v.value .. " "
    end
    return str
  end
}

--dump(ret,nil,nil,true)

local tmpl1 = [[
    lua_State * L = parser->L;
    lua_newtable(L);
    int top = lua_gettop(L);

    lua_pushstring(L, "tag");
    lua_pushstring(L, "%s");
    lua_settable(L, top);

    lua_pushstring(L, "_variant");
    lua_pushnumber(L, %d);
    lua_settable(L, top);
]]
-- tag.name,
local tmpl2 = [[
    lua_pushstring(L, "%s");
    lua_rawgeti(L, LUA_REGISTRYINDEX, $%d);
    lua_settable(L, top);
    luaL_unref(L, LUA_REGISTRYINDEX, $%d);
]]
-- child.name, child.ref
local tmpl3 = [[
    $$ = luaL_ref(L, LUA_REGISTRYINDEX);
    lua_rawgeti(L, LUA_REGISTRYINDEX, $$);
    lua_setfield(L, LUA_GLOBALSINDEX, "tree");
]]

local tmplhan =[[
    {
      lua_State * L = parser->L;

      lua_getfield(L, LUA_GLOBALSINDEX, "parser");
      lua_getfield(L, -1, "%s"); /* name */
      lua_remove(L, -2);
      lua_rawgeti(L, LUA_REGISTRYINDEX, $$);
      lua_pushnumber(L, %d); /* variant */
      lua_getfield(L, LUA_GLOBALSINDEX, "typedefs");
      int e = lua_pcall(L, 3, 0, 0);
      if (e) {
        yyerror(parser, lua_tostring(parser->L, -1));
        lua_pop(L, 1);
      }
    }
]]

function special_handler(i, v, variant, rule)
  if v.res == "declaration" then
    print(string.format(tmplhan, "declaration_handler", variant))
  end
end

for i,v in ipairs(ret) do
  setmetatable(v,tabmet)
  print(v.res)
  for j, rule in ipairs(v.rules) do
    setmetatable(rule,tabmut)
    print(string.format("  %s %s",j==1 and ":" or "|", tostring(rule)))
    print("  {")
    print(string.format("    /* %s */",
      (function(r)
        local str = "$$ <- "
        for j,w in ipairs(r) do
          str = str .. string.format("$%d(%s) ",j, w.tag)
        end
        return str
      end)(rule)))

    print(string.format(tmpl1, v.res, j))

    for j,w in ipairs(rule) do
      print(string.format(tmpl2, string.lower(tostring(w)), j, j))
    end

    print(string.format(tmpl3))

    special_handler(i, v, j, rule)

    print("  }")
  end
  print("  ;\n")
end

if out then
print[[
%%

int parser_push_token(parser_t * parser, token_t * token)
{
  lua_State * L = parser->L;

  lua_newtable(L);
  int top = lua_gettop(L);

  lua_pushstring(L, "tag");
  lua_pushstring(L, "token");
  lua_settable(L, top);

  lua_pushstring(L, "_type");
  lua_pushnumber(L, token->type);
  lua_settable(L, top);

  lua_pushstring(L, "value");
  lua_pushlstring(L, (const char *)token->start, token->end - token->start);
  lua_settable(L, top);

  lua_pushstring(L, "_line");
  lua_pushnumber(L, token->line);
  lua_settable(L, top);

  int ref = luaL_ref(L, LUA_REGISTRYINDEX);

  return ref;
}

int parser_push_type(parser_t * parser, token_t * token, int type_ref)
{
  lua_State * L = parser->L;
  
  lua_newtable(L);
  int top = lua_gettop(L);

  /*for (int i = 1; i <= lua_gettop(L); i++) {
    fprintf(stderr, "%d, %d\n", i, lua_isnumber(L, i));
  }
  fprintf(stderr, "top: %d\n", lua_gettop(L));*/
  
  //luaL_error(L, "fail");
  
  lua_pushstring(L, "tag");
  lua_pushstring(L, "token");
  lua_settable(L, top);
  
  lua_pushstring(L, "_type");
  lua_pushnumber(L, token->type);
  lua_settable(L, top);

  lua_pushstring(L, "value");
  lua_pushlstring(L, (const char *)token->start, token->end - token->start);
  lua_settable(L, top);

  lua_pushstring(L, "_line");
  lua_pushnumber(L, token->line);
  lua_settable(L, top);
  
  lua_pushstring(L, "_ref");
  lua_rawgeti(L, LUA_REGISTRYINDEX, type_ref);
  lua_settable(L, top);
  luaL_unref(L, LUA_REGISTRYINDEX, type_ref);

  int ref = luaL_ref(L, LUA_REGISTRYINDEX);
  
  return ref;
}

int yylex(YYSTYPE * s, parser_t * parser) {
  err_t err = lexer_next(&(parser->lexer), parser->L);
  if (err.composite) {
    return 0;
  } else {
    /*fprintf(stderr, "<....... GET: done: %d, eof: %p, cs: %d, ts: =%p, te: %p, tt: %d\n",
        parser->lexer.done,
        (void *)parser->lexer.eof,
        parser->lexer.cs,
        (void *)parser->lexer.ts,
        (void *)parser->lexer.ts,
        (void *)parser->lexer.te,
        parser->lexer.token_type
        );*/

    if (!parser->lexer.done) {
      token_t token = {
        .type = parser->lexer.token_type,
        .start = parser->lexer.ts,
        .end = parser->lexer.te,
        .line = parser->lexer.line,
      };
      
      if (token.type == TYPE_NAME)
        s->ref = parser_push_type(parser, &token, parser->lexer.type_ref);
      else
        s->ref = parser_push_token(parser, &token);

      return token.type;
    } else {
      s->ref = LUA_NOREF;
      return 0;
    }
  }
}

int yyerror(parser_t * parser, const char * error) {
  fflush(stderr);
  if ( parser->lexer.ts < parser->lexer.pe && parser->lexer.te < parser->lexer.pe) {
    fprintf(stderr, " --------------- ERROR: '%s' at line %u \"%.*s\"\n",
        error, parser->lexer.line, (int)(parser->lexer.te-parser->lexer.line_start-1),
        parser->lexer.line_start+1);
  } else {
          fprintf(stderr, "******** ERROR: '%s'\n", error);
  }

  return 0;
}

/* exports node names */
const char **names = (const char **)yytname;
const unsigned char *translate = (const unsigned char *)yytranslate;
]]
end
