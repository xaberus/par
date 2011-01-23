require "dump"
require "lpeg"

local lpeg = lpeg

local stdout = io.stdout
local stderr = io.stderr
local input = io.input
local open = io.open
local format = string.format
local gsub = string.gsub
local pairs = pairs
local ipairs = ipairs
local setmetatable = setmetatable
local tonumber = tonumber
local type = type
local collectgarbage = collectgarbage

local dump = dump

module("generator")

local P, R, S, C, Cc, Ct, Cg, Cb, V = lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Ct, lpeg.Cg, lpeg.Cb, lpeg.V

local ws = S" \n\t"

local underline = S "_"
local upper_case = R("AZ")
local lower_case = R("az")
local digit = R("09")
local terminal_any = upper_case + lower_case + underline + digit

local quot = P('"')
local colon = P(":")
local comma = P(",")
local semicolon = P(";")
local pipe = P("|")
local left_brace = P("{")
local right_brace = P("}")
local left_paren = P("(")
local right_paren = P(")")

local at = lpeg.P"@"

local comment_start = lpeg.P"/*"
local comment_end = lpeg.P"*/"

local token_nil =  P"nil"
local token_func =  P"func"
local token_eof =  P"eof"
local token_error =  P"error"

-- symbols

local comment = V"comment"
local space = V"space"

local terminal = V"terminal"
local non_terminal = V"non_terminal"
local rule = V"rule"
local ruleblock = V"ruleblock"
local production = V"production"

local lua_block = V"lua_block"

local tokens = V"tokens"
local token_definitions = V"token_definitions"
local token_definition = V"token_definition"
local token_args = V"token_args"

local token_number =  V"token_number"
local token_string =  V"token_string"

-- grammar

local grammar = P {
  "entry_point",

  space =
    (ws + comment)^0,

  comment =
    P"/*" *  (P(1) - P"*/")^0 * P"*/",

  entry_point =
    (space * tokens * space * Ct(production^1) * space)
      / function(...) return {tag = "ast", ...} end,

  tokens =
    (space * P"tokens" * space * left_brace * token_definitions * right_brace)
      / function(...) 
        local args = {...}
        if type(args[1]) == "string" then return {tag = "tokens"} end
        return {tag = "tokens", ...}
      end,

  token_definitions =
    (space * token_definition^0 * space),

  token_definition =
    (space * terminal * space * left_paren * space * token_args * space * right_paren * semicolon * space)
      / function(token, id, rep)
        id = (id ~= "nil") and tonumber(id) or nil
        return {tag = "token_definition", token = token, id = id, rep = rep} 
      end,

  token_args = --C((P(1)-P")")^0),
    (
      C(token_nil)
      + C(token_number)
    ) * space * (comma * space * space * 
      (
        C(token_nil)
        + C(token_eof)
        + C(token_error)
        + token_string
        + C(token_func)
      )
    )^-1,

  token_number =
    ( P"0" + (R"19" * digit^0)),

  token_string =
    quot * C((P(1) - quot)^0) *  quot,

  production =
    (space * non_terminal * space * colon * ruleblock * semicolon * space)
      / function(red, rules) return {tag = "production", red = red, rules = rules} end,

  ruleblock =
    (space * rule * (pipe * rule)^0 * space)
      / function(...) return {tag = "ruleblock", ...} end,

  rule =
    (space * 
      Ct(((terminal + non_terminal) * space)^1) *
      (lua_block^-1) * space)
    / function(parts, block) return {tag = "rule", parts = parts, block = block} end,

  terminal =
    C(upper_case * (terminal_any)^0 ,"terminal")
      / function(value) return {tag = "terminal", value = value} end,

  non_terminal =
    C(lower_case * (terminal_any)^0, "non_terminal")
      / function(value) return {tag = "non_terminal", value = value} end,

  lua_block =
    space * at * space * P"L" * space * left_brace * C((P(1) - right_brace)^0) * right_brace * space
      / function(str) return str end,
}


local function mkvaluetab(tab, fun)
  local t = {}
  for  i,v in ipairs(tab) do
    t[fun(v)] = v
  end
  return t
end

function parse(src)
  local ret = lpeg.match(grammar, src)

  if not ret then
    error("")
  end

  local c = 256
  local tokens = mkvaluetab(ret[1],
    function(v)
      if not v.id then
        v.id = c
        c=c + 1
      end
      return v.token.value
    end)

  local productions = ret[2]
  local map = mkvaluetab(ret[2],
    function(v)
      return v.red.value
    end)

  local fatal = false

  -- check & clean up
  for i, prod in ipairs(productions) do
    for j, rule in ipairs(prod.rules) do
      for k, part in ipairs(rule.parts) do
        if part.tag == "terminal" then
          tokendef = tokens[part.value]
          if not tokendef then
            stderr:write(string.format("no lexical definition for >%s<;\n", part.value))
            fatal = true
          end
          -- make uinque
          rule.parts[k] = tokendef
        elseif part.tag == "non_terminal" then
          -- make uinque
          rule.parts[k] = map[part.value].red
        end
      end
    end
  end
  collectgarbage("collect")

  --dump(tokens)
  --dump(productions)

  stderr:flush()

  if fatal then
    error("incomplete definitions!")
  end

  return productions, tokens, map
end

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

local header = [[
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

local middle = [[

%token <ref> error ERROR

%union {
  int ref;
}

%start source
%%

]]

local footer = [[
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
  
  /*printf("pushed %d\n", ref);
  lua_getfield(L, LUA_GLOBALSINDEX, "dump");
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
  lua_pcall(L, 1, 0, 0);*/
  
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
    fprintf(stderr, "error: '%s' at line %u \"%.*s\"\n",
        error, parser->lexer.line, (int)(parser->lexer.te-parser->lexer.ts),
        parser->lexer.ts);
  } else {
          fprintf(stderr, "error: '%s'\n", error);
  }
  fflush(stderr);
  

  return 0;
}

/* exports node names */
const char **names = (const char **)yytname;
const unsigned char *translate = (const unsigned char *)yytranslate;
]]


local stmpl1 = [[
    lua_State * L = parser->L;
    
    luaL_checkstack(L, %d, "could not grow stack!");

    lua_getfield(L, LUA_GLOBALSINDEX, "parser");
    lua_getfield(L, -1, "%s_%d"); // handler name 
    lua_remove(L, -2);
    
    // env
    lua_getfield(L, LUA_GLOBALSINDEX, "env");

]]
local function genstmpl1(name, variant, n)
  return format(stmpl1, n, name, variant)
end

local function genstmpl2(k, name)
  return
    format("    /* %s */\n", name) ..
    format("    lua_rawgeti(L, LUA_REGISTRYINDEX, $%d);\n", k) ..
    format("    luaL_unref(L, LUA_REGISTRYINDEX, $%d);\n", k)
end

local stmpl3 = [[
    int e = lua_pcall(L, 1 + %d, 1, 0); // env + args
    if (e) {
      yyerror(parser, lua_tostring(parser->L, -1));
      lua_pop(L, 1);
    }

]]
local function genstmpl3(n)
  return format(stmpl3, n)
end
local function genstmpl4()
  return
    format("    if (lua_isnil(L, -1)) {\n") ..
    format("      yyerror(parser, \"parser operation returned nil!\");\n") ..
    format("    } else {\n") ..
    format("      $$ = luaL_ref(L, LUA_REGISTRYINDEX);\n") ..
--[[
      /*lua_getfield(L, LUA_GLOBALSINDEX, "dump");
      lua_rawgeti(L, LUA_REGISTRYINDEX, $$);
      e = lua_pcall(L, 1, 0, 0);
      if (e) {
        yyerror(parser, lua_tostring(parser->L, -1));
        lua_pop(L, 1);
      }*/
]]
    format("    }\n") ..
    format("    lua_settop(L, 0);\n")
    
end


function gen_parser_def(name, productions, tokens, map)
  local fd = open(format("%s.y", name), "w")

  local function write(...)
    fd:write(format(...))
  end
  local function strrule(rule)
    local str = ""
    for i,v in ipairs(rule.parts) do
      if v.tag == "non_terminal" then
        str = str .. v.value .. " "
      elseif v.tag == "token_definition" then
        str = str .. v.token.value .. " "
      end
    end
    return str
  end

  write("%s", header)
  write("/* tokens */\n")
  for key, token in pairs(tokens) do
    write("%%token <ref> %s %d\n", key, token.id)
  end

  write("/* non terminals */\n")
  for key, nterm in pairs(map) do
    write("%%type <ref> %s\n", key)
  end

  write("%s", middle)
  write("/* productions */\n")

  for i, production in ipairs(productions) do
    write("%s\n", production.red.value)
    for j, rule in ipairs(production.rules) do
      write("  %s %s\n", j==1 and ":" or "|", strrule(rule))
      write("  {\n")

      local n = #rule.parts

      write("%s\n", genstmpl1(production.red.value, j, n + 1))

      for k, part in ipairs(rule.parts) do
        if part.tag == "non_terminal" then
          write("%s\n", genstmpl2(k, part.value))
        elseif part.tag == "token_definition" then
          write("%s\n", genstmpl2(k, part.token.value))
        end
      end

      write("%s\n", genstmpl3(n))
      write("%s\n", genstmpl4())

      write("  }\n")
    end
    write("  ;\n")
  end
  write("%s", footer)
end

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

function gen_parser_imp(name, productions, tokens, map)
  local fd = open(format("%s.lua", name), "w")

  local function write(...)
    fd:write(format(...))
  end
  local function strrule(rule)
    local str = ""
    for i,v in ipairs(rule.parts) do
      if v.tag == "non_terminal" then
        str = str .. v.value .. " "
      elseif v.tag == "token_definition" then
        str = str .. v.token.value .. " "
      end
    end
    return str
  end
  function text_sub2(rule)
    local ret = "  " .. rule.block
    --dump(rule)
    for i,v in ipairs(rule.parts) do
      ret = gsub(ret, format("$%d", i), format("v%d", i))
    end
    ret = gsub(ret, "\n", "\n  ")
    return ret
  end

  local header = [[
  require('dump')

  local dump = dump
  local dofile = dofile
  local error = error
  local type = type
  local string = string
  local ipairs = ipairs
  local pairs = pairs
  local io = io
  local table = table
  local setmetatable = setmetatable
  local getmetatable = getmetatable
  local assert = assert
  local tostring = tostring
  local tonumber = tonumber
  local unpack = unpack
  local debug = debug

  local write = function(...)
    io.stdout:write(unpack{...}, " ")
  end

  local print = print
  module("parser")

]]

  write("%s\n", header)
  write("dofile('handlers.inc.lua')\n")

  for i, production in ipairs(productions) do
    write("-- %s\n", production.red.value)
    for j, rule in ipairs(production.rules) do
      write("function %s_%d(env, ...)\n", production.red.value, j)

      local n = #rule.parts

      local args, text = (function(rule)
          local args = ""
          local first = true
          for k, v in ipairs(rule.parts) do
            if args ~= "" then
              args = args .. ", "
            end
            args = args .. format("v%d", k)
          end
          return args
        end)(rule)

      write("--   %s %s\n", j==1 and ":" or "|", strrule(rule))
      write("  local %s = ...\n", args)

      if rule.block then
        write("%s\n", text_sub2(rule))
      end

      write("end\n")

     --[[ write("%s\n", genstmpl1(production.red.value, j, n + 1))

      for k, part in ipairs(rule.parts) do
        if part.tag == "non_terminal" then
          write("%s\n", genstmpl2(k, part.value))
        elseif part.tag == "token_definition" then
          write("%s\n", genstmpl2(k, part.token.value))
        end
      end

      write("%s\n", genstmpl3(n))
      write("%s\n", genstmpl4())

      write("  }\n")]]
    end
    write("--   ;\n")
  end

end

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

do
  local productions, tokens, map = parse(input("parser.in"):read("*a"))

  gen_parser_def("parser", productions, tokens, map)
  gen_parser_imp("parser", productions, tokens, map)
end
