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
local assert = assert
local error = error

local dump = dump

module("generator")

local P, R, S, C, Cc, Ct, Cg, Cb, Cp, Carg, V =
  lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Ct, lpeg.Cg, lpeg.Cb, lpeg.Cp, lpeg.Carg, lpeg.V

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
local brace_block = V"brace_block"

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
    (space * terminal * space * left_paren * space * token_args * Cb("kw") * space * right_paren * semicolon * space)
      / function(token, id, rep, kw)
        id = (id ~= "nil") and tonumber(id) or nil
        return {tag = "token_definition", token = token, id = id, rep = rep, kw = kw == "K" and true or false} 
      end,

  token_args = --C((P(1)-P")")^0),
    (
      C(token_nil)
      + C(token_number)
    ) * space * (comma * space * space * 
      Cg(P(true), "kw") * (
        C(token_nil)
        + C(token_eof)
        + C(token_error)
        + ( Cg(P"K"^-1, "kw") * space * token_string)
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
      Ct(((terminal + non_terminal) * space)^0) *
      (lua_block^-1) * space)
    / function(parts, block) return {tag = "rule", parts = parts, block = block} end,

  terminal =
    C(upper_case * (terminal_any)^0)
      / function(value) return {tag = "terminal", value = value} end,

  non_terminal =
    C(lower_case * (terminal_any)^0)
      / function(value) return {tag = "non_terminal", value = value} end,

  lua_block =
    space * ((at * P"L" * brace_block * at) / function(str) return str:sub(4, #str-2) end) * space,

  brace_block = left_brace * ((P(1) - (left_brace + right_brace)) + brace_block)^0 * right_brace,
}


local function mkvaluetab(tab, fun)
  local t = {}
  for  i,v in ipairs(tab) do
    t[fun(v)] = v
  end
  return t
end

function parse(src)
  local ret, es = lpeg.match((grammar * Cp()), src)


  if es<#src then
    stderr:write(src:sub(es, #src))
  end

  assert(ret)

  -- all custom id's must be below that
  local c = 256
  local tokens = ret[1]
  local tokensmap = mkvaluetab(tokens,
    function(v)
      if not v.id then
        v.id = c
        c=c + 1
      end
      return v.token.value
    end)

  local productions = ret[2]
  local map = mkvaluetab(productions,
    function(v)
      return v.red.value
    end)

  local fatal = false

  --dump(map)

  -- check & clean up
  for i, prod in ipairs(productions) do
    for j, rule in ipairs(prod.rules) do
      for k, part in ipairs(rule.parts) do
        if part.tag == "terminal" then
          tokendef = tokensmap[part.value]
          if not tokendef then
            stderr:write(format("no lexical definition for >%s<;\n", part.value))
            fatal = true
          end
          -- make uinque
          assert(tokendef)
          rule.parts[k] = tokendef
        elseif part.tag == "non_terminal" then
          -- make uinque
          assert(map[part.value])
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

  assert(productions)
  assert(tokens)
  assert(map)
  assert(tokensmap)

  return productions, tokens, map, tokensmap
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

%token-table
%debug

%error-verbose
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

int yylex(YYSTYPE * s, parser_t * parser) {
  int ret = 0;
  lua_State * L = parser->L;

next_token:
  lua_settop(L, 0);

  lua_rawgeti(L, LUA_REGISTRYINDEX, parser->lexref);
  lua_rawgeti(L, LUA_REGISTRYINDEX, parser->ref);

  int e = lua_pcall(L, 1, 1, 0);
  if (e) {
    yyerror(parser, lua_tostring(parser->L, -1));
    lua_pop(L, 1);
  }

  lua_getfield(L, 1, "id");
  ret = luaL_checkint(L, -1);
  lua_pop(L, 1);

  switch (ret) {
    case LINE_COMMENT:
    case MULTI_LINE_COMMENT: {
      lua_pop(L, 1);
      goto next_token;
    }
    case EOF: {
    } break;
    case ERROR: {

    } break;
    default: {
      s->ref = lua_ref(L, LUA_REGISTRYINDEX);
      parser->tokref = s->ref;
     if (yydebug) {
        fprintf(stderr, "\n~~~~~~~~~~~~~~~~~~~~~\n");
        lua_getfield(L, LUA_GLOBALSINDEX, "dump");
        lua_rawgeti(L, LUA_REGISTRYINDEX, s->ref);
        int e = lua_pcall(L, 1, 0, 0);
        if (e) {
        yyerror(parser, lua_tostring(parser->L, -1));
          lua_pop(L, 1);
        }
        fprintf(stderr, "~~~~~~~~~~~~~~~~~~~~~\n");
      }
      //fprintf(stderr, "ID: %d\n", ret);
    }
  }

  return ret;
}

#include <string.h>

int yyerror(parser_t * parser, const char * error) {
  //fprintf(stderr, ">>%s<<\n", error); return 1;

  luaL_Buffer b;
  lua_State * L = parser->L;
  
  int top = lua_gettop(L);

  lua_rawgeti(L, LUA_REGISTRYINDEX, parser->tokref);
  if (lua_istable(L, top+1)) {
    lua_getfield(L, top+1, "tag");
    const char * tag = lua_tostring(L, -1);
    if (!tag || strcmp(tag, "token")) {
      lua_pop(L, 2);
      lua_pushnil(L);
    }
  }
  if (lua_isnil(L, top+1) || !lua_istable(L, top+1)) {
    lua_pop(L, 1);
    lua_rawgeti(L, LUA_REGISTRYINDEX, parser->ref);
    lua_getfield(L, -1, "last");
    lua_remove(L, -2);
  }

  luaL_buffinit(parser->L, &b);
  
  luaL_addstring(&b, "<input>:");

  if (!lua_isnil(L, top+1)) {
    size_t line;
    size_t ts;
    size_t te;
    size_t ls;
    size_t le;
    size_t len;
    int id;
    const char * src;

    lua_getfield(L, top+1, "id");
    id = lua_tonumber(L, -1); lua_pop(L, 1);
    //fprintf(stderr, "%d\n", id);

    lua_getfield(L, top+1, "_line");
    line = lua_tonumber(L, -1);
    luaL_addvalue(&b);
    
    luaL_addstring(&b, ":");

    luaL_addstring(&b, "error: ");
    luaL_addstring(&b, error);

    luaL_addstring(&b, ":\n");

    lua_getfield(L, top+1, "_ts");
    ts = lua_tonumber(L, -1); lua_pop(L, 1);
    lua_getfield(L, top+1, "_te");
    te = lua_tonumber(L, -1); lua_pop(L, 1);


    luaL_addstring(&b, "   ");

    lua_rawgeti(L, LUA_REGISTRYINDEX, parser->ref);
    lua_getfield(L, -1, "src");
    lua_remove(L, -2);

    src = lua_tolstring(L, -1, &len);
    lua_pop(L, 1);

    if (src && te - ts) {
      ts --; te --;
      for (ls = ts; ls > 0; ls --) {
        if (src[ls] == '\n') {
          break;
        }
      }
      ls++;

      for (le = te; le < len; le ++) {
        if (src[le] == '\n') {
          break;
        }
      }

      if (le > len)
        le = te-1;

      if (le - ls) {
        //fprintf(stderr,  "%.*s\n", (int)(le-ls), src+ls);
        lua_pushlstring(L, src + ls, le-ls);

        luaL_addvalue(&b);
        luaL_addstring(&b, "\n");

        luaL_addstring(&b, "   ");

        for (size_t j = 1; j < ts-ls; j ++) {
          luaL_addchar(&b, ' ');
        }
        luaL_addstring(&b, "^");
      } else {
        lua_pop(L, 1);
      }
    }

    lua_pop(L, 1);
  } else {
    lua_pop(L, 1);
    luaL_addstring(&b, "error: ");
    luaL_addstring(&b, error);
  }

  luaL_pushresult(&b);
  const char * msg = lua_tostring(L, -1);

  fprintf(stderr, "%s\n", msg);
  lua_pop(L, 1);

  return 1;
}


/* exports node names */
const char **names = (const char **)yytname;
const unsigned char *translate = (const unsigned char *)yytranslate;

static int l_parse2(lua_State *L) {
  parser_t * parser = lua_touserdata(L, 1);
  lua_settop(L, 0);
  yydebug = 0;
  yyparse(parser);
  return 0;
}

static int l_parse (lua_State *L) {
  parser_t parser = {.L = L};

  luaL_checktype(L, 1, LUA_TTABLE);

  lua_getfield(L, 1, "tag");
  const char * tag = luaL_checkstring(L, -1);

  if (strncmp(tag, "Parser", strlen(tag)))
    luaL_error(L, "expected Parser as first argument!");

  lua_pushvalue(L, 1);
  parser.ref = lua_ref(L, LUA_REGISTRYINDEX);

  if (parser.ref == LUA_NOREF)
    luaL_error(L, "no parser ref");

  lua_getfield(L, 1, "lex");
  luaL_checktype(L, -1, LUA_TFUNCTION);
  parser.lexref = lua_ref(L, LUA_REGISTRYINDEX);

  lua_getfield(L, 1, "env");
  luaL_checktype(L, -1, LUA_TTABLE);
  parser.envref = lua_ref(L, LUA_REGISTRYINDEX);


  if (parser.lexref == LUA_NOREF)
    luaL_error(L, "no lex ref");

  int e = lua_cpcall(L, l_parse2, &parser);
  if (e) {
    fprintf(stderr, "%s\n", lua_tostring(L, -1));
  }

  luaL_unref(L, LUA_REGISTRYINDEX, parser.astref);
  luaL_unref(L, LUA_REGISTRYINDEX, parser.ref);
  luaL_unref(L, LUA_REGISTRYINDEX, parser.lexref);
  luaL_unref(L, LUA_REGISTRYINDEX, parser.envref);

  return 0;
}

static const struct luaL_reg clex_table[] = {
  {"parse", l_parse},
  {NULL, NULL},
};


extern int luaopen_libclex_parse(lua_State * L)
{
  int e = luaL_dostring(L, "require 'parser'\n");
  if (e) {
    return luaL_error(L, lua_tostring(L, -1));
  }

  luaL_register(L, "clex.Parser", clex_table);
  return 1;
}

]]


local stmpl1 = [[
    lua_State * L = parser->L;

    lua_settop(L, 0); luaL_checkstack(L, %d, "could not grow stack!");

    if (yydebug)
      fprintf(stderr, "in c-handler %s_%d\n");

    lua_getfield(L, LUA_GLOBALSINDEX, "clex");
    lua_getfield(L, -1, "%s_%d"); // handler name 
    lua_remove(L, -2);

    lua_rawgeti(L, LUA_REGISTRYINDEX, parser->envref); // env
]]
local function genstmpl1(name, variant, n)
  return format(stmpl1, n, name, variant, name, variant)
end

local function genstmpl2(k, name)
  return format(
[[
    /* %s */
    if ($%d != LUA_NOREF) {
      lua_rawgeti(L, LUA_REGISTRYINDEX, $%d);
      if (lua_istable(L, -1)) {
        if (yydebug > 1) {
          lua_getfield(L, LUA_GLOBALSINDEX, "dump");
          lua_rawgeti(L, LUA_REGISTRYINDEX, $%d);
          int e = lua_pcall(L, 1, 0, 0);
          if (e) {
            yyerror(parser, lua_tostring(parser->L, -1));
            lua_pop(L, 1);
          }
        }
        luaL_unref(L, LUA_REGISTRYINDEX, $%d);
      } else {
        lua_pop(L, 1);
        lua_pushnil(L);
      }
    } else {
      lua_pushnil(L);
    }

]], name, k, k, k, k, k)
end

local stmpl3 = [[
    if (yydebug > 1) {
      fprintf(stderr, "---------------------\n");
    }
    int e = lua_pcall(L, 1 + %d, 1, 0); // env + args
    if (e) {
      yyerror(parser, lua_tostring(parser->L, -1));
      lua_pop(L, 1);
      YYERROR;
    }

]]

--local stmpl3 = [[
--    lua_call(L, 1 + %d, 1);
--]]


local function genstmpl3(n)
  return format(stmpl3, n)
end
local function genstmpl4()
  return [[
    if (lua_isnil(L, -1)) {
      yyerror(parser, "parser operation returned nil!");
      YYERROR;
    } else {
      $$ = luaL_ref(L, LUA_REGISTRYINDEX);
      parser->astref = $$;

      if (yydebug > 1) {
        lua_getfield(L, LUA_GLOBALSINDEX, "dump");
        lua_rawgeti(L, LUA_REGISTRYINDEX, $$);
        int e = lua_pcall(L, 1, 0, 0);
        if (e) {
          yyerror(parser, lua_tostring(parser->L, -1));
          lua_pop(L, 1);
          YYERROR;
        }
      }
    }
    lua_settop(L, 0);
]]
end


function gen_parser_def(name, productions, tokens, map, tokensmap)
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
  for key, token in pairs(tokensmap) do
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
    write("  /**/;\n")
  end
  write([[%%destructor { luaL_unref(parser->L, LUA_REGISTRYINDEX, $$); } ]])
  for key, token in pairs(tokensmap) do
    write("%s ", key)
  end
  for key, nterm in pairs(map) do
    write("%s ", key)
  end
  write(";\n")
  write("%s", footer)
end

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

function gen_parser_imp(name, productions, tokens, map, tokensmap)
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
local rawget = rawget
local assert = assert
local tostring = tostring
local tonumber = tonumber
local unpack = unpack
local debug = debug
local input = io.input
local format = string.format

local write = function(...)
  io.stdout:write(unpack{...}, " ")
end

local print = print
module("clex")

]]

  write("%s\n", header)
  write("%s", input("handlers.inc.lua"):read("*a"))

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

      if rule.block then
        if n > 0 then
          write("  local %s = ...\n", args)
        end
        write("%s\n", text_sub2(rule))
      else
        write("  return {...}\n")
      end

      write("end\n")

    end
    write("--   ;\n")
  end

end

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

function gen_lexer(name, productions, tokens, map, tokensmap)
  local fd = open(format("%s.lua", name), "w")
  --local fd = stdout

  local function write(...)
    fd:write(format(...))
  end

  local header = [[
require "dump"
require "lpeg"

local lpeg = lpeg

local stdout = io.stdout
local stderr = io.stderr
local input = io.input
local open = io.open
local format = string.format
local gsub = string.gsub
local tostring = tostring
local pairs = pairs
local ipairs = ipairs
local unpack = unpack
local setmetatable = setmetatable
local tonumber = tonumber
local type = type
local collectgarbage = collectgarbage
local concat = table.concat
local error = error

local dump = dump

module("clex.Parser")

local P, R, S, C, Cc, Ct, Cg, Cb, Cp, Carg, V =
  lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Ct, lpeg.Cg, lpeg.Cb, lpeg.Cp, lpeg.Carg, lpeg.V

local nl = (Carg(1) * C(P"\n"^1)) / function(rt, nl) rt.line = rt.line + #nl end
local ws = nl + S"\t\v\r "

]]

  local footer = [[

function lex(prsr)
  if prsr.running then
    local tok = (lpeg.match(token, prsr.src, prsr.pos, prsr.rt))
    if not tok then
      prsr.running = false

      if prsr.last then
        if prsr.last._te < #prsr.src then
          error(format("garbage at and of file (%d, %d)", prsr.last._te, #prsr.src))
        end
      else
        error(format("garbage at and of file..."))
      end

      return {id = 0} -- EOF
    end
    
    if tok.id == 100 then -- IDENTIFIER
      if prsr.env.types[tok.value] then
        tok.id = 200 -- TYPE_NAME
      end
    end

    prsr.pos = tok._te
    --dump(tok, nil, nil, nil, stderr)
    prsr.last = tok
    return tok

  else
    error("lexer/parser is not running")
  end
end

]]

--[[local parser = {
  src = input("test.in"):read("*a"),
  running = true,
  pos = 1,
  rt = {line = 1},
}

local wl = 1
stderr:write(format("%5d: ", wl))
while parser.running do
  local tok = lex(parser)
  if tok then
    if wl ~= tok.line then
      wl = tok.line
      stderr:write(format("\n%5d: ", wl))
    end
    --stderr:write(format("%s(%s) ", token_names[tok.id], tok.value))
    stderr:write(format("%s ", tok.value))
  end
end
stderr:write(format("\n"))
]]


  local funcs = {
    ["DEC_INT_CONSTANT"] = function(id)
      write([[( P"0" + (R"19" * R"09"^0)  * (S'uUlL'^0)^-1 )]], id)
    end,
    ["HEX_INT_CONSTANT"] = function(id)
      write([[( P"0" * S"xX" * (R"09" + R"af" + R"AF")^1 * (S'uUlL'^0)^-1 )]], id)
    end,
    ["OCT_INT_CONSTANT"] = function(id)
      write([[( P"0" * (R"07")^1  * (S'uUlL'^0)^-1 )]], id)
    end,
    ["FLOAT_CONSTANT"] = function(id)
      write([[ ((
        R"09"^1 * S'eE' * S'+-'^-1 * R"09"^1 * (S'fFlL')^-1
      + R"09"^0 * P"." * R"09"^1 * (S'eE' * S'+-'^-1 * R"09"^1)^-1 * (S'fFlL')^-1
      + R"09"^1 * P"." * R"09"^0 * (S'eE' * S'+-'^-1 * R"09"^1)^-1 * (S'fFlL')^-1
)) ]], id)
    end,
    ["STRING_CONSTANT"] = function(id)
      write([[  (P'L'^-1 * P'"' * (P'\\' * P(1) + (1 - S'\\"'))^0 * P'"')  ]], id)
    end,
    ["CHAR_CONSTANT"] = function(id)
      write([[  (P'L'^-1 * P"'" * (P'\\' * P(1) + (1 - S"\\'"))^1 * P"'")  ]], id)
    end,
    ["TYPE_NAME"] = function(id)
      write([[P(false)]], id)
    end,
    ["MULTI_LINE_COMMENT"] = function(id)
      write([[( P"/*" * ( ((P(1) - P"*/") - nl) + nl)^0 * "*/" ) ]], id)
    end,
    ["LINE_COMMENT"] = function(id)
      write([[( P"//" * ( P(1) - nl )^0  )]], id)
    end,
    ["IDENTIFIER"] = function(id)
      write([[( ((R"AZ" + R"az" + S"_") *  (R"AZ" + R"az" + S"_" + R"09")^0)-(keywords*(-(R"AZ" + R"az" + S"_" + R"09"))) )]], id)
    end,
  }

  write("local token_names = {\n")
  for key, token in ipairs(tokens) do
    write("[%d] = %q,\n", token.id, token.token.value)
  end
  write("}\n")

  write("%s\n", header)

  local kwfirst = true
  local opfirst = true
  for key, token in ipairs(tokens) do
    --dump(token)

    if token.rep ~= "func" then
      write("-- %s\n", token.token.value)
      write("local token_%d = ", key)

      write("P%q", token.rep)

      write(" / function(tok) return {tag = 'token', id = %d, value = tok} end\n", token.id)

      if token.kw then
        if kwfirst then
          write("local keywords = ")
          kwfirst = false
        else
          write("keywords = keywords + ")
        end
        write("token_%d\n", key)
      else
        if opfirst then
          write("local ops = ")
          opfirst = false
        else
          write("ops = ops + ")
        end
        write("token_%d\n", key)
      end
    end
  end
  write("\n\n")

  local funfirst = true
  for key, token in ipairs(tokens) do
    if token.rep == "func" then
      write("-- %s\n", token.token.value)
      write("local func_%d = ", key)
      funcs[token.token.value](token.id)
      write(" / function(tok) return {tag = 'token', id = %d, value = tok} end\n", token.id)
      if funfirst then
        write("local funcs = ")
        funfirst = false
      else
        write("funcs = funcs + ")
      end
      write("func_%d\n", key)
    end
  end

  write([[
token = 
  (
    ((  
      (ws^0 * (Carg(1) / (function(rt) return rt.line end)) * (Cp() * (funcs + keywords + ops) * Cp()) )
        / function(line, ts, tok, te, nl) tok._ts = ts; tok._te = te; tok._line = line return tok, {nl} end
    ))
  )

]])

  write("%s\n", footer)
end

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

do
  local productions, tokens, map, tokensmap = parse(input("parser.in"):read("*a"))

  gen_lexer("lexer", productions, tokens, map, tokensmap)

  gen_parser_def("parser", productions, tokens, map, tokensmap)
  gen_parser_imp("parser", productions, tokens, map, tokensmap)

end
