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
local loadstring = loadstring
local table = table
local io = io

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
      / function(...) return {["@tag"] = "ast", ...} end,

  tokens =
    (space * P"tokens" * space * left_brace * token_definitions * right_brace)
      / function(...) 
        local args = {...}
        if type(args[1]) == "string" then return {["@tag"] = "tokens"} end
        return {["@tag"] = "tokens", ...}
      end,

  token_definitions =
    (space * token_definition^0 * space),

  token_definition =
    ((space * terminal * space * left_paren * space * token_args * Cb("kw") * space * right_paren * semicolon * space)
      / function(token, id, rep, kw)
        id = (id ~= "nil") and tonumber(id) or nil
        return {["@tag"] = "token_definition", token = token, id = id, rep = rep, kw = kw == "K" and true or false} 
      end)
    + ((space * terminal * space * left_paren * space * C(token_number) * space * right_paren * (lua_block) * semicolon * space)
      / function(token, id, block)
        id = (id ~= "nil") and tonumber(id) or nil
        return {["@tag"] = "token_definition", token = token, id = id, rep = "block", kw = kw == "K" and true or false, block = block}
      end)
    ,

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
      )
    )^-1,

  token_number =
    ( P"0" + (R"19" * digit^0)),

  token_string =
    quot * C((P(1) - quot)^0) *  quot,

  production =
    (space * non_terminal * space * colon * ruleblock * semicolon * space)
      / function(red, rules) return {["@tag"] = "production", red = red, rules = rules} end,

  ruleblock =
    (space * rule * (pipe * rule)^0 * space)
      / function(...) return {["@tag"] = "ruleblock", ...} end,

  rule =
    (space * Ct(((terminal + non_terminal + (lua_block / function(block) return {["@tag"] = "block", block = block} end)) * space)^0) * space)
    / function(parts) return {["@tag"] = "rule", parts = parts} end,

  terminal =
    C(upper_case * (terminal_any)^0)
      / function(value) return {["@tag"] = "terminal", value = value} end,

  non_terminal =
    C(lower_case * (terminal_any)^0)
      / function(value) return {["@tag"] = "non_terminal", value = value} end,

  lua_block =
    space * ((brace_block) / function(str) return str:sub(2, #str-1) end) * space,

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

  -- check & clean up
  for i, prod in ipairs(productions) do
    for j, rule in ipairs(prod.rules) do
      for k, part in ipairs(rule.parts) do
        if part["@tag"] == "terminal" then
          tokendef = tokensmap[part.value]
          if not tokendef then
            stderr:write(format("no lexical definition for >%s<;\n", part.value))
            fatal = true
          end
          -- make uinque
          assert(tokendef)
          rule.parts[k] = tokendef
        elseif part["@tag"] == "non_terminal" then
          -- make uinque
          assert(map[part.value])
          rule.parts[k] = map[part.value].red
        end
      end
    end
  end
  collectgarbage("collect")

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
  int yylex(YYSTYPE *, YYLTYPE*, parser_t *);
  int yyerror(YYLTYPE *, parser_t *, const char *);
  int yyluabridge(parser_t * parser, YYLTYPE * l, const char * name, int unref, unsigned int argc, ...);
}

%pure_parser

%locations

%token-table
%debug

%error-verbose
%parse-param {parser_t * parser}
%lex-param {parser_t * parser}

]]

local middle =  [[

%token error ERROR

%start source
%%

]]

--[[

%token <ref> error ERROR

%union {
  int ref;
}

%start source
%%

]]

local footer = [[
%%

int yyluabridge(parser_t * parser, YYLTYPE * l, const char * name, int unref, unsigned int argc, ...)
{
  lua_State * L = parser->L;
  int ret = LUA_NOREF;
  va_list ap;

  lua_settop(L, 0); luaL_checkstack(L, argc + 2, "could not grow stack!");

  if (yydebug)
    fprintf(stderr, "in c-handler %s\n", name);

  lua_getfield(L, LUA_ENVIRONINDEX, name); // handler name 
  if (lua_isnil(L, -1)) {
    fprintf(stderr, "%s\n", "no handler");
    return LUA_NOREF;
  }

  lua_rawgeti(L, LUA_REGISTRYINDEX, parser->envref); // env

  lua_newtable(L);

  lua_pushinteger(L, l->first_line);
  lua_setfield(L, -2, "_line");

  lua_pushinteger(L, l->first_column);
  lua_setfield(L, -2, "_ts");

  lua_pushinteger(L, l->last_column);
  lua_setfield(L, -2, "_te");

  va_start(ap, argc);
  for (unsigned int k = 0; k < argc; k++) {
    int ref = va_arg(ap, int);
    const char * an= va_arg(ap, char *);

    if (ref != LUA_NOREF && ref != LUA_REFNIL) {
      lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
      if (lua_istable(L, -1)) {
        if (yydebug > 1) {
          lua_getfield(L, LUA_GLOBALSINDEX, "dump");
          lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
          int e = lua_pcall(L, 1, 0, 0);
          if (e) {
            fprintf(stderr, "%s\n", lua_tostring(parser->L, -1));
            lua_pop(L, 1);
          }
        }
        if (unref)
          luaL_unref(L, LUA_REGISTRYINDEX, ref);
      } else {
        lua_pop(L, 1);
        lua_pushnil(L);
      }
    } else {
      if (yydebug > 1) {
        fprintf(stderr, "nil argument for arg '%s' for handler '%s'\n", an, name);
      }
      lua_pushnil(L);
    }
  }
  va_end(ap);

  if (yydebug > 1) {
    fprintf(stderr, "---------------------\n");
  }
  int e = lua_pcall(L, 2 + argc, 1, 0); // env + args
  if (e) {
    yyerror(l, parser, lua_tostring(parser->L, -1));
    lua_pop(L, 1);
    return LUA_NOREF;
  }

  if (unref) {
    if (lua_isnil(L, -1)) {
      yyerror(l, parser, "parser operation returned nil!");
      return LUA_NOREF;
    }
  }

  ret = luaL_ref(L, LUA_REGISTRYINDEX);
  parser->astref = ret;

  if (yydebug > 1) {
    lua_getfield(L, LUA_GLOBALSINDEX, "dump");
    lua_rawgeti(L, LUA_REGISTRYINDEX, ret);
    int e = lua_pcall(L, 1, 0, 0);
    if (e) {
      yyerror(l, parser, lua_tostring(parser->L, -1));
      lua_pop(L, 1);
      return LUA_NOREF;
    }
  }

  lua_settop(L, 0);

  return ret;
}

int yylex(YYSTYPE * s, YYLTYPE * l, parser_t * parser) {
  int ret = 0;
  lua_State * L = parser->L;

next_token:
  lua_settop(L, 0);
  lua_checkstack(L, 20);

  lua_rawgeti(L, LUA_REGISTRYINDEX, parser->lexref);
  lua_rawgeti(L, LUA_REGISTRYINDEX, parser->ref);

  int e = lua_pcall(L, 1, 1, 0);
  if (e) {
    fprintf(stderr, "%s\n", lua_tostring(parser->L, -1));
    lua_pop(L, 1);
    return LUA_NOREF;
  }

  lua_getfield(L, 1, "id");
  ret = luaL_checkint(L, -1);
  lua_pop(L, 1);

  lua_getfield(L, 1, "_line");
  l->first_line = l->last_line = lua_tonumber(L, -1); lua_pop(L, 1);

  lua_getfield(L, 1, "_ts");
  l->first_column = lua_tonumber(L, -1); lua_pop(L, 1);

  lua_getfield(L, 1, "_te");
  l->last_column = lua_tonumber(L, -1); lua_pop(L, 1);

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
      *s = lua_ref(L, LUA_REGISTRYINDEX);
      parser->tokref = *s;
      if (yydebug) {
        fprintf(stderr, "\n~~~~~~~~~~~~~~~~~~~~~\n");
        lua_getfield(L, LUA_GLOBALSINDEX, "dump");
        lua_rawgeti(L, LUA_REGISTRYINDEX, *s);
        int e = lua_pcall(L, 1, 0, 0);
        if (e) {
          fprintf(stderr, "%s\n", lua_tostring(parser->L, -1));
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

int yyerror(YYLTYPE * l, parser_t * parser, const char * error) {
  //fprintf(stderr, ">>%s<<\n", error); return 1;

  luaL_Buffer b;
  lua_State * L = parser->L;

  lua_checkstack(L, 4);

  luaL_buffinit(parser->L, &b);

  luaL_addstring(&b, "<input>:"); // XXX

  if (l) {
    size_t ts = l->first_column;
    size_t te = l->last_column;
    size_t ls;
    size_t le;
    size_t len;
    const char * src = NULL;

    lua_pushinteger(L, l->first_line);
    luaL_addvalue(&b);

    luaL_addstring(&b, ":");

    luaL_addstring(&b, "error: ");
    luaL_addstring(&b, error);

    lua_rawgeti(L, LUA_REGISTRYINDEX, parser->ref);
    lua_getfield(L, -1, "src");
    lua_remove(L, -2);

    src = lua_tolstring(L, -1, &len);
    lua_pop(L, 1);

    if (te >= ts && te > 0 && ts > 0) {
      luaL_addstring(&b, ":\n");
      luaL_addstring(&b, "   ");

      ts --; te --;
      for (ls = ts; ls > 0; ls--) {
        if (src[ls] == '\n') {
          break;
        }
      }
      if ((src[ls] == '\n'))
        ls++;

      for (le = te; le < len; le++) {
        if (src[le] == '\n') {
          break;
        }
      }

      if (le > len)
        le = te-1;

      if (ts < ls) {
        fprintf(stderr,  "WTH just haappened?\n");
      } else if (le - ls) {
        //fprintf(stderr,  "%.*s\n", (int)(le-ls), src+ls);
        lua_pushlstring(L, src + ls, le-ls);

        luaL_addvalue(&b);
        luaL_addstring(&b, "\n");

        luaL_addstring(&b, "   ");

        for (size_t j = 1; j < ts-ls; j++) {
          luaL_addchar(&b, ' ');
        }
        luaL_addstring(&b, "^");
      }
    }

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
  if (yyparse(parser)) {
    return luaL_error(L, "parser error");
  }
  return 0;
}

static int l_parse (lua_State *L) {
  parser_t parser = {.L = L};

  luaL_checktype(L, 1, LUA_TTABLE);

  lua_getfield(L, 1, "@tag");
  const char * tag = lua_tostring(L, -1);

  if (strncmp(tag, "Parser", strlen(tag)))
    luaL_error(L, "expected Parser as first argument!");

  lua_pushvalue(L, 1);
  parser.ref = lua_ref(L, LUA_REGISTRYINDEX);
  if (parser.ref == LUA_NOREF)
    luaL_error(L, "no parser ref");

  lua_getfield(L, 1, "lex");
  if (!lua_isfunction(L, -1))
    return luaL_error(L, "no parser lexer function");
  parser.lexref = lua_ref(L, LUA_REGISTRYINDEX);

  lua_getfield(L, 1, "env");
  if (!lua_istable(L, -1))
    return luaL_error(L, "no local environment");
  parser.envref = lua_ref(L, LUA_REGISTRYINDEX);

  lua_getfield(L, 1, "errf");
  if (!lua_isfunction(L, -1))
    return luaL_error(L, "no parser error function");

  lua_pushcfunction(L, l_parse2);
  lua_getfield(L, 1, "penv");
  if (!lua_istable(L, -1))
    return luaL_error(L, "no parser environment table");
  lua_setfenv(L, -2);

  lua_pushlightuserdata(L, &parser);

  int e = lua_pcall(L, 1, 1, -3);
  if (e) {
    return lua_error(L);
  }

  lua_settop(L, 0);

  lua_rawgeti(L, LUA_REGISTRYINDEX, parser.ref);

  lua_rawgeti(L, LUA_REGISTRYINDEX, parser.envref);
  lua_getfield(L, -1, "tree");
  lua_remove(L, -2);

  luaL_unref(L, LUA_REGISTRYINDEX, parser.astref);
  luaL_unref(L, LUA_REGISTRYINDEX, parser.ref);
  luaL_unref(L, LUA_REGISTRYINDEX, parser.lexref);
  luaL_unref(L, LUA_REGISTRYINDEX, parser.envref);

  return 2;
}

extern int luaopen_libclex_parse(lua_State * L)
{
  lua_pushcfunction(L, l_parse);
  return 1;
}

]]



function gen_parser_def(name, productions, tokens, map, tokensmap)
  local fd = open(format("%s.y", name), "w")

  local function write(...)
    fd:write(format(...))
  end
  local function strrule(rule)
    local str = ""
    for i,v in ipairs(rule.parts) do
      if v["@tag"] == "non_terminal" then
        str = str .. v.value .. " "
      elseif v["@tag"] == "token_definition" then
        str = str .. v.token.value .. " "
      elseif v["@tag"] == "block" then
        str = str .. "{block} "
      end
    end
    return str
  end

  write("%s", header)
  write("/* tokens */\n")
  for key, token in pairs(tokensmap) do
    write("%%token %s %d\n", key, token.id)
  end

  write("%s", middle)
  write("/* productions */\n")

  local function bridge(lparts, pname, j, k, ur)
    write("      $$ = yyluabridge(parser, &@$, \"%s_%d_%d\", %d, %d, \n", pname, j, k, ur, #lparts)
    for m, part in ipairs(lparts) do
      if part["@tag"] == "non_terminal" then
        write("        $%d, \"%s\",\n", m, part.value)
      elseif part["@tag"] == "token_definition" then
        write("        $%d, \"%s\",\n", m, part.token.value)
      elseif part["@tag"] == "block" then
        write("        $%d, \"{block}\",\n", m)
      end
    end
    write("        NULL, NULL);\n")
    write("      if ($$ == LUA_NOREF) {\n")
    write("        yyerror(&@$, parser, \"handler '%s_%d_%d' returned no reference, stopping!\");\n", pname, j, k)
    write("        YYERROR;\n")
    write("      }\n")
  end

  for i, production in ipairs(productions) do
    write("%s\n", production.red.value)
    for j, rule in ipairs(production.rules) do

      local lparts = {}

      write("  %s", j==1 and ":" or "|")

      for k, part in ipairs(rule.parts) do
        if part["@tag"] == "non_terminal" then
          write(" %s", part.value)
        elseif part["@tag"] == "token_definition" then
          write(" %s", part.token.value)
        elseif part["@tag"] == "block" then
          write("\n    {\n")

          bridge(lparts, production.red.value, j, k, (k == #rule.parts) and 1 or 0)

          if k == #rule.parts then
            write("    }")
          else
            write("    }\n   ")
          end
        end

        table.insert(lparts, part)

        if k == #rule.parts and (part["@tag"] ~= "block") then
          write("\n    {\n")
          bridge(lparts, production.red.value, j, k, 1)
          write("    }\n")
        end
      end
      write("\n")
    end
    write("  ;\n")
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
  local function strrule(parts)
    local str = ""
    for i,v in ipairs(parts) do
      if v["@tag"] == "non_terminal" then
        str = str .. v.value .. " "
      elseif v["@tag"] == "token_definition" then
        str = str .. v.token.value .. " "
      elseif v["@tag"] == "block" then
        str = str .. "{block} "
      end
    end
    return str
  end
  function text_sub2(parts, block)
    local ret = "  " .. block
    for i,v in ipairs(parts) do
      ret = gsub(ret, format("$%d", i), format("v%d", i))
    end
    ret = gsub(ret, "\n", "\n  ")
    return ret
  end

  local header = [[

local function include(tab, element)
  table.insert(tab, element)
  return tab
end

local function merge(tab, atab)
  for i, v in ipairs(atab) do
    table.insert(tab, v)
  end
  return tab
end

local M = {}

]]

  write("%s\n", header)
  --write("%s", input("handlers.inc.lua"):read("*a"))

  for i, production in ipairs(productions) do
    write("-- %s\n", production.red.value)
    for j, rule in ipairs(production.rules) do

      local mparts = {}
      local lparts = {}

      for k, part in ipairs(rule.parts) do
        if part["@tag"] == "block" then
          write("M.%s_%d_%d = function(env, m, ...)\n", production.red.value, j, k)
          write("--   %s %s\n", j==1 and ":" or "|", strrule(lparts))
          if #lparts > 0 then
            write("  local %s = ...\n", table.concat(mparts, ","))
            write("%s\n", text_sub2(lparts, part.block))
          end
          write("end\n")

        end

        table.insert(mparts, format("v%d", k))
        table.insert(lparts, part)

        if k == #rule.parts and part["@tag"] ~= "block" then
          write("M.%s_%d_%d = function(env, m, ...)\n", production.red.value, j, k)
          write("  -- XXX default handler\n")
          io.stderr:write(format("warning: generating default handler for %s_%d_%d\n", production.red.value, j, k))
          write("  return {..., _m = m}\n")
          write("end\n")
        end
      end
    end
    write("--   ;\n")
  end
  write("return M")

end

-----------------------------------------------
---@@-------@@@@@@--@@---@@--@@@@@@--@@@@@@----
---@@-------@@-------@@-@@---@@------@@---@@---
---@@-------@@@@@@----@@@----@@@@@@--@@@@@@----
---@@-------@@-------@@-@@---@@------@@--@@----
---@@@@@@@--@@@@@@--@@---@@--@@@@@@--@@---@@---
-----------------------------------------------

function gen_lexer(name, productions, tokens, map, tokensmap)
  local fd = open(format("%s.lua", name), "w")
  --local fd = stdout

  local function write(...)
    fd:write(format(...))
  end

  local header = [[

local lpeg = require "lpeg"
local P, R, S, C, Cc, Ct, Cg, Cb, Cp, Carg, V =
  lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Ct, lpeg.Cg, lpeg.Cb, lpeg.Cp, lpeg.Carg, lpeg.V

local nl = (Carg(1) * C(P"\n"^1)) / function(rt, nl) rt.line = rt.line + #nl end
local ws = nl + S"\t\v\r "

local remove = table.remove
local format = string.format

]]

  local middle = [[
local iden = ((R"AZ" + R"az" + S"_") *  (R"AZ" + R"az" + S"_" + R"09")^0)-(keywords*(-(R"AZ" + R"az" + S"_" + R"09")))

local macrodef = P{
  V"macrodef",
  macrodef = 
    P"@macro" * ws^0 * iden * ws^0 *
      P"(" * ws^0 * (iden * (ws^0 * P"," * ws^0 * iden)^0)^-1 * ws^0 * P")" * ws^0 *
      P"{" * ((P(1) - (S"{}")) + V"macrodef")^0 * P"}"
}
]]

  local footer = [[

local function tb_pop(tb)
  local tok
  if tb[0] then
    tok = tb[0]
    tb[0] = tok[0]
  else
    tok = {id = 0} -- EOF
  end
  return tok
end

local function tb_push(tb, tok)
  if tb[1] then
    tb[1][0] = tok -- append to tail
  else
    tb[0] = tok -- become head
  end
  tb[1] = tok -- become tail
end


local function lex(prsr)
  if prsr.running then

    local tb = prsr.tb
    local mt = prsr.mt

    if not tb then 
      tb = {}
      prsr.tb = tb
    end
    if not mt then
      mt = {}
      prsr.mt = mt
    end

    if not tb[0] and (prsr.pos < #prsr.src) then
      while true do
        local tok = (lpeg.match(token, prsr.src, prsr.pos, prsr.rt))

        if tok then
          prsr.pos = tok._te
          prsr.last = tok

          tb_push(tb, tok)

          if tok.id == 100 then -- IDENTIFIER
            local macro = mt[token.value]
            if macro then
              tok.macro = macro
              tok.id = 5001 -- MACRO
              break
            end
          end
        else
          -- not tok?
          if prsr.pos < #prsr.src then
            local line = lpeg.match( P"\n"^0 * C((P(1) - P"\n")^0 * ( P"\n" + (-P(1)) )), prsr.src, prsr.pos, prsr.rt)
            error(format("garbage at line %d\n %s", prsr.pos, line))
          end
          break
        end
      end
    end

    local tok = tb_pop(tb)

    if tok.id == 100 then -- IDENTIFIER
      if tok.value == "selftype" then
        tok.id = 200 -- TYPE_NAME
      elseif prsr.env.current:type_get_r(tok.value) then
        tok.id = 200 -- TYPE_NAME
      end
    elseif tok.id == 0 then
      prsr.running = false
    end

    return tok

  else
    error("lexer/parser is not running")
  end
end

return lex

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
    if token.rep ~= "block" then
      write("-- %s\n", token.token.value)
      write("local token_%d = ", key)

      write("P%q", token.rep)

      write(" / function(tok) return {['@tag'] = 'token', id = %d, value = tok} end\n", token.id)

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

  write("%s\n", middle)

  local funfirst = true
  for key, token in ipairs(tokens) do
    if token.rep == "block" then
      write("-- %s\n", token.token.value)
      write("local func_%d = ", key)
      loadstring(token.block)(write, token.id)
      write(" / function(tok) return {['@tag'] = 'token', id = %d, value = tok} end\n", token.id)
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
      (ws^0 * (Carg(1) / (function(rt) return rt.line end)) * (Cp() * (funcs + keywords + ops) * (ws^0 + -P(1)) * Cp()) )
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
