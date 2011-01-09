
require "lpeg"

local lpeg = lpeg
local setmetatable = setmetatable

module(...)

local ws = lpeg.S(" \n\t")
local ud = lpeg.S "_"
local up = lpeg.R("AZ") + ud
local lo = lpeg.R("az") + ud
local digit = lpeg.R("19")
local colon = lpeg.S(":")
local semicolon = lpeg.S(";")
local pipe = lpeg.S("|")

local exp = lpeg.V"exp"
local token = lpeg.V"token"
local iden = lpeg.V"iden"
local rule = lpeg.V"rule"
local rules = lpeg.V"rules"

local gram = lpeg.V"gram"
local comment = lpeg.V"comment"

local tokens = {}
local idens = {}

local tagmeta = {
  __tostring = function(s,k)
    return s.value
  end
}

local cs = lpeg.P"/*"
local ce = lpeg.P"*/"
local ci = lpeg.R"az" + lpeg.R"AZ" +
           lpeg.R"09" + lpeg.S"-_ \n\r\t\v"

local G = lpeg.P{
  gram,

  gram = (exp + comment + ws)^1,
  comment = cs * ( ci^1 - ce )^0 * ce,

  exp = lpeg.Ct(iden * colon * rules * semicolon),
  rules = lpeg.Ct(rule * ( pipe * rule)^0),
  rule = ws^0 * lpeg.Ct(lpeg.Cg( (token + iden))^1) * ws^0,
  token = ws^0 * 
    ((up * (up+digit)^1) / function(v)
        local t = tokens[v]
        if not t then
          local r = {tag = "token", value = v}
          setmetatable(r, tagmeta)
          tokens[v] = r;
          return r
        else
          return t
        end
      end)
    * ws^0,
  iden = ws^0 *
    ((lo * (lo+digit)^1) / function(v)
        local t = idens[v]
        if not t then
          local r = {tag = "iden", value = v}
          setmetatable(r, tagmeta)
          idens[v] = r;
          return r
        else
          return t
        end
      end)
    * ws^0,
}

function parse(src)
  tokens = {}
  idens = {}
  return {lpeg.match(G,  src)}, tokens, idens
end
