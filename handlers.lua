
local M = {
  Class = require "class",

  dump = dump,
  ipairs = ipairs,
  pairs = pairs,
  setmetatable = setmetatable,
  type = type,
  concat = table.concat,
  next = next,
}

M.tassert = function(token, cond, message, ...)
  if not cond then
    error({token, message and string.format(message, ...) or "AST/nr"})
  end
  return cond
end

M.tree_get_any_token = function(tree)
  local tok
  for k, v in pairs(tree) do
    if v.tag == "token" then
      tok = v
    elseif type(v) == "table" then
      tok = tree_get_any_token(v)
    end

    if tok then
      return tok
    end
  end
end


local function deepcompare(t1,t2,ignore_mt)
    local ty1 = type(t1)
    local ty2 = type(t2)
    if ty1 ~= ty2 then return false end
    -- non-table types can be directly compared
    if ty1 ~= 'table' and ty2 ~= 'table' then return t1 == t2 end
    -- as well as tables which have the metamethod __eq
    local mt = getmetatable(t1)
    if not ignore_mt and mt and mt.__eq then return t1 == t2 end

    if t1.tag == "token" and t2.tag == "token" then
      return t1.value == t2.value
    end

    for k1,v1 in pairs(t1) do
        local v2 = t2[k1]
        if v2 == nil or not deepcompare(v1,v2) then return false end
    end
    for k2,v2 in pairs(t2) do
        local v1 = t1[k2]
        if v1 == nil or not deepcompare(v1,v2) then return false end
    end
    return true
end

M.deepcompare = deepcompare

local function import(name, env)
  local f,e = loadfile(name)
  if not f then
    error(e, 2)
  end
  setfenv(f, env)
  return f()
end

import("ast-block.lua", M)
import("ast-declaration.lua", M)
import("ast-enum.lua", M)
import("ast-environment.lua", M)
import("ast-expression.lua", M)
import("ast-function.lua", M)
import("ast-initializer.lua", M)
import("ast-namespace.lua", M)
import("ast-source.lua", M)
import("ast-statement.lua", M)
import("ast-struct.lua", M)
import("ast-type.lua", M)

return M

