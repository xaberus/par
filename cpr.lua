
local M = {
  Class = require "class",

  dump = dump,
  ipairs = ipairs,
  pairs = pairs,
  getmetatable = getmetatable,
  setmetatable = setmetatable,
  type = type,
  concat = table.concat,
  next = next,
  remove = table.remove,
  format = string.format,
  -- not needed
  print = print,
  assert = assert,
}

local function idump(self, indent)
  local idnt
  if not indent then
    print("[1;31m┏━━━━━╾───────────────[0;m")
    print(string.format("%s [1;35m%s[0;m:", "[1;31m┃[0;m", self.tag))
    idnt = "[1;31m┠──[0;m"
  else
    idnt = indent
  end
  for k, v in pairs(self) do
    local t = type(v)
    if t == "string" then
      print(string.format("%s %s = '[1;34m%s[0;m'", idnt, tostring(k), v))
    elseif t == "table" then
      if v.tag then
        print(string.format("%s %s = [1;33m%s[0;m", idnt, tostring(k), v.tag))
      elseif k == "loc" then
        print(string.format("%s loc ...", idnt, tostring(k)))
      else
        print(string.format("%s %s:", idnt, tostring(k)))
        idump(v, idnt .. "[1;31m──[0;m")
      end
    else
      print(string.format("%s %s = '[1;32m%s[0;m'", idnt, k, tostring(v)))
    end
  end
  if not indent then
    print("[1;31m┗━━━━━╾───────────────[0;m")
    io.stdout:flush()
  end
end
M.idump = idump

M.tassert = function(token, cond, message, ...)
  if not cond then
    error({token, message and string.format(message, ...) or "AST/nr"})
  end
  return cond
end

M.disown = function(self)
  local m = assert(M[self.tag], "no metatable for " .. self.tag)
  if getmetatable(self) ~= m then
    --print("disown", self.tag)
    m(setmetatable(self, m))
  end
  return self
end

M.explain = function(self)
  local tab = {self.tag}
  for k, v in pairs(self) do
    tab[#tab+1] = "  " .. v.tag
  end
  print(table.concat(tab, "\n"))
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

import("cpr-block.lua", M)
import("cpr-declaration.lua", M)
import("cpr-enum.lua", M)
import("cpr-environment.lua", M)
import("cpr-expression.lua", M)
import("cpr-function.lua", M)
import("cpr-initializer.lua", M)
import("cpr-namespace.lua", M)
import("cpr-source.lua", M)
import("cpr-statement.lua", M)
import("cpr-struct.lua", M)
import("cpr-type.lua", M)

return M

