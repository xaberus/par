require "lpeg"
require "dump"

local code = io.input(arg[1]):read("*a")

--code = [[abc : A1; ]]
--print(code)

local out = true

local ws = lpeg.S(" \n\t")^0
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

local tokens = {}
local idens = {}

local tagmeta = {
  __tostring = function(s,k)
    return s.value
  end
}

local G = lpeg.P{
  exp,
  exp = lpeg.Ct(iden * colon * rules * semicolon),
  rules = lpeg.Ct(rule * ( pipe * rule)^0),
  rule = ws * lpeg.Ct(lpeg.Cg( (token + iden))^1) * ws,
  token = ws * 
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
    * ws,
  iden = ws *
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
    * ws,
}

G=G^1

local ret = {lpeg.match(G, code)}

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

local tmpls = [[
function %s(tree)
]]

local tmplif = [[
  if tree._variant == %d then]]

local tmplelif = [[
  elseif tree._variant == %d then]]

local tmplfi = [[
  else
    error("not reached")
  end
]]

local tmple = [[
end
]]

local tmplcall = [[
    local var%d = %s(tree.%s)]]

local tmpltfn = [[
    write(tree["%s"].value) -- %d]]

print([[
require('dump')

local dump = dump
local dofile = dofile
local error = error
local type = type
local string = string
local ipairs = ipairs
local pairs = pairs

local write = function(...)
  io.stdout:write(unpack{...}, " ")
end

local print = function(...)
  io.stdout:write(string.format(unpack{...}))
end

module(...)

function declaration_handler(tree, variant, typedefs)
  local function decide_typedef(tree)
    local function deep_lookup(tree, key)
      for k,v in pairs(tree) do
        if k == "declaration_specifiers" then
          if deep_lookup(v, key) then return true end
        elseif k == "storage_class_specifier" then
          if deep_lookup(v, key) then return true end
        elseif k == key then 
          --dump(v,nil,nil,true)
          return true
        end
      end
      return false
    end
    local function get_name(tree)
      for k,v in pairs(tree) do
        --print("%s/%s\n", tree.tag, k)
        local r = nil
        if k == "init_declarator_list" then
          r = get_name(v)
        elseif k == "init_declarator" then
          r = get_name(v)
        elseif k == "declarator" then
          r = get_name(v)
        elseif k == "direct_declarator" then
          r = get_name(v)
        elseif k == "identifier" then
          --dump(v,nil,nil,true)
          r = v
        end
        
        if r then return r end
      end
      return nil
    end
    
    if deep_lookup(tree, "typedef") then
      --dump(tree)
      local r = get_name(tree)
      --dump(r)
      return r.value
    end
  end

  if variant == 1 then
    --dump(tree)
  elseif variant == 2 then
    local r = decide_typedef(tree)
    --dump(tree)
    if r then
      typedefs[r] = tree
      --dump(typedefs, 0, 0)
    end
  else
    error("not reached")
  end
end

--dofile("parser.inc.lua")

]])
    
function special_handler(i, v, variant, rule)

end
    
for i,v in ipairs(ret) do
  setmetatable(v,tabmet)
  print(string.format("-- %s", v.res))
  print(string.format(tmpls, v.res))
  for j, rule in ipairs(v.rules) do
    setmetatable(rule,tabmut)

    print(string.format("--   %s %s",j==1 and ":" or "|", tostring(rule)))

    if j == 1 then
      print(string.format(tmplif, j))
    else
      print(string.format(tmplelif, j))
    end

    for k, v in ipairs(rule) do
      if v.tag == "iden" then
        print(string.format(tmplcall, k, v.value, v.value))
      elseif v.tag == "token" then
        print(string.format(tmpltfn, string.lower(v.value), k))
      end
    end
    
    special_handler(i, v, j, rule)
  end

  print(string.format(tmplfi))
  
  print("--   ;\n")
  print(string.format(tmple))
end


--[[
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

    print(string.format(tmpl1, v.res))

    for j,w in ipairs(rule) do
      print(string.format(tmpl2, string.lower(tostring(w)), j, j))
    end

    print(string.format(tmpl3))
    

    print("  }")
  end
  print("  ;\n")
end

]]
