require "dump"
require "generator"

local code = io.input(arg[1]):read("*a")

--code = [[abc : A1; ]]
--print(code)

local out = 
--[[
false
--]] true

if arg[2] and arg[2] == "tmpl" then
	out = false
end

local ret = generator.parse(code)

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
if out then
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
      local v%d = %s(tree.%s)]]
  local tmpltfn = [[
      local v%d = tree["%s"].value ]]

  print([[
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

  local write = function(...)
    io.stdout:write(unpack{...}, " ")
  end

  local print = print
  module("parser")

  ]])

  print(io.input("handlers.inc.lua"):read("*a"))
  print("dofile('tmpl.inc.lua')")


  
  for i,v in ipairs(ret) do
    setmetatable(v,tabmet)
    --print(string.format("-- %s", v.res))
    print(string.format(tmpls, v.res))
    for j, rule in ipairs(v.rules) do
      setmetatable(rule,tabmut)

      --print(string.format("--   %s %s",j==1 and ":" or "|", tostring(rule)))
      if j == 1 then
        print(string.format(tmplif, j))
      else
        print(string.format(tmplelif, j))
      end
      
      --local tmpl = template[v.res][j]

      for k, v in ipairs(rule) do
        if v.tag == "iden" then
          print(string.format(tmplcall, k, v.value, v.value))
        elseif v.tag == "token" then
          print(string.format(tmpltfn, k, string.lower(v.value), string.lower(v.value), k))
        end
      end

      --print(string.format(template[v.res][j].text))
      local args = (function(rule)
          local args = ""
          local first = true
          for k, v in ipairs(rule) do
            local p = false
            if v.tag == "iden" then
              p = true
            elseif v.tag == "token" then
              if v.value == "IDENTIFIER" then
                p = true
              elseif v.value == "TYPE_NAME" then
                p = true
              elseif v.value == "STRING_CONSTANT" then
                p = true
              elseif v.value == "CHAR_CONSTANT" then
                p = true
              elseif v.value == "OCT_INT_CONSTANT" then
                p = true
              elseif v.value == "DEC_INT_CONSTANT" then
                p = true
              elseif v.value == "HEX_INT_CONSTANT" then
                p = true
              elseif v.value == "FLOAT_CONSTANT" then
                p = true
              end
            end
            
            if p then
              if not first then
                args = args .. ", "
              end
              first = false
              args = args .. string.format("v%d", k)
            end
          end
          return args
        end)(rule)
      --print(string.format([[      print("   in %s@%d ( %s)")]], v.res, j, tostring(rule)))
      --print(string.format([[      dump({%s})]], args))
        
      print(string.format([[      local ret = %s_%d(%s)]], v.res, j, args))
      --print(string.format([[      print(">>>>>>>>>>>>>>>>>>>>>>")]]))
      --print(string.format([[      dump(ret)]]))
      print(string.format([[      return ret]]))
    end

    print(string.format(tmplfi))

    print("--   ;\n")
    print(string.format(tmple))
  end
end

if not out then
  do
    local fp = io.open("tmpl.inc.lua", "w")
    local write = function(fp, ...) fp:write(string.format(unpack{...}))end

    --[[
    write(fp, "template = {\n")
    for i,v in ipairs(ret) do
      setmetatable(v,tabmet)
      write(fp, "  ['%s'] = {\n", v.res)
      for j, rule in ipairs(v.rules) do
        setmetatable(rule,tabmut)

        write(fp, "    [%d] = { ", j)

        write(fp, "--   %s %s\n",j==1 and ":" or "|", tostring(rule))

        write(fp, "      text =\n"\n",\n", j)

        write(fp, "    },\n")
      end

      write(fp, "--   ;\n")
      write(fp, "  },")
    end
    ]]

    for i,v in ipairs(ret) do
      setmetatable(v,tabmet)
      write(fp, "-- %s\n\n", v.res)
      for j, rule in ipairs(v.rules) do
        setmetatable(rule,tabmut)

        write(fp, "function %s_%d(%s)\n", v.res, j,
          (function(rule)
            local args = ""
            local first = true
            for k, v in ipairs(rule) do
              if v.tag == "iden" then
                if not first then
                  args = args .. ", "
                end
                first = false
                args = args .. v.value
              elseif v.tag == "token" then
                if v.value == "IDENTIFIER" then
                  if not first then
                    args = args .. ", "
                  end
                  first = false
                  args = args .. "identifier"
                elseif v.value == "TYPE_NAME" then
                  if not first then
                    args = args .. ", "
                  end
                  first = false
                  args = args .. "tn"
                elseif
                  v.value == "TYPE_NAME" or
                  v.value == "STRING_CONSTANT" or
                  v.value == "CHAR_CONSTANT" or
                  v.value == "OCT_INT_CONSTANT" or
                  v.value == "DEC_INT_CONSTANT" or
                  v.value == "HEX_INT_CONSTANT" or
                  v.value == "FLOAT_CONSTANT" then
                  if not first then
                    args = args .. ", "
                  end
                  first = false
                  args = args .. "literal"
                end
                --print(string.format(tmpltfn, k, string.lower(v.value), string.lower(v.value), k))
              end
            end
            return args
          end)(rule))
        --write(fp, "  -- %s\n", tostring(rule))
        write(fp,"  print('in %s_%d ( %s)')\n\n", v.res, j, tostring(rule))
        write(fp, "end\n\n")
      end

      write(fp, "--   ;\n\n")
    end
    fp:close()
  end
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

