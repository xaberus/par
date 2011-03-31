
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
local tostring = tostring
local tonumber = tonumber
local unpack = unpack
local debug = debug
local input = io.input
local format = string.format
local next = next

local write = function(...)
  io.stdout:write(unpack{...}, " ")
end

local function tassert(token, cond, message, ...)
  if not cond then
    error({token, message and string.format(message, ...) or "AST/nr"})
  end
  return cond
end

local print = print
module("clex")

local write = function(...)
  io.stdout:write(unpack{...}, " ")
end

local function tree_get_any_token(tree)
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

local function Class(name, meta)
  meta.__index = meta
  meta.tag = name
  
  return setmetatable(
    meta,
    {
      __call = function(self, ...)
        local t = self.constructor(...)
        return setmetatable(t, self)
      end
    }
  )
end

Environment = Class("Environment", {
  constructor = function()
    return {
    }
  end,

  child = function(self, kind)
    local e = Environment()
    e.parent = self
    e.kind = kind or "env"
    table.insert(self, e)
    return e
  end,

  sym_reg = function(self, id, sym)
    if not self.syms then
      self.syms = {}
    else
      tassert(nil, not self.syms[id], "symbol redefined in same scope")
    end

    local s = {sym = sym}
    self.syms[id] = s
    return sym
  end,
  sym_get_r = function(self, id)
    local s = self.syms and self.syms[id] or nil

    if not s and self.parent then
      return self.parent:sym_get_r(id)
    end

    return s
  end,
  sym_get = function(self, id)
    if not self.syms then return nil end
    return self.syms[id]
  end,



  struct_reg = function(self, id, decl)
    if not self.structs then
      self.structs = {}
    else
      tassert(nil, not self.structs[id], "struct redefined in same scope")
    end

    local d = {decl = decl}
    self.structs[id] = d
    return decl
  end,
  struct_get_r = function(self, id)
    local s = self.structs and self.structs[id] or nil

    if not s and self.parent then
      return self.parent:struct_get_r(id)
    end

    return s
  end,
  struct_get = function(self, id)
    if not self.structs then return nil end
    return self.structs[id]
  end,


  enum_reg = function(self, id, decl)
    if not self.enums then
      self.enums = {}
    else
      tassert(nil, not self.enums[id], "enum redefined in same scope")
    end

    local d = {decl = decl}
    self.enums[id] = d
    return decl
  end,
  enum_get_r = function(self, id)
    local e = self.enums and self.enums[id] or nil

    if not e and self.parent then
      return self.parent:enum_get_r(id)
    end

    return e
  end,
  enum_get = function(self, id)
    if not self.enums then return nil end
    return self.enums[id]
  end,


  type_reg = function(self, id, t)
    if not self.types then
      self.types = {}
    else
      tassert(nil, not self.types[id], "type redefined in same scope")
    end
    local t = {t = t}
    self.types[id] = t
    return t
  end,

  type_get_r = function(self, id)
    local t = self.types and self.types[id] or nil

    if not t and self.parent then
      return self.parent:type_get_r(id)
    end

    return t
  end,

  label_reg = function(self, id, t)
    if not self.labels then
      self.labels = {}
    else
      tassert(nil, not self.labels[id], "label redefined in same scope")
    end
    local t = {t = t}
    self.labels[id] = t
    return t
  end,

  label_get = function(self, id)
    if not self.labels then return nil end
    return self.labels[id]
  end,
  label_get_r = function(self, id)
    local t = self.labels and self.labels[id] or nil

    if not t and self.parent then
      local k = self.parent.kind
      if k == "block" or k == "loop" then
        return self.parent:label_get_r(id)
      end
    end

    return t
  end,

})

Translation = Class("Translation", {

})

Declarator = Class("Declarator", {
  constructor = function(env, ctype, tree)
    local self = {}
    if tree.init then
      local init = Initializer(env, tree.init)
      self.init = init
    end
    self.id = tree.iden
    self.ctype = ctype

    env:sym_reg(self.id.value, self)

    return self
  end,
})

Declaration = Class("Declaration", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "decl")

    local self = {}

    if tree.pure then
      self.pure = true
      local ctype = Type(env, tree.ctype)
      for k, v in ipairs(tree.decl) do
        self[#self+1] = Declarator(env, ctype, v)
      end
      self.ctype = ctype
    elseif tree.struct or tree.union then
      env:struct_reg(tree.iden.value, {}) -- dummy
      local s = Struct(env, tree, nil, nil, tree.iden)
      local ref = env:struct_get(tree.iden.value).decl
      for k, v in pairs(s) do
        ref[k] = v
      end
      self.struct = ref
      self.id = tree.iden
      setmetatable(ref, Struct)
    elseif tree.tdef then
      self = tassert(tree.tdef, env:type_get_r(tree.tdef.value),
        "could not find already defined type '%s'", tree.tdef.value)
      self.tdef = true
      self.id = tree.tdef
      self.ctype = Type(env, tree.ctype)
    elseif tree.enum then
      self.enum = Enum(env, tree)
      env:enum_reg(tree.enum.value, self.enum)
    else
      dump(tree, true)
      tassert(nil, false)
    end

    return self
  end,
  repr = function(self, indent)
    if self.pure then -- plain declaration
      local tab = {}
      for k, v in ipairs(self) do
        if v.init then
          tab[#tab+1] = v.id.value .. " = " .. v.init:repr(indent)
        else
          tab[#tab+1] = v.id.value
        end
      end
      if indent then
        return indent .. self.ctype:repr(indent) .. " " .. table.concat(tab, ", ") .. ";\n"
      else
        return self.ctype:repr(indent) .. " "  .. table.concat(tab, ", ") .. ";"
      end
    elseif self.struct then
      return indent .. self.struct:repr(indent) .. ";\n"
    elseif self.tdef then
      return indent .. "typedef " .. self.ctype:repr(indent) .. " " .. self.id.value .. ";\n"
    elseif self.enum then
      return indent .. self.enum:repr(indent) .. ";\n"
    end
  end,
})

local function qd(o)
  for k, v in pairs(o) do
    print(k, v)
  end
end

local function indent(lvl)
  return string.rep ("  ", lvl)
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

Type = Class("Type", {
  set_int_flag = function(self, flag, ...)
    tassert(nil, not self.reg or self.reg == "i")
    if not self.reg then
      self.reg = "i"
      self.int = {}
      self.con = {}
    end

    local function mkadd(int, con, flag)
      if not int[flag] then
        con[flag] = true
      else
        con[flag] = nil
      end

      if false then
      elseif flag == "unsigned" then
        con["signed"] = nil
      elseif flag == "signed" then
        con["unsigned"] = nil
      elseif flag == "char" then
        con["int"] = nil
        con["short"] = nil
        con["long"] = nil
      elseif flag == "short" then
        con["char"] = nil
        con["int"] = nil
        con["long"] = nil
      elseif flag == "long" then
        con["char"] = nil
        con["short"] = nil
      elseif flag == "int" then
        con["char"] = nil
        con["short"] = nil
      end
    end

    if flag == "signed" then
      tassert(nil, not self.int.unsigned, "signed vs. unsigned")
      self.int.signed = true
    elseif flag == "unsigned" then
      tassert(nil, not self.int.signed, "signed vs. unsigned")
      self.int.unsigned = true
    elseif flag == "char" then
      tassert(nil, not self.int.short, "char vs. char")
      tassert(nil, not self.int.short, "char vs. short")
      tassert(nil, not self.int.long, "char vs. long")
      tassert(nil, not self.int.int, "char vs. int")
      self.int.char = true
    elseif flag == "short" then
      tassert(nil, not self.int.char, "short vs. char")
      tassert(nil, not self.int.short, "short vs. short")
      tassert(nil, not self.int.long, "short vs. long")
      self.int.short = true
    elseif flag == "long" then
      tassert(nil, not self.int.char, "long vs. char")
      tassert(nil, not self.int.short, "long vs. short")
      tassert(nil, not self.int.long, "long vs. long")
      self.int.long = true
    elseif flag == "int" then
      tassert(nil, not self.int.char, "int vs. char")
      tassert(nil, not self.int.int, "int vs. int")
      self.int.int = true
    end

    mkadd(self.int, self.con, flag)
    for i, v in ipairs({...}) do
      mkadd(self.int, self.con, v)
    end
    --dump({self.int, self.con, "#####", flag, ...})

  end,

  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "ctype")

    local self = setmetatable({
      qual = {}
    }, Type)

    if tree.functype then
      self.reg = "c"
      self.rctype = Type(env, tree.functype.ret)
      local list = {}
      for k, v in ipairs(tree.functype.list) do
        list[#list+1] = Type(env, v)
      end
      self.list = list
    else
      for v, q in ipairs(tree.sqlist.qual) do
        if q.tag == "token" then
          local v = q.value
          if v == "const" then
            tassert(nil, not self.qual.volatile, "const vs. volatile")
            self.qual.const = true
          elseif v == "volatile" then
            tassert(nil, not self.qual.const, "volatile vs. const")
            self.qual.volatile = true
          elseif v == "restrict" then
            tassert(nil, false, "restrict is only valid for pointers")
          else
            dump(self, true)
            tassert(nil, false)
          end
        else
          dump(self, true)
          tassert(nil, false)
        end
      end

      for v, s in ipairs(tree.sqlist.spec) do
        if s.tag == "token" then
          local v = s.value
          if v == "int" then
            self:set_int_flag("int", "signed")
          elseif v == "char" then
            self:set_int_flag("char", "signed")
          elseif v == "short" then
            self:set_int_flag("short", "signed", "int")
          elseif v == "long" then
            self:set_int_flag("long", "signed", "int")
          elseif v == "signed" then
            self:set_int_flag("signed", "int")
          elseif v == "unsigned" then
            self:set_int_flag("unsigned", "int")
          elseif v == "int8" then
            self.reg = "x"
            self.fixed = s
          elseif v == "int16" then
            self.reg = "x"
            self.fixed = s
          elseif v == "int32" then
            self.reg = "x"
            self.fixed = s
          elseif v == "int64" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint8" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint16" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint32" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint64" then
            self.reg = "x"
            self.fixed = s
          elseif v == "void" then
            self.reg = "x"
            self.fixed = s
          else
            local ref = env:type_get_r(v)
            if ref then
              self.reg = "r"
              self.ref = ref
              self.id = s
            else
              dump(tree, true)
              tassert(nil, false)
            end
          end
        elseif s.struct or s.union then
          tassert(nil, not self.reg, "impossible combination of specifiers")
          local struct = Struct(env, s)
          self.reg = "s"
          self.struct = struct
        else
          dump(tree, true)
          tassert(nil, false)
        end
      end
    end


    if self.reg == "i" then
      local con = self.con
      for k, v in pairs(self.int) do
        con[k] = v
      end
      self.complete = true
    elseif self.reg == "x" then
      self.complete = true
    elseif self.reg == "r" then
      self.complete = true
    elseif self.reg == "s" then
      self.complete = true
    elseif self.reg == "c" then
      self.complete = true
    else
      dump(self, true)
      tassert(nil, false, "type machine: NIY")
    end

    if tree.adecl then
      self.abs = true
      local ad = tree.adecl
      if ad.pointer then
        --dump(ad, true)
        local ptr = {}
        self.pointer = ptr
        for j, p in ipairs(ad.pointer) do
          local quals = {}
          --dump(ad.pointer, true)

          if p.qual then
            for k, q in ipairs(p.qual) do
              local n = q.value
              if n == "const" then
                quals.const = true
              elseif n == "volatile" then
                quals.volatile = true
              elseif n == "restrict" then
                quals.restrict = true
              else
                dump(self, true)
                tassert(nil, false)
              end
            end
          end
          table.insert(ptr, quals)
        end
      end
      if ad.array then
        --dump(ad, true)
        local arr = {}
        self.array = arr
        for j, a in ipairs(ad.array) do
          local quals = {}

          if a.qual then
            for k, q in ipairs(a.qual) do
              local n = q.value
              if n == "const" then
                quals.const = true
              else
                tassert(nil, false,
                  "qualifier '%s' is not valid in this context", n)
              end
            end
          end

          if a.static then
            quals.static = true
          end

          local s = a.expr
          if not s then
            quals.flexible = true
          else
            local expr = Expression(env, s)

            if expr:is_constant() or env.kind == "block" then
              quals.size = expr
            elseif expr:is_symbol() then
              local sym = tassert(nil, expr:symbol_eval(), "not a symbol?")
              quals.size = expr
              quals.ref = sym.ref
              quals.vla = true
            else
              tassert(tree_get_any_token(s), false,
                "invalid expression '%s' in this context", expr:repr(""))
            end
          end
          table.insert(arr, quals)
        end
      end
    end

    return self
  end,
  repr = function(self, indent)
    dump({self.qual, self.int, self.reg}, nil, nil, nil, "Type")
    return "..."
  end,
  -- [[
  repr = function(self, indent)
    local ret = ""
    if self.complete then
      local first = true
      for k, v in pairs(self.qual) do
        if not first then
          ret = ret .. " " .. k
        else
          ret = ret .. k
          first = false
        end
      end
      if not first then
        ret = ret .. " "
      end
      if self.reg == "i" then
        for k, v in pairs(self.int) do
          if not first then
            ret = ret .. " " .. k
          else
            ret = ret .. k
            first = false
          end
        end
      elseif self.reg == "s" then
        ret = ret .. self.struct:repr(indent)
      elseif self.reg == "x" then
        ret = ret .. self.fixed.value
      elseif self.reg == "r" then
        ret = ret .. self.id.value
      elseif self.reg == "c" then
        local tab = {}
        for k, v in ipairs(self.list) do
          tab[#tab+1] = v:repr(indent)
        end
        ret = ret .. "[" .. self.rctype:repr(indent)  .. " : " .. table.concat(tab, ", ") .. "]"
      end

      if self.abs then
        if self.pointer then
          local tab = {}
          for k, v in ipairs(self.pointer) do
            if next(v) then
              local qt = {}
              for q, b in pairs(v) do
                qt[#qt+1] = q
              end
              tab[#tab+1] = "* " .. table.concat(qt, " ")
            else
              tab[#tab+1] = "*"
            end
          end
          ret = ret .. " " .. table.concat(tab, " ")
        end
        if self.array then
          local tab = {}
          for k, v in ipairs(self.array) do
            local prt = {}
            if v.static then
              prt[#prt+1] = "static"
            end
            if v.const then
              prt[#prt+1] = "const"
            end
            if not v.flexible then
              prt[#prt+1] = v.size:repr(indent)
            end
            ret = ret .. "[" .. table.concat(prt, " ") .. "]"
          end
          --dump(self.array, true)
        end
      end

    else
      dump(self, true)
      ret = ret .. "<incomplete type>"
    end

    return ret
  end, --]]
  compare = function(a, b)
    --dump({a,b}, true, nil, nil, "inref")

    if a.reg == "r" then
      a = a.ref.ctype
    end
    if b.reg == "r" then
      b = b.ref.ctype
    end

    --dump({a,b}, true, nil, nil, "outref")
    --dump({a,b}, nil, nil, nil, "outref")

    if a.complete ~= b.complete then return false end
    if a.abs ~= b.abs then return false end

    if not deepcompare(a.qual, b.qual) then return false end

    if a.reg == "i" then
      if not deepcompare(a.con, b.con) then return false end
    end

    if a.abs then
      if not deepcompare(a.pointer, b.pointer) then return false end
      if not deepcompare(a.array, b.array) then return false end
    end

    if a.reg ~= b.reg then return false end
    return true
  end,
})

Parameter = Class("Parameter", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "param")
    local ctype = Type(env, tree.ctype)
    local self = {id = tree.param, ctype = ctype}

    env:sym_reg(self.id.value, self)

    return self
  end,

  repr = function(self, indent)
    return self.ctype:repr(indent) .. " " .. self.id.value
  end,
})

StructDeclarator = Class("StructDeclarator", {
  constructor = function(env, scope, struct, ctype, tree)
    --dump(tree, nil, nil, nil, "structdeclr")

    local self = {}
    if tree.width then
      local expr = Expression(env, tree.width)
      tassert(nil, expr:is_constant())
      self.width = expr
      tassert(nil, ctype.reg == "i")
      tassert(nil, not ctype.abs)
    end
    tassert(nil, scope)
    tassert(nil, not scope[tree.iden.value], "field %s already defined in this scope", tree.iden.value)

    self.id = tree.iden
    scope[self.id.value] = {self, struct}
    self.ctype = ctype

    return self
  end,
})

StructDeclaration = Class("StructDeclaration", {
  constructor = function(env, scope, struct, tree)
    --dump(tree, nil, nil, nil, "structdecl")

    local self = {}

    if tree.decl then
      local ctype = Type(env, tree.ctype)
      for k, v in ipairs(tree.decl) do
        self[#self+1] = StructDeclarator(env, scope, struct, ctype, v)
      end
      self.ctype = ctype
    elseif tree.anon then
      local chld = tassert(nil, tree.ctype.sqlist.spec[1])
      tassert(nil, chld.struct or chld.union)

      return {anon = Struct(env, chld, nil, tassert(nil, scope))}
    else
      dump(tree, true)
      tassert(nil, false)
    end

    return self
  end,
  repr = function(self, indent)
    if not self.anon then 
      local tab = {}
      for k, v in ipairs(self) do
        if v.width then
          tab[#tab+1] = v.id.value .. " : " .. v.width:repr(indent)
        else
          tab[#tab+1] = v.id.value
        end
      end

      if indent then
        return indent .. self.ctype:repr(indent) .. " " .. table.concat(tab, ", ") .. ";\n"
      else
        return self.ctype:repr(indent) .. " " .. table.concat(tab, ", ") .. ";"
      end
    else
      return indent .. self.anon:repr(indent) .. ";\n"
    end
  end,
})

Struct = Class("Struct", {
  constructor = function(env, tree, name, scope, id)
    --dump(tree, nil, nil, nil, "struct")
    if type(tree.struct) == "table" then
      local name = tree.struct.value
      local ref = tassert(tree.struct, env:struct_get_r(name),
        "undefined struct '%s'", name)
      return {ref = ref}
    else
      local self = {
        scope = scope or {[0] = 1},
      }

      if tree.union then
        self.union = true
      end

      if id then
        name = id.value
        self.id = id
      end

      if name then
        self.name = name
      else
        self.name = "anonymous" .. self.scope[0]
        self.scope[0] = self.scope[0] + 1
      end

      for k, v in ipairs(tree.decl) do
        self[#self+1] = StructDeclaration(env, self.scope, self, v)
      end

      --dump(self, true, nil, nil, "Struct")
      return self
    end
  end,
  repr = function(self, indent)
    local tab = {}

    if self.ref then
      local ref = self.ref.decl

      if ref.union then
        tab[#tab+1] = "union"
      else
        tab[#tab+1] = "struct"
      end

      tab[#tab+1] = " "
      tab[#tab+1] = ref.id.value
    else
      if self.union then
        tab[#tab+1] = "union"
      else
        tab[#tab+1] = "struct"
      end

      tab[#tab+1] = " "
      if self.id then
        tab[#tab+1] = self.id.value
        tab[#tab+1] = " "
      end

      tab[#tab+1] = "{\n"

      do
        local indent1 = indent .. "  "
        for k,v in ipairs(self) do
          tab[#tab+1] = v:repr(indent1)
        end
      end

      tab[#tab+1] = indent
      tab[#tab+1] = "}"

    end
    return table.concat(tab, "")
  end,

  Anonymous = function(kind, decl)
    return setmetatable({
       anonymous = true,
       kind = kind,
       decl = decl,
    }, Struct)
  end,
  Named = function(id, kind, decl)
    return setmetatable({
      id = id,
      kind = kind,
      decl = decl,
    }, Struct)
  end,
  MakeHeadless = function(ctype)
    if ctype.spec[1].specifier.tag == "Struct" then
      return setmetatable({
        headless = ctype.spec[1].specifier
      }, Struct)
    else
      error("anonymous member is not a struct?")
    end
  end,
})

Enumerator = Class("Enum", {
  constructor = function(env, enum, tree)
    --dump(tree, nil, nil, nil, "enumerator")
    local self = {}
    self.enum = enum
    self.id = tree.iden
    if tree.value then
      self.value = Expression(env, tree.value)
    end
    env:sym_reg(self.id.value, self)
    return self
  end,
  repr = function(self, indent)
    if self.value then
      return self.id.value .. " = " .. self.value:repr(indent)
    else
      return self.id.value
    end
  end,
})

Enum = Class("Enum", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "enum")
    local self = {}
    for k ,v in ipairs(tree.list) do
      self[#self+1] = Enumerator(env, self, v)
    end
    self.id = tassert(nil, tree.enum, "no id")
    return self
  end,
  repr = function(self, indent)
    local tab = {}
    local indent2 = indent .. "  "
    for k, v in ipairs(self) do
      tab[#tab+1] = indent2 .. v:repr(indent)
    end
    return "enum " .. self.id.value .. " {\n" ..table.concat(tab, ",\n") .. "\n" .. indent .. "}"
  end,
})

Designation = Class("Designation", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "init")
    local self = {}
    for k, v in ipairs(tree) do
      if v.iden then
        self[#self+1] = {name = v.iden}
      elseif v.index then
        self[#self+1] = {name = Expression(env, index)}
      end
    end
    return self
  end,
  repr = function(self, indent)
    local tab = {}
    for k, v in ipairs(self) do
      if v.name then
        tab[#tab+1] = "." .. v.name.value
      elseif b.index then
        tab[#tab+1] = "[" .. v.index:repr(indent) .. "]"
      end
    end
    return table.concat(tab, " = ")
  end,
})

Initializer = Class("Initializer", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "init")
    if tree.asgnexpr then
      local expr = Expression(env, tree.asgnexpr)
      return {expr = expr}
    elseif tree.initlist then
      local list = {}
      for k, v in ipairs(tree.initlist) do
        local init = {}
        init.expr = Expression(env, v.asgnexpr)
        if v.desgn then
          init.desgn = Designation(env, v.desgn)
        end
        list[#list+1] = init
      end
      return {list = list}
    end
    dump(tree)
    tassert(nil, false)
  end,
  repr = function(self, indent)
    if self.expr then
      return self.expr:repr(indent)
    elseif self.list then
      local tab = {}
      local indent2 = indent .. "  "
      tab[#tab+1] = "{\n"
      for k,v in ipairs(self.list) do
        tab[#tab+1] = indent2
        if v.desgn then
          tab[#tab+1] = v.desgn:repr(indent2)
          tab[#tab+1] = " = "
        end
        tab[#tab+1] = v.expr:repr(indent2)
        tab[#tab+1] = ",\n"
      end
      tab[#tab+1] = indent
      tab[#tab+1] = "}"
      return table.concat(tab, "")
    else
      dump(self, true)
      tassert(nil, false)
    end
  end,
})

Block = Class("Block", {
  add_decl = function(self, decl)
    self[#self+1] = decl
  end,
  add_stmt = function(self, decl)
    self[#self+1] = decl
  end,
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "block")
    local self = setmetatable({env = env}, Block)

    local function tree_get_labels(env, tree)
      for k, v in ipairs(tree) do
        if v.stmt then
          local stmt = v.stmt
          if stmt.label then
            local lbl = env:label_get_r(stmt.label.value)
            if lbl then
              --dump(lbl)
              tassert(stmt.label, not env:label_get(stmt.label.value))
              lbl.env = env
            else
              --dump(stmt)
              lbl = {}
              lbl.kind = "label"
              lbl.id = stmt.label
              lbl.env = env
              env:label_reg(lbl.id.value, lbl)
            end
          elseif v.stmt.compound then
            tree_get_labels(env, stmt.compound)
          end
        end
      end
    end

    -- register labels in advance
    tree_get_labels(env, tree)
    --dump(env)

    for k, v in ipairs(tree) do
      if v.decl then
        self:add_decl(Declaration(env, v.decl))
      elseif v.stmt then
        self:add_stmt(Statement(env, v.stmt))
      else
        dump(v, true)
        tassert(nil, false)
      end
    end

    return self
  end,
  repr = function(self, indent)
    local tab = {}
    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent .. "  ")
    end
    return "{\n" .. table.concat(tab, "") .. indent .. "}"
  end,
})

Function = Class("Function", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "funcdef")

    local rctype = Type(env, tree.rctype)

    local penv = env:child("param")

    local params = {}
    for k, v in ipairs(tree.list) do
      local param = Parameter(penv, v)
      params[#params+1] = param
    end

    local function tree_check_fn_proto(fna, fnb)
      if fna.id.value ~= fnb.id.value then return false end
      if not Type.compare(fna.rctype, fnb.rctype) then return false end
      if #fna.params ~= #fnb.params then return false end
      params = fna.params
      for k, v in ipairs(params) do
        local a = fna.params[k]
        if not a then return false end
        local b = fnb.params[k]
        if not b then return false end
        if not Type.compare(a.ctype, b.ctype) then return false end
      end
      if fna.id.rctype ~= fnb.id.rctype then return false end

      return true
    end

    local self

    if tree.forward then
      self = {rctype = rctype, id = tree.funcdef, params = params, forward = true}
      env:sym_reg(tree.funcdef.value, self)
    else
      local benv = penv:child("block")
      local ref = env:sym_get(tree.funcdef.value)
      if ref then
        tassert(tree.funcdef, ref.sym.forward, "attempt to redefine function '%s'", tree.funcdef.value)
        self = {rctype = rctype, id = tree.funcdef, params = params}

        tassert(tree.funcdef, tree_check_fn_proto(ref.sym, self), "forward declaration of wrong type")

        ref.ref = self
      else
        self = {rctype = rctype, id = tree.funcdef, params = params}
        env:sym_reg(self.id.value, self)
      end
      local block = Block(benv, tree.block)
      self.block = block
    end

    if tree.list.vararg then
      self.vararg = true
    end

    return self
  end,
  repr = function(self, indent)
    local spec = {}
    local prt = {}
    local tab = {}

    tab[#tab+1] = indent

    if self.spec then
      for k, v in ipairs(self.spec) do
        --spec[#spec+1] = v
      end
      tab[#tab+1] = table.concat(spec, " ")
      tab[#tab+1] = " "
    end

    tab[#tab+1] = self.rctype:repr(indent)
    tab[#tab+1] = " "

    tab[#tab+1] = self.id.value

    for k, v in ipairs(self.params) do
      prt[#prt+1] = v:repr(indent)
    end

    if self.vararg then
      prt[#prt+1] = "..."
    end

    tab[#tab+1] = "(" .. table.concat(prt, ", ") .. ")"


    if self.block and #self.block then
      tab[#tab+1] = "\n"
      tab[#tab+1] = indent
      tab[#tab+1] = self.block:repr(indent)
      tab[#tab+1] = "\n"
      return table.concat(tab, "")
    elseif self.forward then
      tab[#tab+1] = ";"
      return table.concat(tab, "")
    else
      dump(self, true)
      tassert(nil, false)
    end
  end,
})

Namespace = Class("Namespace", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "stmt")
    local env = env:child("ns")
    local self = {}

    self.env = env

    self.id = tree.ns
    self.src = Source(env, tree.list)

    return self
  end,
  repr = function(self, indent)
    return indent .. "namespace " .. self.id.value .. " {\n" .. self.src:repr(indent .. "  ") .. indent .. "};\n"
  end,
})


Statement = Class("Statement", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "stmt")
    local self = {}
    if tree.ifcl then
      self.expr = Expression(env, tree.ifcl)
      self.dstmt = Statement(env, tree.dstmt);
      self.opened = tree.opened
      if tree.estmt then
        self.kind = "ifelse"
        self.estmt = Statement(env, tree.estmt);
      else
        self.kind = "if"
      end
    elseif tree.docl then
      local env = env:child("loop")
      self.kind = "do"
      self.expr = Expression(env, tree.docl)
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.forcl then
      local env = env:child("loop")
      local m = tree.forcl
      self.kind = "for"
      if m == "ee" then
        self.start = Expression(env, tree.init)
        self.cond = Expression(env, tree.cond)
      elseif m == "eee" then
        if(tree.init) then
          self.start = Expression(env, tree.init)
        end
        self.cond = Expression(env, tree.cond)
        self.run = Expression(env, tree.incr)
      elseif m == "de" then
        self.decl = Declaration(env, tree.decl)
        self.cond = Expression(env, tree.cond)
      elseif m == "dee" then
        self.decl = Declaration(env, tree.decl)
        self.cond = Expression(env, tree.cond)
        self.run = Expression(env, tree.incr)
      end
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.whilecl then
      local env = env:child("loop")
      self.kind = "while"
      self.expr = Expression(env, tree.whilecl)
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.switchcl then
      local env = env:child("switch")
      self.kind = "switch"
      self.expr = Expression(env, tree.switchcl)
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.default then
      tassert(nil, 
        env.kind == "switch"
        or env.parent.kind == "switch")
      self.kind = "default"
    elseif tree.case then
      tassert(nil, 
        env.kind == "switch"
        or env.parent.kind == "switch")
      self.kind = "case"
      self.expr = Expression(env, tree.case)
    elseif tree.expr then
      self.kind = "expr"
      self.expr = Expression(env, tree.expr)
    elseif tree.empty then
      self.kind = "empty"
    elseif tree.compound then
      self.kind = "block"
      local env = env:child("block")
      self.block = Block(env, tree.compound)
    elseif tree.retstmt then
      self.kind = "return"
      if type(tree.retstmt) == "table" then
        self.expr = Expression(env, tree.retstmt)
      end
    elseif tree.breakstmt then
      local be = env
      while be do
        local k = be.kind
        if k == "switch" or k == "loop" then
          break
        else
          be = be.parent
        end
      end
      self.kind = "break"
      self.env = tassert(tree.breakstmt, be, "no scope to break out to")
    elseif tree.contstmt then
      local le = env
      while le do
        if le.kind == "loop" then
          break
        else
          le = le.parent
        end
      end
      tassert(tree.contstmt, le, "continue not in a loop scope")
      self.kind = "cont"
      self.env = le
    elseif tree.label then
      self.kind = "label"
      self.ref = env:label_get_r(tree.label.value)
      self.id = tree.label
    elseif tree.gotostmt then
      self.kind = "goto"
      self.id = tree.gotostmt
      self.ref = tassert(nil, env:label_get_r(self.id.value), 
        "no label %s in this scope", self.id.value)
    else
      dump(tree, nil, nil, nil, "stmt")
      tassert(nil, false)
    end
    return self
  end,
  repr = function(self, indent)
    local kind = self.kind
    local function dostmt(tab, dstmt, indent)
      local k = dstmt.kind
      if k ~= "block" and k~= "empty" then
        tab[#tab+1] = "\n"
        tab[#tab+1] = dstmt:repr(indent .. "  ")
        return false
      else
        if k == "empty" then
          tab[#tab+1] = ";\n"
          return false
        elseif k == "block" then
          tab[#tab+1] = " "
          tab[#tab+1] = dstmt.block:repr(indent)
          tab[#tab+1] = "\n"
          return true
        end
      end
    end
    local tab = {}
    --dump(self, true)
    if kind == "if" then
      tab[#tab+1] = indent
      tab[#tab+1] = "if"
      tab[#tab+1] = " ("
      tab[#tab+1] = self.expr:repr(indent)
      tab[#tab+1] = ")"
      dostmt(tab, self.dstmt, indent)

    elseif kind == "ifelse" then
      local cl = self
      local k
      while cl do
        if not k then
          tab[#tab+1] = indent
        else
          tab[#tab+1] = " "
        end
        tab[#tab+1] = "if"
        tab[#tab+1] = " ("
        tab[#tab+1] = cl.expr:repr(indent)
        tab[#tab+1] = ")"
        if dostmt(tab, cl.dstmt, indent) then
          tab[#tab] = " "
        else
          tab[#tab+1] = indent
        end
        tab[#tab+1] = "else"
        k = cl.estmt.kind
        if k == "if" then
          local ie = cl.estmt
          tab[#tab+1] = " "
          tab[#tab+1] = "if"
          tab[#tab+1] = " ("
          tab[#tab+1] = ie.expr:repr(indent)
          tab[#tab+1] = ")"
          dostmt(tab, ie.dstmt, indent)
          break;
        elseif k == "ifelse" then
          cl = cl.estmt
        else
          dostmt(tab, cl.estmt, indent)
          break;
        end
      end

    elseif kind == "expr" then
      tab[#tab+1] = indent
      tab[#tab+1] = self.expr:repr(indent)
      tab[#tab+1] = ";\n"
    elseif kind == "do" then
      tab[#tab+1] = indent
      tab[#tab+1] = "do"
      dostmt(tab, self.dstmt, indent)
      tab[#tab] = " " -- replace newline
      tab[#tab+1] = "while"
      tab[#tab+1] = " ("
      tab[#tab+1] = self.expr:repr(nil)
      tab[#tab+1] = ")"
      tab[#tab+1] = ";\n"
    elseif kind == "for" then
      tab[#tab+1] = indent
      tab[#tab+1] = "for"
      tab[#tab+1] = " ("
      if self.decl then
        tab[#tab+1] = self.decl:repr(nil)
      else
        if self.start then
          tab[#tab+1] = self.start:repr(nil)
        end
        tab[#tab+1] = ";"
      end
      tab[#tab+1] = " "
      tab[#tab+1] = self.cond:repr(nil)
      tab[#tab+1] = ";"
      if self.run then
        tab[#tab+1] = " "
        tab[#tab+1] = self.run:repr(nil)
      end
      tab[#tab+1] = ")"
      dostmt(tab, self.dstmt, indent)
    elseif kind == "while" then
      tab[#tab+1] = indent
      tab[#tab+1] = "while"
      tab[#tab+1] = " ("
      tab[#tab+1] = self.expr:repr(nil)
      tab[#tab+1] = ")"
      dostmt(tab, self.dstmt, indent)
    elseif kind == "block" then
      tab[#tab+1] = indent .. self.block:repr(indent) .. "\n"
    elseif kind == "return" then
      tab[#tab+1] = indent
      tab[#tab+1] = "return"
      if self.expr then
        tab[#tab+1] = " "
        tab[#tab+1] = self.expr:repr(indent)
      end
      tab[#tab+1] = ";\n"
    elseif kind == "break" then
      tab[#tab+1] = indent
      tab[#tab+1] = "break"
      tab[#tab+1] = ";\n"
    elseif kind == "cont" then
      tab[#tab+1] = indent
      tab[#tab+1] = "continue"
      tab[#tab+1] = ";\n"
    elseif kind == "label" then
      tab[#tab+1] = self.id.value
      tab[#tab+1] = ":\n"
    elseif kind == "goto" then
      tab[#tab+1] = indent
      tab[#tab+1] = "goto"
      tab[#tab+1] = " "
      tab[#tab+1] = self.id.value
      tab[#tab+1] = ";\n"
    elseif kind == "switch" then
      tab[#tab+1] = indent
      tab[#tab+1] = "switch"
      tab[#tab+1] = "("
      tab[#tab+1] = self.expr:repr(nil)
      tab[#tab+1] = ") "
      tab[#tab+1] = "{\n"
      --dostmt(tab, self.dstmt, indent)
      for k, v in ipairs(self.dstmt.block) do
        if v.kind == "case" or v.kind == "default" then
          tab[#tab+1] = v:repr(indent .. "  ")
        else
          tab[#tab+1] = v:repr(indent .. "    ")
        end
      end
      tab[#tab+1] = indent
      tab[#tab+1] = "}\n"
    elseif kind == "default" then
      tab[#tab+1] = indent
      tab[#tab+1] = "default"
      tab[#tab+1] = ":\n"
    elseif kind == "case" then
      tab[#tab+1] = indent
      tab[#tab+1] = "case"
      tab[#tab+1] = " "
      tab[#tab+1] = self.expr:repr(indent)
      tab[#tab+1] = ":\n"
    end
    return table.concat(tab, "")
  end,

  Jump = function(jump)
    return setmetatable({
      kind = "jump",
      jump = jump,
    }, Statement)
  end,
  Expression = function(expr)
    return setmetatable({
      kind = "expression",
      expr = expr,
    }, Statement)
  end,
  If = function(opened, clause, dostmt)
    return setmetatable({
      kind = "if",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
    }, Statement)
  end,
  IfElse = function(opened, clause, dostmt, elsestmt)
    return setmetatable({
      kind = "elseif",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
      elsestmt = elsestmt,
    }, Statement)
  end,
  Compound = function(compound)
    return setmetatable({
      kind = "compound",
      compound = compound,
    }, Statement)
  end,
  For = function(opened, clause, dostmt)
    return setmetatable({
      kind = "for",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
    }, Statement)
  end,
  While = function(opened, clause, dostmt)
    return setmetatable({
      kind = "while",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
    }, Statement)
  end,
  Label = function(label)
    return setmetatable({
      kind = "label",
      label = label,
    }, Statement)
  end,
  Switch = function(switch)
    return setmetatable({
      kind = "switch",
      switch = switch,
    }, Statement)
  end,

})

Expression = Class("Expression", {
  constructor = function(env, tree)
    --dump(tree)
    local E = Expression
    if tree.dot then
      return setmetatable({op = 'acc_deref', a = Expression(env, tree.dot), b = tree.iden}, Expression)
    elseif tree.iden then
      local self = setmetatable({identifier = tree.iden}, Expression)
      local ref = tassert(self.identifier, env:sym_get_r(self.identifier.value),
        "no symbol '" .. self.identifier.value .."' in same scope")
      self.ref = ref
      return self
    elseif tree.const then
      local c = tree.const
      if c.oct then
        return setmetatable({constant = c.oct, kind = "oct"}, Expression)
      elseif c.dec then
        return setmetatable({constant = c.dec, kind = "dec"}, Expression)
      elseif c.hex then
        return setmetatable({constant = c.hex, kind = "hex"}, Expression)
      elseif c.char then
        return setmetatable({constant = c.char, kind = "char"}, Expression)
      elseif c.string then
        return setmetatable({constant = c.string, kind = "string"}, Expression)
      end
    elseif tree.list then
      local list = {}
      for k,v in ipairs(tree.list) do
        list[#list+1] = Expression(env, v)
      end
      tassert(nil, #list > 0, "empty expression list")
      return setmetatable({list = list}, Expression)
   -------- binary op ---------
    elseif tree.shr then
      return E.shr(env, tree.shr, tree.expr)
    elseif tree.shl then
      return E.shl(env, tree.shl, tree.expr)
    elseif tree.bnot then
      return E.bnot(env, tree.bnot, tree.expr)
    elseif tree.band then
      return E.band(env, tree.band, tree.expr)
    elseif tree.bor then
      return E.bor(env, tree.bor, tree.expr)
    elseif tree.bxor then
      return E.bxor(env, tree.bxor, tree.expr)

    elseif tree.shr_asgn then
      return E.shr_asgn(env, tree.shr_asgn, tree.expr)
    elseif tree.shl_asgn then
      return E.shl_asgn(env, tree.shl_asgn, tree.expr)
    elseif tree.band_asgn then
      return E.band_asgn(env, tree.band_asgn, tree.expr)
    elseif tree.bor_asgn then
      return E.bor_asgn(env, tree.bor_asgn, tree.expr)
    elseif tree.bxor_asgn then
      return E.bxor_asgn(env, tree.bxor_asgn, tree.expr)

  -------- logical op ---------
    elseif tree.land then
      return E.land(env, tree.land, tree.expr)
    elseif tree.lnot then
      return E.lnot(env, tree.lnot, tree.expr)
    elseif tree.lor then
      return E.lor(env, tree.lor, tree.expr)

  -------- comparison op ---------
    elseif tree.ne then
      return E.ne(env, tree.ne, tree.expr)
    elseif tree.ls then
      return E.ls(env, tree.ls, tree.expr)
    elseif tree.gt then
      return E.gt(env, tree.gt, tree.expr)
    elseif tree.eq then
      return E.eq(env, tree.eq, tree.expr)
    elseif tree.le then
      return E.le(env, tree.le, tree.expr)
    elseif tree.ge then
      return E.ge(env, tree.ge, tree.expr)

  -------- unary op ---------
    elseif tree.unp then
      return E.unp(env, tree.unp, tree.expr)
    elseif tree.unm then
      return E.unm(env, tree.unm, tree.expr)
    elseif tree.udec then
      return E.udec(env, tree.udec, tree.expr)
    elseif tree.uinc then
      return E.uinc(env, tree.uinc, tree.expr)

  -------- math op ---------
    elseif tree.add then
      return E.add(env, tree.add, tree.expr)
    elseif tree.sub then
      return E.sub(env, tree.sub, tree.expr)
    elseif tree.mul then
      return E.mul(env, tree.mul, tree.expr)
    elseif tree.div then
      return E.div(env, tree.div, tree.expr)
    elseif tree.mod then
      return E.mod(env, tree.mod, tree.expr)

    elseif tree.add_asgn then
      return E.add_asgn(env, tree.add_asgn, tree.expr)
    elseif tree.sub_asgn then
      return E.sub_asgn(env, tree.sub_asgn, tree.expr)
    elseif tree.mul_asgn then
      return E.mul_asgn(env, tree.mul_asgn, tree.expr)
    elseif tree.div_asgn then
      return E.div_asgn(env, tree.div_asgn, tree.expr)
    elseif tree.mod_asgn then
      return E.mod_asgn(env, tree.mod_asgn, tree.expr)

    elseif tree.inc then
      return E.inc(env, tree.inc, tree.expr)
    elseif tree.dec then
      return E.dec(env, tree.dec, tree.expr)

  -------- c op ---------
    elseif tree.asgn then
      return E.asgn(env, tree.asgn, tree.expr)

    elseif tree.deref then
      return E.deref(env, tree.deref, tree.expr)
    elseif tree.cast then
      return E.cast(env, tree.cast, tree.ctype)

    elseif tree.sizeof then
      return setmetatable({op = 'sizeof', a = Expression(env, tree.sizeof)}, Expression)
    elseif tree.fsizeof then
      return setmetatable({op = 'fsizeof', a = Type(env, tree.fsizeof)}, Expression)

    elseif tree.cond then
      return E.cond(env, tree.cond, tree.texpr, tree.fexpr)
    elseif tree.acc_ptrderef then
      return E.acc_ptrderef(env, tree.acc_ptrderef, tree.expr)

    elseif tree.addr then
      return E.addr(env, tree.addr, tree.expr)

    elseif tree.par then
      return E.par(env, tree.par, tree.expr)

    elseif tree.call then
      return E.call(env, tree.call, tree.args)

    elseif tree.index then
      return E.index(env, tree.index, tree.expr)

    --------------------------------------
    end
    dump(tree, true)
    for k,v in pairs(tree) do print(k,v) end
    tassert(nil, false)
  end,


  -------- binary op ---------
  shr = function(env, a, b)
    return setmetatable({op = 'shr', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  shl = function(env, a, b)
    return setmetatable({op = 'shl', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  bnot = function(env, a)
    return setmetatable({op = 'bnot', a = Expression(env, a)}, Expression)
  end,
  band = function(env, a, b)
    return setmetatable({op = 'band', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  bor = function(env, a, b)
    return setmetatable({op = 'bor', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  bxor = function(env, a, b)
    return setmetatable({op = 'bxor', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  shr_asgn = function(env, a, b)
    return setmetatable({op = 'shr_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  shl_asgn = function(env, a, b)
    return setmetatable({op = 'shl_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  band_asgn = function(env, a, b)
    return setmetatable({op = 'band_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  bor_asgn = function(env, a, b)
    return setmetatable({op = 'bor_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  bxor_asgn = function(env, a, b)
    return setmetatable({op = 'bxor_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  -------- logical op ---------
  land = function(env, a, b)
    return setmetatable({op = 'land', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  lnot = function(env, a)
    return setmetatable({op = 'lnot', a = Expression(env, a)}, Expression)
  end,
  lor = function(env, a, b)
    return setmetatable({op = 'lor', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  -------- comparison op ---------
  ne = function(env, a, b)
    return setmetatable({op = 'ne', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  ls = function(env, a, b)
    return setmetatable({op = 'ls', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  gt = function(env, a, b)
    return setmetatable({op = 'gt', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  eq = function(env, a, b)
    return setmetatable({op = 'eq', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  le = function(env, a, b)
    return setmetatable({op = 'le', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  ge = function(env, a, b)
    return setmetatable({op = 'ge', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  -------- unary op ---------
  unp = function(env, a)
    return setmetatable({op = 'unp', a = Expression(env, a)}, Expression)
  end,
  unm = function(env, a)
    return setmetatable({op = 'unm', a = Expression(env, a)}, Expression)
  end,
  udec = function(env, a)
    return setmetatable({op = 'udec', a = Expression(env, a)}, Expression)
  end,
  uinc = function(env, a)
    return setmetatable({op = 'uinc', a = Expression(env, a)}, Expression)
  end,

  -------- math op ---------
  add = function(env, a, b)
    return setmetatable({op = 'add', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  sub = function(env, a, b)
    return setmetatable({op = 'sub', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  mul = function(env, a, b)
    return setmetatable({op = 'mul', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  div = function(env, a, b)
    return setmetatable({op = 'div', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  mod = function(env, a, b)
    return setmetatable({op = 'mod', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  add_asgn = function(env, a, b)
    return setmetatable({op = 'add_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  sub_asgn = function(env, a, b)
    return setmetatable({op = 'sub_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  mul_asgn = function(env, a, b)
    return setmetatable({op = 'mul_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  div_asgn = function(env, a, b)
    return setmetatable({op = 'div_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,
  mod_asgn = function(env, a, b)
    return setmetatable({op = 'mod_asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  inc = function(env, a)
    return setmetatable({op = 'inc', a = Expression(env, a)}, Expression)
  end,
  dec = function(env, a)
    return setmetatable({op = 'dec', a = Expression(env, a)}, Expression)
  end,

  -------- c op ---------
  asgn = function(env, a, b)
    return setmetatable({op = 'asgn', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  deref = function(env, a)
    return setmetatable({op = 'deref', a = Expression(env, a)}, Expression)
  end,
  cast = function(env, a, b)
    return setmetatable({op = 'cast', a = Expression(env, a), b = Type(env, b)}, Expression)
  end,

  cond = function(env, a, b, c)
    return setmetatable({op = 'cond', a = Expression(env, a), b = Expression(env, b), c = Expression(env, c)}, Expression)
  end,

  acc_ptrderef = function(env, a, b)
    return setmetatable({op = 'acc_ptrderef', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  addr = function(env, a)
    return setmetatable({op = 'addr', a = Expression(env, a)}, Expression)
  end,

  par = function(env, a)
    return setmetatable({op = 'par', a = Expression(env, a)}, Expression)
  end,

  call = function(env, a, b)
    return setmetatable({op = 'call', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  index = function(env, a, b)
    return setmetatable({op = 'index', a = Expression(env, a), b = Expression(env, b)}, Expression)
  end,

  -- leaf constructors
  Self = function(a)
    return setmetatable({cself = a}, Expression)
  end,
  Statement = function(a)
    return setmetatable({statement = a}, Expression)
  end,
  TypeSizeof = function(a)
    return setmetatable({typesizeof = a}, Expression)
  end,


  is_constant = function(self)
    return not (not self.constant)
  end,
  is_symbol = function(self)
    return not (not self.identifier)
  end,

  symbol_eval = function(self)
    if self.identifier then
      return self
    else
      return nil
    end
  end,


  repr = function(self, indent)
    if self.identifier then
      return self.identifier.value
    elseif self.list then
        local tab = {}
        for k,v in ipairs(self.list) do
          tab[#tab+1] = v:repr(indent)
        end
        return table.concat(tab, ", ")
    elseif self.op then
      local op = self.op
      local a = self.a
      local b = self.b
      local c = self.c
      if op == "eq" then
        return a:repr(indent) .. " == " .. b:repr(indent)
      elseif op == "shr" then
        return a:repr(indent) .. " >> " .. b:repr(indent)
      elseif op == "deref" then
        return "*" .. a:repr(indent)
      elseif op == "cast" then
        return "(" .. b:repr(indent) .. ") " .. a:repr(indent)
      elseif op == "ls" then
        return a:repr(indent) .. " < " .. b:repr(indent)
      elseif op == "land" then
        return a:repr(indent) .. " && " .. b:repr(indent)
      elseif op == "sizeof" then
        if a.op and a.op == "par" then
          return "sizeof" .. a:repr(indent) 
        else
          return "sizeof " .. a:repr(indent) 
        end
      elseif op == "fsizeof" then
        return "sizeof(" .. a:repr(indent) .. ")"
      elseif op == "ne" then
        return a:repr(indent) .. " != " .. b:repr(indent)
      elseif op == "udec" then
        return "--" .. a:repr(indent)
      elseif op == "shl" then
        return a:repr(indent) .. " >> " .. b:repr(indent)
      elseif op == "div" then
        return a:repr(indent) .. " / " .. b:repr(indent)
      elseif op == "ge" then
        return a:repr(indent) .. " >= " .. b:repr(indent)
      elseif op == "unp" then
        return "+" .. a:repr(indent)
      elseif op == "mul" then
        return a:repr(indent) .. " * " .. b:repr(indent)
      elseif op == "bor_asgn" then
        return a:repr(indent) .. " |= " .. b:repr(indent)
      elseif op == "band" then
        return a:repr(indent) .. " & " .. b:repr(indent)
      elseif op == "cond" then
        return a:repr(indent) .. " ? " .. b:repr(indent)  .. " : " .. c:repr(indent)
      elseif op == "inc" then
        return a:repr(indent) .. "++"
      elseif op == "band_asgn" then
        return a:repr(indent) .. " &= " .. b:repr(indent)
      elseif op == "gt" then
        return a:repr(indent) .. " > " .. b:repr(indent)
      elseif op == "shr_asgn" then
        return a:repr(indent) .. " >>= " .. b:repr(indent)
      elseif op == "shl_asgn" then
        return a:repr(indent) .. " <<= " .. b:repr(indent)
      elseif op == "bnot" then
        return "~" .. a:repr(indent)
      elseif op == "mod_asgn" then
        return a:repr(indent) .. " %= " .. b:repr(indent)
      elseif op == "addr" then
        return "&" .. a:repr(indent)
      elseif op == "bxor_asgn" then
        return a:repr(indent) .. " ^= " .. b:repr(indent)
      elseif op == "lnot" then
        return "!" .. a:repr(indent)
      elseif op == "mul_asgn" then
        return a:repr(indent) .. " *= " .. b:repr(indent)
      elseif op == "par" then
        return "(" .. a:repr(indent) .. ")" 
      elseif op == "mod" then
        return a:repr(indent) .. " % " .. b:repr(indent)
      elseif op == "unm" then
        return "- " .. a:repr(indent)
      elseif op == "sub" then
        return a:repr(indent) .. " - " .. b:repr(indent)
      elseif op == "div_asgn" then
        return a:repr(indent) .. " /= " .. b:repr(indent)
      elseif op == "asgn" then
        return a:repr(indent) .. " = " .. b:repr(indent)
      elseif op == "lor" then
        return a:repr(indent) .. " || " .. b:repr(indent)
      elseif op == "bor" then
        return a:repr(indent) .. " | " .. b:repr(indent)
      elseif op == "sub_asgn" then
        return a:repr(indent) .. " -= " .. b:repr(indent)
      elseif op == "le" then
        return a:repr(indent) .. " <= " .. b:repr(indent)
      elseif op == "bxor" then
        return a:repr(indent) .. " ^ " .. b:repr(indent)
      elseif op == "acc_deref" then
        return a:repr(indent) .. "." .. b.value
      elseif op == "dec" then
        return a:repr(indent) .. "--"
      elseif op == "call" then
        return  a:repr(indent) .. "(" .. b:repr(indent) .. ")"
      elseif op == "add_asgn" then
        return a:repr(indent) .. " += " .. b:repr(indent)
      elseif op == "index" then
        return a:repr(indent) .. "[" .. b:repr(indent) .. "]"
      elseif op == "add" then
        return a:repr(indent) .. " + " .. b:repr(indent)
      end
    elseif self.constant then
      return self.constant.value
    end

    dump(self, true)
    tassert(nil, false, "implement me")
  end,
})


Source = Class("Source", {
  constructor = function(env, tree)
    local self = {}
    for k, v in ipairs(tree) do
      if v.funcdef then
        self[#self+1] = Function(env, v)
      elseif v.decl or v.tdef or v.enum or v.struct or v.union then
        self[#self+1] = Declaration(env, v)
      elseif v.ns then
        self[#self+1] = Namespace(env, v)
      else
        dump(v)
        assert(false, "AST/nr")
      end
    end
    return self
  end,
  repr = function(self, indent)
    local tab = {}

    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent)
    end

    return table.concat(tab, "\n")
  end,
})

Parser = Class("Parser", {
  constructor = function(src)
    local self = {
      src = input(src):read("*a"),
      running = true,
      pos = 1,
      rt = {line = 1},
      env = {
        global = Environment(),
      }
    }
    self.env.current = self.env.global
    return self
  end
})

