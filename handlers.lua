
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


local write = function(...)
  io.stdout:write(unpack{...}, " ")
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
      structs = {},
      types = {},
      enums = {},
    }
  end,
  struct_reg = function(self, id, decl)
    local decl = {decl = decl}
    self.structs[id] = decl
    return decl
  end,
  struct_get = function(self, id)
    local struct = self.structs[id]
    if not struct then
      return {unresolved_struct = id}
      --error(format("undefined struct %s!", id))
    end
    return struct
  end,

  enum_reg = function(self, id, decl)
    local decl = {decl = decl}
    self.enums[id] = decl
    return decl
  end,
  enum_get = function(self, id)
    local enum = self.enums[id]
    if not enum then
      error(format("undefined enum %s!", id))
    end
    return enum
  end,


  type_reg = function(self, id, t)
    local t = {t = t}
    self.types[id] = t
    return t
  end,
  set_ast = function(self, ast)
    self.ast = ast
    return ast
  end,
})

Translation = Class("Translation", {

})

Declaration = Class("Declaration", {
  Typefef = function(id, ctype)
    return setmetatable({
       id = id,
       ctype = ctype,
    }, Declaration)
  end,
  Plain = function(id, ctype, list)
    return setmetatable({
      id = id,
      list = list,
      ctype = ctype,
    }, Declaration)
  end,
  Struct = function(struct)
    return setmetatable({
      struct = struct,
    }, Declaration)
  end,
  Enum = function(enum)
    return setmetatable({
      enum = enum,
    }, declaration)
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


Type = Class("Type", {
  constructor = function()
    return {
      complete = false,
      flags = {
      },
      reg = nil,
    }
  end,
  repr = function(self, lvl, fd)
    if self.complete then
      if self.reg == "i" then
        fd:write(indent(lvl))
        local first = true
        for k, v in pairs(self.flags) do
          if not first then
            fd:write(" " .. k)
          else
            fd:write(k)
            first = false
          end
        end
        for k, v in pairs(self.con) do
          if not first then
            fd:write(" " .. k)
          else
            fd:write(k)
            first = false
          end
        end
      end

      if self.pointer then
        for i, v in ipairs(self.pointer) do
          fd:write(" *")
          for k, q in pairs(v) do
            fd:write(" " .. k)
          end
        end
      end

      if self.array then
        for i, p in ipairs(self.array) do
          if p.size then
            fd:write(format("[%u]", p.size))
          elseif p.vararray then
            fd:write("[]")
          else
            fd:write("<invalid>")
          end
        end
      end
    else
      fd:write(indent(lvl) .. "<incomplete type>")
    end
    fd:write("\n") -- XXX:remove me
  end,
  set_int_flag = function(self, flag, ...)
    assert(not self.reg or self.reg == "i")
    if not self.reg then
      self.reg = "i"
      self.int = {}
      self.con = {}
    end

    local function mkadd(int, con, flag)
      if flag == "signed" then if true
        and not int.unsigned
          then con.signed = true else con.signed = nil end
      elseif flag == "unsigned" then if true
        and not int.signed
          then con.unsigned = true else con.unsigned = nil end
      elseif flag == "char" then if true
        and not int.int
        and not int.short
        and not int.long
          then con.char = true else con.char = nil end
      elseif flag == "short" then if true
        and not int.char
        and not int.int
        and not int.long
          then con.short = true else con.short = nil end
      elseif flag == "long" then if true
        and not int.char
        and not int.int
        and not int.long
          then con.long = true else con.long = nil end
      elseif flag == "int" then if true
        and not int.char
        and not int.int
          then con.int = true else con.int = nil end
      else
        assert(false, flag)
      end
    end

    if flag == "signed" then
      assert(not self.int.unsigned, "signed vs. unsigned")
      self.int.signed = true
    elseif flag == "unsigned" then
      assert(not self.int.signed, "signed vs. unsigned")
      self.int.unsigned = true
    elseif flag == "char" then
      assert(not self.int.short, "char vs. char")
      assert(not self.int.short, "char vs. short")
      assert(not self.int.long, "char vs. long")
      assert(not self.int.int, "char vs. int")
      self.int.char = true
    elseif flag == "short" then
      assert(not self.int.char, "short vs. char")
      assert(not self.int.short, "short vs. short")
      assert(not self.int.long, "short vs. long")
      assert(not self.int.int, "short vs. int")
      self.int.short = true
    elseif flag == "long" then
      assert(not self.int.char, "long vs. char")
      assert(not self.int.short, "long vs. short")
      assert(not self.int.long, "long vs. long")
      self.int.long = true
    elseif flag == "int" then
      assert(not self.int.char, "int vs. char")
      assert(not self.int.int, "int vs. int")
      self.int.int = true
    end

    for i, v in ipairs({...}) do
      mkadd(self.int, self.con, v)
    end

  end,
  Plain = function(spec)
    local self = Type()
    for k, s in ipairs(spec) do
      --dump(s)
      if s.qualifier then
        local q = s.qualifier
        if q.tag == "token" then
          local v = q.value
          if v == "const" then
            assert(not self.flags.volatile, "const vs. volatile")
            self.flags.const = true
          elseif v == "volatile" then
            assert(not self.flags.const, "volatile vs. const")
            self.flags.volatile = true
          elseif v == "restrict" then
            self.flags.restrict = true
          else
            assert(false)
          end
        else
          assert(false)
        end
      elseif s.specifier then
        local q = s.specifier
        if q.tag == "token" then
          local v = q.value
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
          else
            assert(false)
          end
        else
          assert(false)
        end
      else
        assert(false)
      end
    end

    if self.reg == "i" then
      local con = self.con
      for k, v in pairs(self.int) do
        con[k] = v
      end
      self.complete = true
    else
      assert(false)
    end

    return self
  end,
  Abstract = function(spec, abstract)
    local self = Type.Plain(spec)

    dump(abstract)

    local body = {} -- pointer
    local tail = {} -- array

    if abstract.pointer then
      local pointer = {}
      for i, p in ipairs(abstract.pointer) do
        local ref = {}
        for k, q in ipairs(p.qualifiers) do
          if q.tag == "token" then
            local v = q.value
            if v == "const" then
              assert(not ref.volatile, "const vs. volatile")
              ref.const = true
            elseif v == "volatile" then
              assert(not ref.const, "volatile vs. const")
              ref.volatile = true
            elseif v == "restrict" then
              ref.restrict = true
            else
              assert(false)
            end
          else
            assert(false)
          end
        end
        table.insert(pointer, ref)
      end
      self.pointer = pointer
    end

    if abstract.abstract then
      local array, param

      for k, a in ipairs(abstract.abstract) do
        if a.array or a.vararray then
          array = array or {}
          local pt = {}
          if a.size then
            pt.size = a.size:constant_eval()
          elseif a.vararray then
            -- assert(k == #abstract.abstract)
            pt.vararray = true
          else
            assert(false)
          end
          table.insert(array, pt)
        end
      end

      if array then
        self.array = array
      else
        assert(false)
      end
    end

    return self;
  end,
})

Parameter = Class("Parameter", {
  constructor = function(id, ctype)
    return {
      id = id,
      ctype = ctype,
    }
  end,
})


Struct = Class("Struct", {
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
      --dump(ctype)
      error("anonymous member is not a struct?")
    end
  end,
})

Enum = Class("Enum", {
  Anonymous = function(decl)
    return setmetatable({
       anonymous = true,
       decl = decl,
    }, Enum)
  end,
  Named = function(id, decl)
    return setmetatable({
      id = id,
      decl = decl,
    }, Enum)
  end,

})


Initializer = Class("Initializer", {
  Plain = function(ctype)
    return setmetatable({
      ctype = ctype,
    }, Initializer)
  end,
  List = function(list)
    return setmetatable({
      list = list,
    }, Initializer)
  end,

})

Function = Class("Function", {
  constructor = function(spec, ctype, id, params, statement)
    return {
      spec = spec,
      ctype = ctype,
      id = id,
      params = params,
      statement = statement,
    }
  end,
  Abstract = function(spec, ctype, id, params)
    return setmetatable({
      spec = spec,
      ctype = ctype,
      id = id,
      params = params,
    }, Function)
  end
})

Block = Class("Block", {
  List = function(list)
    return setmetatable({
      list = list,
    }, Block)
  end,
})

Statement = Class("Statement", {
  Jump = function(jump)
    return setmetatable({
      stmt = "jump",
      jump = jump,
    }, Statement)
  end,
  Expression = function(expr)
    return setmetatable({
      stmt = "expression",
      expr = expr,
    }, Statement)
  end,
  If = function(opened, clause, dostmt)
    return setmetatable({
      stmt = "if",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
    }, Statement)
  end,
  IfElse = function(opened, clause, dostmt, elsestmt)
    return setmetatable({
      stmt = "elseif",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
      elsestmt = elsestmt,
    }, Statement)
  end,
  Compound = function(compound)
    return setmetatable({
      stmt = "compound",
      compound = compound,
    }, Statement)
  end,
  For = function(opened, clause, dostmt)
    return setmetatable({
      stmt = "for",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
    }, Statement)
  end,
  While = function(opened, clause, dostmt)
    return setmetatable({
      stmt = "while",
      opened = opened,
      clause = clause,
      dostmt = dostmt,
    }, Statement)
  end,
  Label = function(label)
    return setmetatable({
      stmt = "label",
      label = label,
    }, Statement)
  end,
  Switch = function(switch)
    return setmetatable({
      stmt = "switch",
      switch = switch,
    }, Statement)
  end,

})

Expression = Class("Expression", {
  eq = function(self, b)
    return setmetatable({op = 'eq', a = self, b = b}, Expression)
  end,
  shr = function(self, b)
    return setmetatable({op = 'shr', a = self, b = b}, Expression)
  end,
  deref = function(self)
    return setmetatable({op = 'deref', a = self}, Expression)
  end,
  cast = function(self, b)
    return setmetatable({op = 'cast', a = self, b = b}, Expression)
  end,
  ls = function(self, b)
    return setmetatable({op = 'ls', a = self, b = b}, Expression)
  end,
  land = function(self, b)
    return setmetatable({op = 'land', a = self, b = b}, Expression)
  end,
  sizeof = function(self)
    return setmetatable({op = 'sizeof', a = self}, Expression)
  end,
  ne = function(self, b)
    return setmetatable({op = 'ne', a = self, b = b}, Expression)
  end,
  udec = function(self)
    return setmetatable({op = 'udec', a = self}, Expression)
  end,
  shl = function(self, b)
    return setmetatable({op = 'shl', a = self, b = b}, Expression)
  end,
  div = function(self, b)
    return setmetatable({op = 'div', a = self, b = b}, Expression)
  end,
  ge = function(self, b)
    return setmetatable({op = 'ge', a = self, b = b}, Expression)
  end,
  unp = function(self)
    return setmetatable({op = 'unp', a = self}, Expression)
  end,
  mul = function(self, b)
    return setmetatable({op = 'mul', a = self, b = b}, Expression)
  end,
  add_expr = function(self, b)
    return setmetatable({op = 'add_expr', a = self, b = b}, Expression)
  end,
  bor_asgn = function(self, b)
    return setmetatable({op = 'bor_asgn', a = self, b = b}, Expression)
  end,
  band = function(self, b)
    return setmetatable({op = 'band', a = self, b = b}, Expression)
  end,
  cond = function(self, b, c)
    return setmetatable({op = 'cond', a = self, b = b, c = c}, Expression)
  end,
  inc = function(self)
    return setmetatable({op = 'inc', a = self}, Expression)
  end,
  band_asgn = function(self, b)
    return setmetatable({op = 'band_asgn', a = self, b = b}, Expression)
  end,
  gt = function(self, b)
    return setmetatable({op = 'gt', a = self, b = b}, Expression)
  end,
  shr_asgn = function(self, b)
    return setmetatable({op = 'shr_asgn', a = self, b = b}, Expression)
  end,
  shl_asgn = function(self, b)
    return setmetatable({op = 'shl_asgn', a = self, b = b}, Expression)
  end,
  bnot = function(self)
    return setmetatable({op = 'bnot', a = self}, Expression)
  end,
  acc_ptrderef = function(self, b)
    return setmetatable({op = 'acc_ptrderef', a = self, b = b}, Expression)
  end,
  mod_asgn = function(self, b)
    return setmetatable({op = 'mod_asgn', a = self, b = b}, Expression)
  end,
  uinc = function(self)
    return setmetatable({op = 'uinc', a = self}, Expression)
  end,
  addr = function(self)
    return setmetatable({op = 'addr', a = self}, Expression)
  end,
  bxor_asgn = function(self, b)
    return setmetatable({op = 'bxor_asgn', a = self, b = b}, Expression)
  end,
  lnot = function(self)
    return setmetatable({op = 'lnot', a = self}, Expression)
  end,
  mul_asgn = function(self, b)
    return setmetatable({op = 'mul_asgn', a = self, b = b}, Expression)
  end,
  par = function(self)
    return setmetatable({op = 'par', a = self}, Expression)
  end,
  mod = function(self, b)
    return setmetatable({op = 'mod', a = self, b = b}, Expression)
  end,
  unm = function(self)
    return setmetatable({op = 'unm', a = self}, Expression)
  end,
  sub = function(self, b)
    return setmetatable({op = 'sub', a = self, b = b}, Expression)
  end,
  div_asgn = function(self, b)
    return setmetatable({op = 'div_asgn', a = self, b = b}, Expression)
  end,
  asgn = function(self, b)
    return setmetatable({op = 'asgn', a = self, b = b}, Expression)
  end,
  lor = function(self, b)
    return setmetatable({op = 'lor', a = self, b = b}, Expression)
  end,
  bor = function(self, b)
    return setmetatable({op = 'bor', a = self, b = b}, Expression)
  end,
  sub_asgn = function(self, b)
    return setmetatable({op = 'sub_asgn', a = self, b = b}, Expression)
  end,
  le = function(self, b)
    return setmetatable({op = 'le', a = self, b = b}, Expression)
  end,
  bxor = function(self, b)
    return setmetatable({op = 'bxor', a = self, b = b}, Expression)
  end,
  acc_deref = function(self, b)
    return setmetatable({op = 'acc_deref', a = self, b = b}, Expression)
  end,
  dec = function(self)
    return setmetatable({op = 'dec', a = self}, Expression)
  end,
  add_call = function(self, b)
    return setmetatable({op = 'add_call', a = self, b = b}, Expression)
  end,
  add_asgn = function(self, b)
    return setmetatable({op = 'add_asgn', a = self, b = b}, Expression)
  end,
  add_index = function(self)
    return setmetatable({op = 'add_index', a = self}, Expression)
  end,
  add = function(self, b)
    return setmetatable({op = 'add', a = self, b = b}, Expression)
  end,

  ------------------

  List = function(self)
    return setmetatable({list = {self}}, Expression)
  end,

  -- leaf constructors

  Identifier = function(a)
    return setmetatable({identifier = a}, Expression)
  end,
  Self = function(a)
    return setmetatable({cself = a}, Expression)
  end,
  Constant = function(kind, a)
    return setmetatable({constant = a, kind = kind}, Expression)
  end,
  Statement = function(a)
    return setmetatable({statement = a}, Expression)
  end,
  TypeSizeof = function(a)
    return setmetatable({typesizeof = a}, Expression)
  end,


  constant_eval = function(self)
    if self.constant then
      return tonumber(self.constant.value)
    else
      assert(false)
    end
  end
})


Parser = Class("Parser", {
  constructor = function(src)
    return {
      src = input(src):read("*a"),
      running = true,
      pos = 1,
      rt = {line = 1},
      env = Environment()
    }
  end
})


local green = "[1;32m"
local yellow = "[1;33m"
local red = "[1;31m"
local blue = "[1;34m"
local white = "[1;37m"
local clear = "[0;m"

local function line(lvl, ...)
  if lvl then
    return string.rep ("  ", lvl) .. string.format(unpack{...})
  else
    return string.format(unpack{...})
  end
end

env_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\ntype_meta: not found field " .. k)
    end
  end,
  struct_reg = function(self, idstr, struct)
    self.structs[idstr] = struct
    return struct
  end,

  type_reg = function(self, idstr, type)
    --print(string.format("reg %s", idstr))
    self.defs[idstr] = type
    return true
  end,
  type_get = function(self, idstr)
    --print(string.format("get %s", idstr))
    return self.defs[idstr]
  end,
}

--setmetatable(env, env_meta)

type_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\ntype_meta: not found field " .. k)
    end
  end,
  tag = "type",
  join = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,

  last = function(self, create)
    local l = rawget(self,#self)
    if not l and create then
      l = {}
      table.insert(self, l)
    end
    return l
  end,
  
  add_struct_specifier  = function(self, struct)
    local t = self:last(true)
    t.struct_spec = {struct = struct}
    return self
  end,
  add_type_specifier = function(self, name)
    local t = self:last(true)
    t.spec = {n = name}
    return self
  end,
  add_qualifier = function(self, name)
    local t = self:last(true)
    t.qual = {n = name}
    return self
  end,
  add_ref = function(self, kind, extra)
    local t = self:last(true)
    if not t.ref then
      t.ref = {k = kind, d=1, e = extra}
    else
      t.ref.d = t.ref.d + 1
    end
    return self
  end,
  add_vararray = function(self)
    local t = self:last(true)
    t.array = {n = 0, v = true}
    return self
  end,
  add_array = function(self, n)
    local t = self:last(true)
    t.array = {n = n, v = false}
    return self
  end,
  add_params = function(self, p)
    local t = self:last(true)
    t.params = p
    return self
  end,
  add_type_ref = function(self, ref)
    local t = self:last(true)
    t.ref = { id = ref, ref = ref._ref }
    return self
  end,
}

setmetatable(type_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, type_meta)
  end,
})

param_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nparam_meta: not found field " .. k)
    end
  end,
  tag = "param",

  next = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,

  add_decl = function(self, t, id)
    self[#self+1] = {t = t, id = id}
    return self
  end,
}

setmetatable(param_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, param_meta)
  end,
})


decl_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\ndecl_meta: not found field " .. k)
    end
  end,
  tag = "decl",

  next = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,

  add_id = function(self, id, init)
    self[#self+1] = {id = id, init = init}
    return self
  end,
  add_type = function(self, t)
    self.t = t
    return self
  end,
  add_stor_class = function(self, sc)
    self.sc = sc
    return self
  end,
}

setmetatable(decl_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, decl_meta)
  end,
})


init_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nblock_meta: not found field " .. k)
    end
  end,
  tag = "init",
  add_expr = function(self, expr)
    self[#self+1] = {expr = expr}
    return self
  end,
}

setmetatable(init_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, init_meta)
  end,
})

block_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nblock_meta: not found field " .. k)
    end
  end,
  tag = "block",
  join = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,

  add_decl = function(self, decl)
    self[#self+1] = {decl = decl}
    return self
  end,
  add_stmt = function(self, stmt)
    self[#self+1] = {stmt = stmt}
    return self
  end,
  
  finalize = function(self)
    --dump(self)
    --print(tostring(self))
    return self
  end,
}

setmetatable(block_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, block_meta)
  end,
})

stmt_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nstmt_meta: not found field " .. k)
    end
  end,
  tag = "stmt",

  add_expr = function(self, expr)
    self.expr = expr
    return self
  end,
  add_if_clause = function(self, expr)
    self.ic = {e = expr}
    return self
  end,
  add_if_else = function(self, ds, es)
    self.ic.ds = ds
    self.ic.es = es
    return self
  end,
  add_return = function(self, expr)
    self.r = expr
    return self
  end,
  add_for_clause = function(self, init, cond, act)
    self.f = {init = init, cond = cond, act = act}
    return self
  end,
  add_switch_clause = function(self, expr)
    self.sw = {expr = expr, cases = {}}
    return self
  end,
  add_case = function(self, case)
    self.case = {case}
    return self
  end,
  add_for_stmt = function(self, stmt)
    self.f.s = stmt
    return self
  end,
  gen_c_expr_stmt = function(expr)
    return setmetatable({
      closed = true,
      expr = expr,
    }, stmt_meta)
  end,
  gen_c_if_else_stmt = function(c, at, af)
    return setmetatable({
      closed = true,
      if_else_clause = c,
      act_true = at,
      act_false = af,
    }, stmt_meta)
  end,
  gen_c_compound_stmt = function(c)
    return setmetatable({
      closed = true,
      compound = c,
    }, stmt_meta)
  end,
  gen_c_for_stmt = function(c, l)
    return setmetatable({
      closed = true,
      for_clause = c,
      loop = l,
    }, stmt_meta)
  end,
  gen_c_label_stmt= function(c, label, stmt)
    return true
  end,

  gen_o_if_stmt = function(c, at)
    return setmetatable({
      closed = false,
      if_clause = c,
      act_true = at,
    }, stmt_meta)
  end,
  gen_c_jump_stmt = function(j)
    return setmetatable({
      closed = true,
      jump = j,
    }, stmt_meta)
  end,
}

setmetatable(stmt_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, stmt_meta)
  end,
})

trans_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\ntrans_meta: not found field " .. k)
    end
  end,
  tag = "trans",
  join = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,

  add_decl = function(self, decl)
    self[#self+1] = {decl = decl}
    return self
  end,
  add_fun = function(self, fun)
    self[#self+1] = {fun = fun}
    return self
  end,
}

setmetatable(trans_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, trans_meta)
  end,
})

func_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nfunc_meta: not found field " .. k)
    end
  end,
  tag = "func",

  add_ret = function(self, t)
    self.ret = t
    return self
  end,
  add_id = function(self, id)
    self.id = id
    return self
  end,
  add_param = function(self, param)
    self.param = param
    return self
  end,
  add_comp = function(self, comp)
    self.comp = comp
    return self
  end,
  add_spec = function(self, spec)
    self.spec = spec
    return self
  end,
}

setmetatable(func_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, func_meta)
  end,
})

-------------------------------------

struct_decl_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nstruct_decl_meta: not found field " .. k)
    end
  end,
  tag = "struct_decl",
  join = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,

  add_id = function(self, id, depth)
    self[#self+1] = {id = id, d = depth}
    return self
  end,
  add_type = function(self, t)
    self.t = t
    return self
  end,
}

setmetatable(struct_decl_meta, {
  __call = function()
    local ret = {}
    return setmetatable(ret, struct_decl_meta)
  end,
})

struct_meta = {
  __index = function(self,k)
    local r = getmetatable(self)[k]
    if r then
      return r
    else
      --error(debug.traceback().."\nstruct_meta: not found field " .. k)
    end
  end,
  tag = "struct",
  
  next = function(a, b)
    for i,v in ipairs(b) do
      a[#a+1] = v
    end
    return a
  end,
  
  add_field = function(self, field)
    self[#self + 1] = field
    return self
  end,
  add_kind = function(self, kind)
    self.k = kind
    return self
  end,
  add_id = function(self, id)
    self.id = id
    return self
  end,
}

setmetatable(struct_meta, {
  __call = function(self)
    local ret = {}
    return setmetatable(ret, struct_meta)
  end,
})

------------------------------------------------------------------------

