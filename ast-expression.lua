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
        return concat(tab, ", ")
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


