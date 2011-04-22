Expression = Class("Expression", {
  h = {
    ["id"] = function(env, tree)
      local v = tree.iden.value
      local self = mktab(env, tree, {identifier = v}, Expression)
      local ref = tassert(tree.iden, env:sym_get_r(v),
        "no symbol '%s' in same scope", v)
      self.cid = tassert(nil, ref.cid, "AST/Expression no c id")
      self.ref = ref
      --dump(self, true)
      self.ctype = tassert(tree.iden, self:get_type(), "could not resolve type for '%s'", v)
      return self
    end,
    ["c"] = function(env, tree)
      local c = tree.const
      local self = mktab(env, tree, {ctype = Type.int_literal}, Expression)
      if c.oct then
        self.constant = c.oct.value
        self.kind = "oct"
      elseif c.dec then
        self.constant = c.dec.value
        self.kind = "dec"
      elseif c.hex then
        self.constant = c.hex.value
        self.kind = "hex"
      elseif c.char then
        self.constant = c.char.value
        self.kind = "char"
      elseif c.string then
        self.constant = c.string.value
        self.kind = "string"
      end
      return self
    end,
    -- ["self"]
    ["l"] = function(env, tree)
      local list = {}
      for k,v in ipairs(tree.list) do
        list[#list+1] = v
      end
      --tassert(nil, #list > 0, "empty expression list") TODO
      local self = mktab(env, tree, {list = list}, Expression)
      return self
    end,
    ["d"] = function(env, tree)
      local a = tree.dot
      local b = tree.iden.value

      local t, s = a:get_struct(env)

      --dump(t, true)

      local r = tassert(tree.iden, t and s, "'%s' is neither a struct not an union", a:repr("")):dereference()
      local f = tassert(tree.iden, r:get_field(b), "no field %s in:\n%s", b, "  " .. r:repr("  "))

      --tassert(b, false, "FOP")

      local self = mktab(env, tree, {op = 'acc', a = tree.dot, b = b, ctype = f.ctype}, Expression)
      return self
    end,

    -------- binary op ---------
    ["op_shl"] = function(env, tree)
      local self = mktab(env, tree, {op = 'shl', a = tree.shl, b = tree.expr}, Expression)
      return self
    end,
    ["op_shr"] = function(env, tree)
      local self = mktab(env, tree, {op = 'shr', a = tree.shr, b = tree.expr}, Expression)
      return self
    end,
    ["op_bnot"] = function(env, tree)
      local self = mktab(env, tree, {op = 'bnot', a = tree.bnot}, Expression)
      return self
    end,
    ["op_band"] = function(env, tree)
      local self = mktab(env, tree, {op = 'band', a = tree.band, b = tree.expr}, Expression)
      return self
    end,
    ["op_bor"] = function(env, tree)
      local self = mktab(env, tree, {op = 'bor', a = tree.bor, b = tree.expr}, Expression)
      return self
    end,
    ["op_bxor"] = function(env, tree)
      local self = mktab(env, tree, {op = 'bxor', a = tree.bxor, b = tree.expr}, Expression)
      return self
    end,

    ["op_shlasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'shl_asgn', a = tree.shl_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_shrasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'shr_asgn', a = tree.shr_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_bandasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'band_asgn', a = tree.band_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_borasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'bor_asgn', a = tree.bor_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_bxorasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'bxor_asgn', a = tree.bxor_asgn, b = tree.expr}, Expression)
      return self
    end,

    -------- logical op ---------
    ["op_lnot"] = function(env, tree)
      local self = mktab(env, tree, {op = 'lnot', a = tree.lnot}, Expression)
      return self
    end,
    ["op_land"] = function(env, tree)
      local self = mktab(env, tree, {op = 'land', a = tree.land, b = tree.expr}, Expression)
      return self
    end,
    ["op_lor"] = function(env, tree)
      local self = mktab(env, tree, {op = 'lor', a = tree.lor, b = tree.expr}, Expression)
      return self
    end,

    -------- comparison op ---------
    ["op_ne"] = function(env, tree)
      local self = mktab(env, tree, {op = 'ne', a = tree.ne, b = tree.expr}, Expression)
      return self
    end,
    ["op_eq"] = function(env, tree)
      local self = mktab(env, tree, {op = 'eq', a = tree.eq, b = tree.expr}, Expression)
      return self
    end,
    ["op_ls"] = function(env, tree)
      local self = mktab(env, tree, {op = 'ls', a = tree.ls, b = tree.expr}, Expression)
      return self
    end,
    ["op_gt"] = function(env, tree)
      local self = mktab(env, tree, {op = 'gt', a = tree.gt, b = tree.expr}, Expression)
      return self
    end,
    ["op_le"] = function(env, tree)
      local self = mktab(env, tree, {op = 'le', a = tree.le, b = tree.expr}, Expression)
      return self
    end,
    ["op_ge"] = function(env, tree)
      local self = mktab(env, tree, {op = 'ge', a = tree.ge, b = tree.expr}, Expression)
      return self
    end,

    -------- unary op ---------
    ["op_unp"] = function(env, tree)
      local self = mktab(env, tree, {op = 'unp', a = tree.unp}, Expression)
      return self
    end,
    ["op_unm"] = function(env, tree)
      local self = mktab(env, tree, {op = 'unm', a = tree.unm}, Expression)
      return self
    end,
    ["op_udec"] = function(env, tree)
      local self = mktab(env, tree, {op = 'udec', a = tree.udec}, Expression)
      return self
    end,
    ["op_uinc"] = function(env, tree)
      local self = mktab(env, tree, {op = 'uinc', a = tree.uinc}, Expression)
      return self
    end,

    -------- math op ---------
    ["op_add"] = function(env, tree)
      local self = mktab(env, tree, {op = 'add', a = tree.add, b = tree.expr}, Expression)
      return self
    end,
    ["op_sub"] = function(env, tree)
      local self = mktab(env, tree, {op = 'sub', a = tree.sub, b = tree.expr}, Expression)
      return self
    end,
    ["op_mul"] = function(env, tree)
      local self = mktab(env, tree, {op = 'mul', a = tree.mul, b = tree.expr}, Expression)
      return self
    end,
    ["op_div"] = function(env, tree)
      local self = mktab(env, tree, {op = 'div', a = tree.div, b = tree.expr}, Expression)
      return self
    end,
    ["op_mod"] = function(env, tree)
      local self = mktab(env, tree, {op = 'mod', a = tree.mod, b = tree.expr}, Expression)
      return self
    end,

    ["op_addasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'add_asgn', a = tree.add_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_subasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'sub_asgn', a = tree.sub_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_mulasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'mul_asgn', a = tree.mul_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_divasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'div_asgn', a = tree.div_asgn, b = tree.expr}, Expression)
      return self
    end,
    ["op_modasgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'mod_asgn', a = tree.mod_asgn, b = tree.expr}, Expression)
      return self
    end,

    ["op_inc"] = function(env, tree)
      local self = mktab(env, tree, {op = 'inc', a = tree.inc}, Expression)
      return self
    end,
    ["op_dec"] = function(env, tree)
      local self = mktab(env, tree, {op = 'dec', a = tree.dec}, Expression)
      return self
    end,

    -------- c op ---------
    ["op_asgn"] = function(env, tree)
      local self = mktab(env, tree, {op = 'asgn', a = tree.asgn, b = tree.expr}, Expression)
      return self
    end,

    ["op_deref"] = function(env, tree)
      local self = mktab(env, tree, {op = 'deref', a = tree.deref}, Expression)
      return self
    end,

    ["cast"] = function(env, tree)
      local self = mktab(env, tree, {op = 'cast', a = tree.cast, b = tree.ctype}, Expression)
      return self
    end,

    ["op_sizeof"] = function(env, tree)
      local self = mktab(env, tree, {op = 'sizeof', a = tree.sizeof}, Expression)
      return self
    end,
    ["op_fsizeof"] = function(env, tree)
      local self = mktab(env, tree, {op = 'fsizeof', a = tree.fsizeof}, Expression)
      return self
    end,

    ["cond"] = function(env, tree)
      local self = mktab(env, tree, {op = 'cond', a = tree.cond, b = tree.texpr, c = tree.fexpr}, Expression)
      return self
    end,

    --["op_acc_ptrderef"]

    ["op_addr"] = function(env, tree)
      local self = mktab(env, tree, {op = 'addr', a = tree.addr}, Expression)
      return self
    end,

    ["par"] = function(env, tree)
      local self = mktab(env, tree, {op = 'par', a = tree.par}, Expression)
      return self
    end,

    ["call"] = function(env, tree)
      local self = mktab(env, tree, {op = 'call', a = tree.call, b = tree.args}, Expression)
      return self
    end,

    ["idx"] = function(env, tree)
      local self = mktab(env, tree, {op = 'index', a = tree.index, b = tree.expr}, Expression)
      return self
    end,
  },

  opth = {
    ["eq"] = function(self, env)
      return Type.bool_res
    end,
    ["shr"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["deref"] = function(self, env)
      return self.a:get_type():strip_pointer()
    end,
    ["cast"] = function(self, env)
      return self.b
    end,
    ["ls"] = function(self, env)
      return Type.bool_res
    end,
    ["land"] = function(self, env)
      return Type.bool_res
    end,
    ["sizeof"] = function(self, env)
      return Type.uint_literal
    end,
    ["fsizeof"] = function(self, env)
      return Type.uint_literal
    end,
    ["ne"] = function(self, env)
      return Type.bool_res
    end,
    ["udec"] = function(self, env)
      return self.a:get_type()
    end,
    ["shl"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["div"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["ge"] = function(self, env)
      return Type.bool_res
    end,
    ["unp"] = function(self, env)
      return self.a:get_type()
    end,
    ["mul"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["bor_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["band"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["cond"] = function(self, env)
      -- TODO
      --dump(self, true)
      return self.b:get_type()
    end,
    ["inc"] = function(self, env)
      return self.a:get_type()
    end,
    ["uinc"] = function(self, env)
      return self.a:get_type()
    end,
    ["band_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["gt"] = function(self, env)
      return Type.bool_res
    end,
    ["shr_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["shl_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["bnot"] = function(self, env)
      return self.a:get_type()
    end,
    ["mod_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["addr"] = function(self, env)
      return self.a:get_type():to_pointer()
    end,
    ["bxor_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["lnot"] = function(self, env)
      return Type.bool_res
    end,
    ["mul_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["par"] = function(self, env)
      return tassert(env.loc[self], self.a:get_type(), "PAR")
    end,
    ["mod"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["unm"] = function(self, env)
      return self.a:get_type()
    end,
    ["sub"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["div_asgn"] = function(self, env)
    end,
    ["asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["lor"] = function(self, env)
      return Type.bool_res
    end,
    ["bor"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["sub_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["le"] = function(self, env)
      return Type.bool_res
    end,
    ["bxor"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
    ["acc"] = function(self, env)
      --return self.a:get_type(env)
    end,
    ["dec"] = function(self, env)
      return self.a:get_type()
    end,
    ["call"] = function(self, env)
      local fn = self.a:get_function()
      if not fn.rctype then
        dump(fn, true)
      end
      return tassert(env.loc[self], fn.rctype, "CALL")
    end,
    ["add_asgn"] = function(self, env)
      return self.a:get_type(env)
    end,
    ["index"] = function(self, env)
      if self.a.ctype.pointer and not self.a.ctype.array then
        return self.a:get_type():strip_pointer()
      else
        return self.a:get_type()
      end
    end,
    ["add"] = function(self, env)
      return tassert(env.loc[self], Type:get_arith_type(self.a:get_type(), self.b:get_type()), "IGG")
    end,
  },

  reprh = {
    ["eq"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " == " .. b:repr(indent)
    end,
    ["shr"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " >> " .. b:repr(indent)
    end,
    ["deref"] = function(self, op, a, b, c, indent)
      return "*" .. a:repr(indent)
    end,
    ["cast"] = function(self, op, a, b, c, indent)
      return "(" .. b:repr(indent) .. ") " .. a:repr(indent)
    end,
    ["ls"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " < " .. b:repr(indent)
    end,
    ["land"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " && " .. b:repr(indent)
    end,
    ["sizeof"] = function(self, op, a, b, c, indent)
      if a.op and a.op == "par" then
        return "sizeof" .. a:repr(indent) 
      else
        return "sizeof " .. a:repr(indent) 
      end
    end,
    ["fsizeof"] = function(self, op, a, b, c, indent)
      return "sizeof(" .. a:repr(indent) .. ")"
    end,
    ["ne"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " != " .. b:repr(indent)
    end,
    ["uinc"] = function(self, op, a, b, c, indent)
      return "++" .. a:repr(indent)
    end,
    ["udec"] = function(self, op, a, b, c, indent)
      return "--" .. a:repr(indent)
    end,
    ["shl"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " << " .. b:repr(indent)
    end,
    ["div"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " / " .. b:repr(indent)
    end,
    ["ge"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " >= " .. b:repr(indent)
    end,
    ["unp"] = function(self, op, a, b, c, indent)
      return "+" .. a:repr(indent)
    end,
    ["mul"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " * " .. b:repr(indent)
    end,
    ["bor_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " |= " .. b:repr(indent)
    end,
    ["band"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " & " .. b:repr(indent)
    end,
    ["cond"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " ? " .. b:repr(indent)  .. " : " .. c:repr(indent)
    end,
    ["inc"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. "++"
    end,
    ["band_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " &= " .. b:repr(indent)
    end,
    ["gt"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " > " .. b:repr(indent)
    end,
    ["shr_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " >>= " .. b:repr(indent)
    end,
    ["shl_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " <<= " .. b:repr(indent)
    end,
    ["bnot"] = function(self, op, a, b, c, indent)
      return "~" .. a:repr(indent)
    end,
    ["mod_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " %= " .. b:repr(indent)
    end,
    ["addr"] = function(self, op, a, b, c, indent)
      return "&" .. a:repr(indent)
    end,
    ["bxor_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " ^= " .. b:repr(indent)
    end,
    ["lnot"] = function(self, op, a, b, c, indent)
      return "!" .. a:repr(indent)
    end,
    ["mul_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " *= " .. b:repr(indent)
    end,
    ["par"] = function(self, op, a, b, c, indent)
      return "(" .. a:repr(indent) .. ")" 
    end,
    ["mod"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " % " .. b:repr(indent)
    end,
    ["unm"] = function(self, op, a, b, c, indent)
      return "- " .. a:repr(indent)
    end,
    ["sub"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " - " .. b:repr(indent)
    end,
    ["div_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " /= " .. b:repr(indent)
    end,
    ["asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " = " .. b:repr(indent)
    end,
    ["lor"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " || " .. b:repr(indent)
    end,
    ["bor"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " | " .. b:repr(indent)
    end,
    ["sub_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " -= " .. b:repr(indent)
    end,
    ["le"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " <= " .. b:repr(indent)
    end,
    ["bxor"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " ^ " .. b:repr(indent)
    end,
    ["acc"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. "." .. b
    end,
    ["dec"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. "--"
    end,
    ["call"] = function(self, op, a, b, c, indent)
      return  a:repr(indent) .. "(" .. b:repr(indent) .. ")"
    end,
    ["add_asgn"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " += " .. b:repr(indent)
    end,
    ["index"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. "[" .. b:repr(indent) .. "]"
    end,
    ["add"] = function(self, op, a, b, c, indent)
      return a:repr(indent) .. " + " .. b:repr(indent)
    end,
  },


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

  finr = function(self, env)
    if self.op and not self.ctype then
      local fn = self.opth[self.op]
      if not fn then
        dump(self, true, nil, nil)
      end
      local r = fn(self, env)
      if not r then
        dump({self.op, self}, nil, nil, nil, "TODO")
      end
      self.ctype = r
    end
  end,

  repr = function(self, indent)
    if self.identifier then
      return self.identifier
      --return  "(" .. self.ctype:repr(nil).. ")(" .. self.identifier.value .. ")"
    elseif self.constant then
      return  self.constant
      --return  "(" .. self.ctype:repr(indent).. ")(" .. self.constant.value .. ")"
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
      --return self.reprh[self.op](self, op, a, b, c, indent)
      if not self.ctype.repr then
        dump(self.ctype, true, nil, nil, "FUU")
      end
      return self.reprh[self.op](self, op, a, b, c, indent)
      --return "(" .. self.ctype:repr(nil).. ")(" .. self.reprh[self.op](self, op, a, b, c, indent) .. ")"
    end

    dump(self, true)
    tassert(nil, false, "implement me")
  end,

  get_function = function(self, env)
    if self.identifier then
      if not self.ref.get_function then
        dump({self.ref["@tag"], self.ref}, true)
      end
      return self.ref:get_function()
    end

    local fn = self.ctype:get_function(self, env)
    return fn
  end,

  get_struct = function(self, env)
    local ctype = self:get_type()
    return ctype, ctype:get_struct(env)
  end,

  get_type = function(self, env)
    if self.identifier then
      return self.ref:get_type()
    elseif self.list then
      local l = self.list
      local t = l[#l]
      local ctype = t:get_type()
      if not ctype then
        dump(t, true, nil, nil, "FUUU")
      end
      return ctype
    elseif self.op == "par" then
      return self.a:get_type()
    end
    return self.ctype
  end,
},
function(E, env, tree)
  if not E.h[tree.k] then dump(tree) end
  local ret = E.h[tree.k](env, tree)
  ret:finr(env);
  return ret
end)


