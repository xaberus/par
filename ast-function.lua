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
      tab[#tab+1] = concat(spec, " ")
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

    tab[#tab+1] = "(" .. concat(prt, ", ") .. ")"


    if self.block and #self.block then
      tab[#tab+1] = "\n"
      tab[#tab+1] = indent
      tab[#tab+1] = self.block:repr(indent)
      tab[#tab+1] = "\n"
      return concat(tab, "")
    elseif self.forward then
      tab[#tab+1] = ";"
      return concat(tab, "")
    else
      dump(self, true)
      tassert(nil, false)
    end
  end,
})
