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

  get_type = function(self)
    return self.ctype
  end,
  get_function = function(self, env)
    return self.ctype:get_function()
  end
})

Function = Class("Function", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "funcdef")

    local rctype = Type(env, tree.rctype)

    local penv = env:child("param")

    local list = {}
    local ftype = setmetatable({
      reg = "c";
      list = list;
      rctype = rctype;
      qual = {};
      complete = true;
    }, Type) -- hacky?

    local params = {}
    for k, v in ipairs(tree.list) do
      local param = Parameter(penv, v)
      local ty = param:get_type()
      params[#params+1] = param
      list[#list+1] = ty:strip_dep()
    end

    local function tree_check_fn_proto(fna, fnb)
      local ret = {}

      if fna.id.value ~= fnb.id.value then 
        ret[#ret+1] = "  " .. fna.id.value .. " vs. " .. fnb.id.value
      end

      --dump({fna.rctype, fnb.rctype}, true)

      if not Type.compare(fna.rctype, fnb.rctype) then 
          ret[#ret+1] = format("  return: %s vs. %s", fna.rctype:repr(""), fnb.rctype:repr(""))
      end
      if #fna.params ~= #fnb.params then
        ret[#ret+1] = format("  %d arguments  vs. %d arguments",  #fna.params, #fnb.params)
      else
        params = fna.params
        for k, v in ipairs(params) do
          local a = fna.params[k]
          local b = fnb.params[k]
          if not Type.compare(a.ctype, b.ctype) then
            ret[#ret+1] = format("  argument %d: %s vs. %s", k, a:repr(""), b:repr(""))
          end
        end
      end

      return concat(ret, "\n")
    end

    local self

    if tree.forward then
      self = {rctype = rctype, id = tree.funcdef, params = params, forward = true, ctype = ftype}
      env:sym_reg(tree.funcdef.value, self)
    else
      local benv = penv:child("block")
      local ref = env:sym_get(tree.funcdef.value)
      if ref then
        tassert(tree.funcdef, ref.forward, "attempt to redefine function '%s'", tree.funcdef.value)
        self = {rctype = rctype, id = tree.funcdef, params = params, ctype = ftype}

        local err = tree_check_fn_proto(ref, self)
        tassert(tree.funcdef, #err == 0, "forward declaration of wrong type:\n%s", err)

        ref.ref = self
      else
        self = {rctype = rctype, id = tree.funcdef, params = params, ctype = ftype}
        env:sym_reg(self.id.value, self)
      end
      setmetatable(self, Function)
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
      tab[#tab+1] = ";\n"
      return concat(tab, "")
    else
      dump(self, true)
      tassert(nil, false)
    end
  end,
  get_type = function(self, env)
    return self.ctype:get_function()
  end,
  get_function = function(self, env)
    return self.ctype:get_function()
  end
})
