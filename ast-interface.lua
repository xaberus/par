Method = Class("Method", {
  get_type = function(self, env)
    return self.ctype:get_function()
  end,
  get_function = function(self, env)
    return self.ctype:get_function()
  end
},
function(F, env, iface, tree)
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
  }, Type) -- hacky? then watch next...

  local params = {}

  local stype = iface.ctype:to_pointer()
  local cself = setmetatable({
    ctype = stype,
    id = "self",
  }, Parameter)

  list[#list+1] = stype:strip_dep()
  penv:sym_reg("self", cself)

  for k, v in ipairs(tree.list) do
    local param = Parameter(penv, v)
    local ty = param:get_type()
    params[#params+1] = param
    list[#list+1] = ty:strip_dep()
  end

  local function tree_check_fn_proto(fna, fnb)
    local ret = {}

    if fna.id ~= fnb.id then 
      ret[#ret+1] = "  " .. fna.id .. " vs. " .. fnb.id
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

    if #ret > 0 then
      return concat(ret, "\n")
    end
  end

  local v = tree.funcdef.value
  local self = mktab(env, tree, {rctype = rctype, id = v, params = params, ctype = ftype}, F)

  self.cself = cself;

  if tree.forward then
    self.forward = true
    env:sym_reg(v, self)
    self.cid = env:ns_get_sym(v)
  else
    local benv = penv:child("block")
    local ref = env:sym_get(v)
    if ref then
      tassert(tree.funcdef, ref.forward, "attempt to redefine function '%s'", v)
      local err = tree_check_fn_proto(ref, self)
      tassert(tree.funcdef, not err, "forward declaration of wrong type:\n%s", err)

      ref.ref = self
    else
      env:sym_reg(v, self)
    end
    setmetatable(self, Method)
    self.cid = env:ns_get_sym(v)
    local block = Block(benv, tree.block)
    self.block = block
  end

  if tree.list.vararg then
    self.vararg = true
  end

  return self
end)

Interface = Class("Interface", {
  repr = function(self, indent)
    return "<iface>"
  end,
  declaration = function(I, env, self, tree)
    for k, v in ipairs(tree) do
      if v.decl then
        self[#self+1] = StructDeclaration(env, self.scope, self, v)
      elseif v.inherit then
        --dump(tree, nil, nil, nil, "idecl")
        local v = v.inherit.value
        tassert(v._m, v ~= self.id, "recursive interface definition")
        local iface = tassert(v._m, env:iface_get_r(v), "no interface '%s' in this scope", v)
        self.super = iface
      else
        dump(tree, nil, nil, nil, "idecl")
        tassert(tree._m, false, "AST/Interface declaration NIY")
      end
    end
  end,
  dereference = function(self)
    return self
  end,
  get_field = function(self, fid)
    local e = self.scope[fid]
    if e then return
      e.field
    end
  end,
},
function(I, env, tree)
  --dump(tree)


  local m = {}
  local self = mktab(env, tree, {scope = scope or {[0] = 1}, methods = m, env = env}, I)

  local id = tree.iface.value

  env:iface_reg(id, self)

  self.ctype = Type(env, tree)

  self.id = id
  self.cid = env:ns_get_iface(id)

  local env = env:child("iface")
  env.pfx = id  .. "_"
  env:type_reg("selftype", self.ctype)


  self.decl = I:declaration(env, self, tree.idecl)

  for k, v in ipairs(tree.idef) do
    if v.funcdef then
      m[#m+1] = Method(env, self, v)
    else
      dump(v, nil, nil, nil, "idef")
      tassert(tree._m, false, "AST/Interface NIY")
    end
  end

  return self
end)
