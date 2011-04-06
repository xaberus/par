Enumerator = Class("Enumerator", {
  repr = function(self, indent)
    if self.value then
      return self.id .. " = " .. self.value:repr(indent)
    else
      return self.id
    end
  end,
  get_type = function(self, env)
    return self.ctype
  end,
},
function(E, env, enum, tree)
  --dump(tree, nil, nil, nil, "enumerator")
  local self = mktab(env, tree, {}, E)
  local v = tree.iden.value
  self.enum = enum
  self.id = v
  if tree.value then
    self.value = Expression(env, tree.value)
  end
  self.ctype = enum.ctype
  env:sym_reg(v, self)
  self.cid = env:ns_get_sym(v)
  return self
end)

Enum = Class("Enum", {
  repr = function(self, indent)
    if #self>0 and indent then
      local tab = {}
      local indent2 = indent .. "  "
      for k, v in ipairs(self) do
        tab[#tab+1] = indent2 .. v:repr(indent)
      end
      return "enum " .. self.id .. " {\n" ..concat(tab, ",\n") .. "\n" .. indent .. "}"
    else
      return "enum " .. self.id
    end
  end,
},
function(E, env, tree)
  --dump(tree, nil, nil, nil, "enum")
  local list = tree.list
  local v = tassert(tree._m, tree.enum, "no id").value
  if list then
    local self = mktab(env, tree, {}, E)
    local etype = setmetatable({
      reg = "e";
      enum = self;
      qual = {};
      complete = true;
    }, Type) -- hacky?

    env:enum_reg(v, self)

    self.id = v
    self.cid = env:ns_get_enum(v)
    self.ctype = etype

    tassert(tree._m, #list > 0, "empty enumerations are not allowed")

    for k ,v in ipairs(list) do
      self[#self+1] = Enumerator(env, self, v)
    end
    return self
  else
    local enum = tassert(tree.enum, env:enum_get_r(v), "no enum %s in this scope", v)
    local self = mktab(env, tree, {}, E)
    self.id = v
    self.ctype = enum.ctype
    self.cid = env:ns_get_enum(v)
    return self
  end
end)

