Enumerator = Class("Enumerator", {
  constructor = function(env, enum, tree)
    --dump(tree, nil, nil, nil, "enumerator")
    local self = {}
    self.enum = enum
    self.id = tree.iden
    if tree.value then
      self.value = Expression(env, tree.value)
    end
    self.ctype = enum.ctype
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
  get_type = function(self, env)
    return self.ctype
  end,
})

Enum = Class("Enum", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "enum")
    local list = tree.list
    if list then
      local self = {}
      local etype = setmetatable({
        reg = "e";
        enum = self;
        qual = {};
        complete = true;
      }, Type) -- hacky?

      self.id = tassert(nil, tree.enum, "no id")
      self.ctype = etype

      for k ,v in ipairs(list) do
        self[#self+1] = Enumerator(env, self, v)
      end
      return self
    else
      local enum = tassert(tree.enum, env:enum_get_r(tree.enum.value),
        "no enum %s in this scope", tree.enum.value)
      local self = {}
      self.id = tassert(nil, tree.enum, "no id")
      self.ctype = enum.ctype
      return self
    end
  end,
  repr = function(self, indent)
    if #self>0 and indent then
      local tab = {}
      local indent2 = indent .. "  "
      for k, v in ipairs(self) do
        tab[#tab+1] = indent2 .. v:repr(indent)
      end
      return "enum " .. self.id.value .. " {\n" ..concat(tab, ",\n") .. "\n" .. indent .. "}"
    else
      return "enum " .. self.id.value
    end
  end,
})

