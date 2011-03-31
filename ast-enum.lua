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
    return "enum " .. self.id.value .. " {\n" ..concat(tab, ",\n") .. "\n" .. indent .. "}"
  end,
})

