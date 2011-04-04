Designation = Class("Designation", {
  repr = function(self, indent)
    local tab = {}
    for k, v in ipairs(self) do
      if v.name then
        tab[#tab+1] = "." .. v.name
      elseif b.index then
        tab[#tab+1] = "[" .. v.index:repr(indent) .. "]"
      end
    end
    return concat(tab, " = ")
  end,
},
function(D, env, tree)
  --dump(tree, nil, nil, nil, "init")
  local self = mktab(env, tree, {}, D)
  for k, v in ipairs(tree) do
    if v.iden then
      self[#self+1] = mktab(env, v, {name = v.iden.value})
    elseif v.index then
      self[#self+1] = mktab(env, v, {name = Expression(env, index)})
    end
  end
  return self
end)

Initializer = Class("Initializer", {
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
      return concat(tab, "")
    else
      dump(self, true)
      tassert(nil, false, "AST/Initializer not reached")
    end
  end,
},
function(I, env, tree)
  --dump(tree, nil, nil, nil, "init")
  if tree.asgnexpr then
    local expr = Expression(env, tree.asgnexpr)
    local self = mktab(env, tree, {expr = expr}, I)
    return self
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
    local self = mktab(env, tree, {list = list}, I)
    return self
  end
  dump(tree)
  tassert(tree._m, false, "AST/Initializer not reached")
end)

