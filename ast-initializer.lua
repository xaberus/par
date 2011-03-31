Designation = Class("Designation", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "init")
    local self = {}
    for k, v in ipairs(tree) do
      if v.iden then
        self[#self+1] = {name = v.iden}
      elseif v.index then
        self[#self+1] = {name = Expression(env, index)}
      end
    end
    return self
  end,
  repr = function(self, indent)
    local tab = {}
    for k, v in ipairs(self) do
      if v.name then
        tab[#tab+1] = "." .. v.name.value
      elseif b.index then
        tab[#tab+1] = "[" .. v.index:repr(indent) .. "]"
      end
    end
    return concat(tab, " = ")
  end,
})

Initializer = Class("Initializer", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "init")
    if tree.asgnexpr then
      local expr = Expression(env, tree.asgnexpr)
      return {expr = expr}
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
      return {list = list}
    end
    dump(tree)
    tassert(nil, false)
  end,
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
      tassert(nil, false)
    end
  end,
})

