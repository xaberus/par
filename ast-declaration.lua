Declarator = Class("Declarator", {
  repr = function(self, indent)
    return self.ctype:repr(indent) .. " " .. self.id
  end,
  get_type = function(self)
    return self.ctype
  end,
},
function(D, env, ctype, tree)
  local self = mktab(env, tree, {}, D)
  if tree.init then
    local init = tree.init
    self.init = init
  end
  local v = tree.iden.value
  self.id = v
  self.ctype = ctype

  env:sym_reg(v, self)

  self.cid = tassert(nil, env:ns_get_sym(v), "AST/Declarator no c id")

  return self
end)

Declaration = Class("Declaration", {
  repr = function(self, indent)
    if self.pure then -- plain declaration
      local tab = {}
      for k, v in ipairs(self) do
        if v.init then
          tab[#tab+1] = v.id .. " = " .. v.init:repr(indent)
        else
          tab[#tab+1] = v.id
        end
      end
      if indent then
        return indent .. self.ctype:repr(indent) .. " " .. concat(tab, ", ") .. ";\n"
      else
        return self.ctype:repr(indent) .. " "  .. concat(tab, ", ")
      end
    elseif self.struct then
      return indent .. self.struct:repr(indent) .. ";\n"
    elseif self.tdef then
      if indent then
        return indent .. "typedef " .. self.ctype:repr(indent) .. " " .. self.id .. ";\n"
      else
        return "typedef " .. self.ctype:repr() .. " " .. self.id
      end
    elseif self.enum then
      return indent .. self.enum:repr(indent) .. ";\n"
    end
  end,
},
function(D, env, tree)
  --dump(tree, nil, nil, nil, "decl")

  if tree.pure then
    local self = mktab(env, tree, {}, D)
    self.pure = true
    local ctype = tree.ctype
    for k, v in ipairs(tree.decl) do
      self[#self+1] = Declarator( env, ctype, v)
    end
    self.ctype = ctype
    return self
  elseif tree["@tag"] == "Struct" then
    local self = mktab(env, {_m = env.loc[tree]}, {}, D)
    self.struct = tree
    self.id = tree.id
    return self
  elseif tree.tdef then
    local v = tree.tdef.value
    local self = mktab(env, tree, {}, D)
    self.tdef = true
    self.id = v
    self.ctype = tree.ctype
    env:type_reg(v, self)
    self.cid = tassert(nil, env:ns_get_type(v), "AST/Declaration no c id")
    return self
  elseif tree["@tag"] == "Enum" then
    local self = mktab(env, {_m = env.loc[tree]}, {}, D)
    self.enum = tree
    return self
  else
    dump(tree, true)
    tassert(tree._m, false, "AST/Declaration not reached")
  end

end)

