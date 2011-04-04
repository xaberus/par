Declarator = Class("Declarator", {
  get_type = function(self)
    return self.ctype
  end,
},
function(D, env, ctype, tree)
  local self = mktab(env, tree, {}, D)
  if tree.init then
    local init = Initializer(env, tree.init)
    self.init = init
  end
  local v = tree.iden.value
  self.id = v
  self.ctype = ctype

  env:sym_reg(v, self)

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
        return self.ctype:repr(indent) .. " "  .. concat(tab, ", ") .. ";"
      end
    elseif self.struct then
      return indent .. self.struct:repr(indent) .. ";\n"
    elseif self.tdef then
      return indent .. "typedef " .. self.ctype:repr(indent) .. " " .. self.id .. ";\n"
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
    local ctype = Type(env, tree.ctype)
    for k, v in ipairs(tree.decl) do
      self[#self+1] = Declarator(env, ctype, v)
    end
    self.ctype = ctype
    return self
  elseif tree.struct or tree.union then
    local self = mktab(env, tree, {}, D)
    local v = tree.iden.value
    local s = Struct(env, tree, v)
    self.struct = s
    self.id = v
    return self
  elseif tree.tdef then
    local v = tree.tdef.value
    local self = mktab(env, tree, tassert(tree.tdef, env:type_get_r(v),
      "could not find already defined type '%s'", v), D)
    self.tdef = true
    self.id = v
    self.ctype = Type(env, tree.ctype)
    return self
  elseif tree.enum then
    local self = mktab(env, tree, {}, D)
    local v = tree.enum.value
    self.enum = Enum(env, tree)
    env:enum_reg(v, self.enum)
    return self
  else
    dump(tree, true)
    tassert(tree._m, false, "AST/Block not reached")
  end

end)

