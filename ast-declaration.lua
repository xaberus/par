Declarator = Class("Declarator", {
  constructor = function(env, ctype, tree)
    local self = {}
    if tree.init then
      local init = Initializer(env, tree.init)
      self.init = init
    end
    self.id = tree.iden
    self.ctype = ctype

    env:sym_reg(self.id.value, self)

    return self
  end,
  get_type = function(self)
    return self.ctype
  end,
})

Declaration = Class("Declaration", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "decl")

    local self = {}

    if tree.pure then
      self.pure = true
      local ctype = Type(env, tree.ctype)
      for k, v in ipairs(tree.decl) do
        self[#self+1] = Declarator(env, ctype, v)
      end
      self.ctype = ctype
    elseif tree.struct or tree.union then
      env:struct_reg(tree.iden.value, {}) -- dummy
      local s = Struct(env, tree, nil, nil, tree.iden)
      local ref = env:struct_get(tree.iden.value).decl
      for k, v in pairs(s) do
        ref[k] = v
      end
      self.struct = ref
      self.id = tree.iden
      setmetatable(ref, Struct)
    elseif tree.tdef then
      self = tassert(tree.tdef, env:type_get_r(tree.tdef.value),
        "could not find already defined type '%s'", tree.tdef.value)
      self.tdef = true
      self.id = tree.tdef
      self.ctype = Type(env, tree.ctype)
    elseif tree.enum then
      self.enum = Enum(env, tree)
      env:enum_reg(tree.enum.value, self.enum)
    else
      dump(tree, true)
      tassert(nil, false)
    end

    return self
  end,
  repr = function(self, indent)
    if self.pure then -- plain declaration
      local tab = {}
      for k, v in ipairs(self) do
        if v.init then
          tab[#tab+1] = v.id.value .. " = " .. v.init:repr(indent)
        else
          tab[#tab+1] = v.id.value
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
      return indent .. "typedef " .. self.ctype:repr(indent) .. " " .. self.id.value .. ";\n"
    elseif self.enum then
      return indent .. self.enum:repr(indent) .. ";\n"
    end
  end,
})

