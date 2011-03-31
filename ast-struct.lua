StructDeclarator = Class("StructDeclarator", {
  constructor = function(env, scope, struct, ctype, tree)
    --dump(tree, nil, nil, nil, "structdeclr")

    local self = {}
    if tree.width then
      local expr = Expression(env, tree.width)
      tassert(nil, expr:is_constant())
      self.width = expr
      tassert(nil, ctype.reg == "i")
      tassert(nil, not ctype.abs)
    end
    tassert(nil, scope)
    tassert(nil, not scope[tree.iden.value], "field %s already defined in this scope", tree.iden.value)

    self.id = tree.iden
    scope[self.id.value] = {self, struct}
    self.ctype = ctype

    return self
  end,
})

StructDeclaration = Class("StructDeclaration", {
  constructor = function(env, scope, struct, tree)
    --dump(tree, nil, nil, nil, "structdecl")

    local self = {}

    if tree.decl then
      local ctype = Type(env, tree.ctype)
      for k, v in ipairs(tree.decl) do
        self[#self+1] = StructDeclarator(env, scope, struct, ctype, v)
      end
      self.ctype = ctype
    elseif tree.anon then
      local chld = tassert(nil, tree.ctype.sqlist.spec[1])
      tassert(nil, chld.struct or chld.union)

      return {anon = Struct(env, chld, nil, tassert(nil, scope))}
    else
      dump(tree, true)
      tassert(nil, false)
    end

    return self
  end,
  repr = function(self, indent)
    if not self.anon then 
      local tab = {}
      for k, v in ipairs(self) do
        if v.width then
          tab[#tab+1] = v.id.value .. " : " .. v.width:repr(indent)
        else
          tab[#tab+1] = v.id.value
        end
      end

      if indent then
        return indent .. self.ctype:repr(indent) .. " " .. concat(tab, ", ") .. ";\n"
      else
        return self.ctype:repr(indent) .. " " .. concat(tab, ", ") .. ";"
      end
    else
      return indent .. self.anon:repr(indent) .. ";\n"
    end
  end,
})

Struct = Class("Struct", {
  constructor = function(env, tree, name, scope, id)
    --dump(tree, nil, nil, nil, "struct")
    if type(tree.struct) == "table" then
      local name = tree.struct.value
      local ref = tassert(tree.struct, env:struct_get_r(name),
        "undefined struct '%s'", name)
      return {ref = ref}
    else
      local self = {
        scope = scope or {[0] = 1},
      }

      if tree.union then
        self.union = true
      end

      if id then
        name = id.value
        self.id = id
      end

      if name then
        self.name = name
      else
        self.name = "anonymous" .. self.scope[0]
        self.scope[0] = self.scope[0] + 1
      end

      for k, v in ipairs(tree.decl) do
        self[#self+1] = StructDeclaration(env, self.scope, self, v)
      end

      --dump(self, true, nil, nil, "Struct")
      return self
    end
  end,
  repr = function(self, indent)
    local tab = {}

    if self.ref then
      local ref = self.ref.decl

      if ref.union then
        tab[#tab+1] = "union"
      else
        tab[#tab+1] = "struct"
      end

      tab[#tab+1] = " "
      tab[#tab+1] = ref.id.value
    else
      if self.union then
        tab[#tab+1] = "union"
      else
        tab[#tab+1] = "struct"
      end

      tab[#tab+1] = " "
      if self.id then
        tab[#tab+1] = self.id.value
        tab[#tab+1] = " "
      end

      tab[#tab+1] = "{\n"

      do
        local indent1 = indent .. "  "
        for k,v in ipairs(self) do
          tab[#tab+1] = v:repr(indent1)
        end
      end

      tab[#tab+1] = indent
      tab[#tab+1] = "}"

    end
    return concat(tab, "")
  end,
})
