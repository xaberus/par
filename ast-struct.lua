StructDeclarator = Class("StructDeclarator", {},
function(S, env, scope, struct, ctype, tree)
  --dump(tree, nil, nil, nil, "structdeclr")

  local self = mktab(env, tree, {}, S)

  if tree.width then
    local expr = Expression(env, tree.width)
    local loc = env.loc[expr]
    tassert(loc, expr:is_constant(), "not constant width of struct field")
    self.width = expr
    tassert(loc, ctype.reg == "x", "only integer widths allowed")
    tassert(loc, not ctype.abs, "only integer widths allowed")
  end
  local loc = env.loc[self]
  local v = tree.iden.value
  tassert(loc, scope, "AST/StructDeclarator no scope")
  tassert(tree.iden, not scope[v], "field %s already defined in this scope", v)

  self.id = v
  scope[v] = {field = self, struct = struct}
  self.ctype = ctype

  return self
end)

StructDeclaration = Class("StructDeclaration", {
  repr = function(self, indent)
    if not self.anon then 
      local tab = {}
      for k, v in ipairs(self) do
        if v.width then
          tab[#tab+1] = v.id .. " : " .. v.width:repr(indent)
        else
          tab[#tab+1] = v.id
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
},
function(S, env, scope, struct, tree)
  --dump(tree, nil, nil, nil, "structdecl")

  if tree.decl then
    local self = mktab(env, tree, {}, S)
    local ctype = Type(env, tree.ctype)
    for k, v in ipairs(tree.decl) do
      self[#self+1] = StructDeclarator(env, scope, struct, ctype, v)
    end
    self.ctype = ctype
    return self
  elseif tree.anon then
    local chld = tassert(tree._m, tree.ctype.sqlist.spec[1], "AST/Struct, not a child")
    tassert(tree._m, chld.struct or chld.union, "AST/Struct neither struct nor union")

    local self = mktab(env, tree, {anon = Struct(env, chld, nil, tassert(tree._m, scope, "AST/Struct no scope"))}, S)
    return self
  else
    dump(tree, true)
    tassert(tree._m, false, "AST/Struct not reached")
  end
end)

Struct = Class("Struct", {
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
      tab[#tab+1] = ref.id
    else
      if self.union then
        tab[#tab+1] = "union"
      else
        tab[#tab+1] = "struct"
      end

      tab[#tab+1] = " "
      if not self.anon then
        tab[#tab+1] = self.id
        tab[#tab+1] = " "
      end


      if indent then
        tab[#tab+1] = "{\n"

        do
          local indent1 = indent .. "  "
          for k,v in ipairs(self) do
            tab[#tab+1] = v:repr(indent1)
          end
        end

        tab[#tab+1] = indent
        tab[#tab+1] = "}"
      else
        -- not in source
      end
    end
    --dump(tab)
    return concat(tab, "")
  end,

  dereference = function(self)
    if self.ref then
      return self.ref.decl
    else
      return self
   end
  end,

  get_field = function(self, fid)
    local e = self.scope[fid]
    if e then return
      e.field
    end
  end,

},
function(S, env, tree, id, scope)
  --dump(tree, nil, nil, nil, "struct")
  if type(tree.struct) == "table" then
    local id = tree.struct.value
    local ref = tassert(tree.struct, env:struct_get_r(id),
      "undefined struct '%s'", id)
    local self = mktab(env, tree, {ref = ref}, S)
    return self
  else
    local self = mktab(env, tree, {scope = scope or {[0] = 1}}, S)
    if id then
      env:struct_reg(id, self)
    end

    if tree.union then
      self.union = true
    end

    if id then
      self.id = id
    else
      self.anon = true
      self.id = "anonymous" .. self.scope[0]
      self.scope[0] = self.scope[0] + 1
    end

    for k, v in ipairs(tree.decl) do
      self[#self+1] = StructDeclaration(env, self.scope, self, v)
    end

    --dump(self, true, nil, nil, "Struct")
    return self
  end
end)
