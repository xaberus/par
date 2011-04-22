StructDeclarator = Class("StructDeclarator", {},
function(S, env, ctype, tree)
  --dump(tree, nil, nil, nil, "structdeclr")

  local self = mktab(env, tree, {}, S)

  if tree.width then
    local expr = tree.width
    local loc = env.loc[expr]
    tassert(loc, expr:is_constant(), "not constant width of struct field")
    self.width = expr
    tassert(loc, ctype.reg == "x", "only integer widths allowed")
    tassert(loc, not ctype.abs, "only integer widths allowed")
  end
  local loc = env.loc[self]
  local v = tree.iden.value

  self.id = v

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
function(S, env, tree)
  --dump(tree, nil, nil, nil, "structdecl")

  if tree.decl then
    local self = mktab(env, tree, {}, S)
    local ctype = tree.ctype
    for k, v in ipairs(tree.decl) do
      local dr = StructDeclarator( env, ctype, v)
      self[#self+1] = dr
    end
    self.ctype = ctype
    return self
  elseif tree.anon then
    if tree.ctype then
      tassert(tree._m, tree.ctype.reg == "s", "AST/Struct, not a child")
      local self = mktab(env, tree, {
        anon = tree.ctype.struct
      }, S)
      return self
    elseif tree.struct then
      local self = mktab(env, tree, {
        anon = tree.struct
      }, S)
      return self
    end
  else
    dump(tree, true)
    tassert(tree._m, false, "AST/StructDeclaration not reached")
  end
end)

local function merge_scopes(self, scope)
  scope = scope or self.scope
  -- dump({self,scope})
  
  for k, decl in ipairs(self) do
    if decl.anon then
      local chld = decl.anon
      for id, f in pairs(chld.scope) do
        if type(id) == "string" then
          tassert(nil, not scope[id], "field %s already defined in this scope", id)
          scope[id] = f
        end
      end

      scope[0] = chld.scope[0] + scope[0]
      chld.scope = scope
      chld.id = "anonymous" .. (self.scope[0] - 1)
    end
  end
end

Struct = Class("Struct", {
  repr = function(self, indent)
    local tab = {}

    if self.ref then
      local ref = self.ref

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
      return self.ref
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
function(S, env, tree, id)
  --dump(tree, nil, nil, nil, "struct")
  if not tree then
    local fwd = setmetatable({empty = true}, S)
    tassert(nil, id, "no id in forward declaration")
    env:struct_reg(id, fwd)
    return fwd
  elseif type(tree.struct) == "table" then
    local id = tree.struct.value
    local ref = tassert(tree.struct, env:struct_get_r(id),
      "undefined struct '%s'", id)
    local self = mktab(env, tree, {ref = ref}, S)
    self.cid = tassert(nil, env:ns_get_struct(id), "AST/Struct no c id")
    return self
  else
    local self = env:struct_get(id)
    if self then
      tassert(nil, self.empty, "attempt to redeclare struct")
    else
      self = mktab(env, tree, {}, S)
      if id then
        env:struct_reg(id, self)
      end
    end

    self.empty = nil
    self.scope = {[0] = 1}

    if tree.union then
      self.union = true
    end

    if id then
      self.id = id
      self.cid = tassert(nil, env:ns_get_struct(id), "AST/Struct no c id")
    else
      self.anon = true
    end

    for k, decl in ipairs(tree.decl) do
      local scope = self.scope
      tassert(nil, scope, "AST/Struct no scope")
      if decl["@tag"] == "StructDeclaration" then
        for k, dr in ipairs(decl) do
          tassert(nil, not scope[dr.id], "field %s already defined in this scope", k)
          scope[dr.id] = {field = dr, struct = struct}
        end
        self[#self+1] = decl
      elseif decl.inherit then
        self[#self+1] = decl
      else 
        dump(tree, nil, nil, nil, "struct")
        tassert(nil, false, "AST/Struct NIY")
      end
    end
    
    merge_scopes(self)

    --dump(self, true, nil, nil, "Struct")
    return self
  end
end)
