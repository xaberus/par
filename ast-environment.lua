Environment = Class("Environment", {
  constructor = function()
    return {
    }
  end,

  child = function(self, kind)
    local e = Environment()
    e.parent = self
    e.kind = kind or "env"
    self[#self+1] = e
    return e
  end,

  sym_reg = function(self, id, sym)
    if not self.syms then
      self.syms = {}
    else
      tassert(nil, not self.syms[id], "symbol redefined in same scope")
    end

    local s = {sym = sym}
    self.syms[id] = s
    return sym
  end,
  sym_get_r = function(self, id)
    local s = self.syms and self.syms[id] or nil

    if not s and self.parent then
      return self.parent:sym_get_r(id)
    end

    if s then
      return s.sym
    end
  end,
  sym_get = function(self, id)
    if not self.syms then return nil end
    local s = self.syms[id]
    if s then
      return s.sym
    end
  end,



  struct_reg = function(self, id, decl)
    if not self.structs then
      self.structs = {}
    else
      tassert(nil, not self.structs[id], "struct redefined in same scope")
    end

    local d = {decl = decl}
    self.structs[id] = d
    return decl
  end,
  struct_get_r = function(self, id)
    local s = self.structs and self.structs[id] or nil

    if not s and self.parent then
      return self.parent:struct_get_r(id)
    end

    -- TODO remove cruft
    return s
  end,
  struct_get = function(self, id)
    if not self.structs then return nil end
    return self.structs[id]
  end,


  enum_reg = function(self, id, decl)
    if not self.enums then
      self.enums = {}
    else
      tassert(nil, not self.enums[id], "enum redefined in same scope")
    end

    local d = {decl = decl}
    self.enums[id] = d
    return decl
  end,
  enum_get_r = function(self, id)
    local e = self.enums and self.enums[id] or nil

    if not e and self.parent then
      return self.parent:enum_get_r(id)
    end

    if e then
      return e.decl
    end
  end,
  enum_get = function(self, id)
    if not self.enums then return nil end
    local e = self.enums[id]
    if e then
      return e.decl
    end
  end,


  type_reg = function(self, id, t)
    if not self.types then
      self.types = {}
    else
      tassert(nil, not self.types[id], "type redefined in same scope")
    end
    local t = {t = t}
    self.types[id] = t
    return t
  end,

  type_get_r = function(self, id)
    local t = self.types and self.types[id] or nil

    if not t and self.parent then
      return self.parent:type_get_r(id)
    end

    if t then
      return t.t
    end
  end,

  label_reg = function(self, id, l)
    if not self.labels then
      self.labels = {}
    else
      tassert(nil, not self.labels[id], "label redefined in same scope")
    end
    local l = {l = l}
    self.labels[id] = l
    return l
  end,

  label_get = function(self, id)
    if not self.labels then return nil end
    return self.labels[id]
  end,
  label_get_r = function(self, id)
    local l = self.labels and self.labels[id] or nil

    if not l and self.parent then
      local k = self.parent.kind
      if k == "block" or k == "loop" then
        return self.parent:label_get_r(id)
      end
    end

    return l
  end,

  ns_reg = function(self, id)
    if not self.nss then
      self.nss = {}
    end

    local ns = self.nss[id]
    if self.nss[id] then
      return ns.env
    else
      local ns = {env = self:child("ns")}
      self.nss[id] = ns
      return ns.env
    end
  end,

  ns_get_r = function(self, id)
    local ns = self.nss and self.nss[id] or nil

    if not ns and self.parent then
      return self.parent:ns_get_r(id)
    end

    return ns.env
  end,


})

