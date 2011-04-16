Environment = Class("Environment", {
  child = function(self, kind)
    local e = setmetatable({}, getmetatable(self))
    e.parent = self
    e.kind = tassert(nil, kind, "environment without type")
    e.loc = self.loc
    self[#self+1] = e
    return e
  end,

  sym_reg = function(self, id, sym)
    if not self.syms then
      self.syms = {}
    else
      tassert(self.loc[sym], not self.syms[id], "symbol '%s' redefined in same scope", id)
    end

    self.syms[id] = sym
    return sym
  end,
  sym_get_r = function(self, id)
    local s = self.syms and self.syms[id] or nil

    if not s and self.parent then
      return self.parent:sym_get_r(id)
    end

    return s, self
  end,
  sym_get = function(self, id)
    if not self.syms then return nil end
    local s = self.syms[id]
    return s
  end,



  struct_reg = function(self, id, decl)
    if not self.structs then
      self.structs = {}
    else
      tassert(nil, not self.structs[id], "struct redefined in same scope")
    end
    tassert(self.loc[decl], not self:iface_get(id), "struct/interface namespace clash for '%s'", id)

    self.structs[id] = decl
    return decl
  end,
  struct_get_r = function(self, id)
    local s = self.structs and self.structs[id] or nil

    if not s and self.parent then
      return self.parent:struct_get_r(id)
    end

    -- TODO remove cruft
    return s, self
  end,
  struct_get = function(self, id)
    if not self.structs then return nil end
    local s = self.structs[id]
    return s
  end,


  enum_reg = function(self, id, decl)
    if not self.enums then
      self.enums = {}
    else
      tassert(nil, not self.enums[id], "enum redefined in same scope")
    end

    self.enums[id] = decl
    return decl
  end,
  enum_get_r = function(self, id)
    local e = self.enums and self.enums[id] or nil

    if not e and self.parent then
      return self.parent:enum_get_r(id)
    end

    return e, self
  end,
  enum_get = function(self, id)
    if not self.enums then return nil end
    local e = self.enums[id]
    return e
  end,


  type_reg = function(self, id, t)
    if not self.types then
      self.types = {}
    else
      tassert(nil, not self.types[id], "type redefined in same scope")
    end
    self.types[id] = t
    return t
  end,

  type_get_r = function(self, id)
    local t = self.types and self.types[id] or nil

    if not t and self.parent then
      return self.parent:type_get_r(id)
    end

    if t then
      return t, self
    end
  end,

  label_reg = function(self, id, l)
    if not self.labels then
      self.labels = {}
    else
      tassert(nil, not self.labels[id], "label redefined in same scope")
    end
    self.labels[id] = l
    return l
  end,

  label_get = function(self, id)
    if not self.labels then return nil end
    local l = self.labels[id]
    return l
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
      return ns
    else
      local ns = self:child("ns")
      self.nss[id] = ns
      return ns
    end
  end,

  ns_get_r = function(self, id)
    local ns = self.nss and self.nss[id] or nil

    if not ns and self.parent then
      return self.parent:ns_get_r(id)
    end

    return ns
  end,

  iface_reg = function(self, id, i)
    if not self.ifaces then
      self.ifaces = {}
    else
      tassert(nil, not self.ifaces[id], "interface redefined in same scope")
    end
    tassert(self.loc[i], not self:struct_get(id), "struct/interface namespace clash for '%s'", id)
    self.ifaces[id] = i
    return i
  end,

  iface_get_r = function(self, id)
    local i = self.ifaces and self.ifaces[id] or nil

    if not i and self.parent then
      return self.parent:iface_get_r(id)
    end

    if i then
      return i, self
    end
  end,
  iface_get = function(self, id)
    if not self.ifaces then return nil end
    local i = self.ifaces[id]
    return i
  end,



---------------------------------

  ns_get_pfxs = function(self)
    local function _walk(e, pfxs)
      while e do
        if e.kind == "ns" then
          insert(pfxs, 1, e.pfx)
        elseif e.kind == "iface" then
          insert(pfxs, 1, e.pfx)
        end
        e = e.parent
      end
    end
    pfxs = {}
    _walk(self, pfxs)
    return pfxs
  end,

  ns_get_sym = function(self, id)
    local sym, env = self:sym_get_r(id)
    if sym then
      if env.kind == "global" or env.kind == "ns" or env.kind == "iface" then
        local p = env:ns_get_pfxs()
        if #p > 0 then
          return concat(p, "") .. id
        end
      end
    end
  end,
  ns_get_type = function(self, id)
    local sym, env = self:type_get_r(id)
    if sym then
      if env.kind == "global" or env.kind == "ns" or env.kind == "iface" then
        local p = env:ns_get_pfxs()
        if #p > 0 then
          return concat(p, "") .. id
        end
      end
    end
  end,
  ns_get_struct = function(self, id)
    local sym, env = self:struct_get_r(id)
    if sym then
      if env.kind == "global" or env.kind == "ns" or env.kind == "iface" then
        local p = env:ns_get_pfxs()
        if #p > 0 then
          return concat(p, "") .. id
        end
      end
    end
  end,
  ns_get_enum = function(self, id)
    local sym, env = self:enum_get_r(id)
    if sym then
      if env.kind == "global" or env.kind == "ns" or env.kind == "iface" then
        local p = env:ns_get_pfxs()
        if #p > 0 then
          return concat(p, "") .. id
        end
      end
    end
  end,
  ns_get_iface = function(self, id)
    local sym, env = self:iface_get_r(id)
    if sym then
      if env.kind == "global" or env.kind == "ns" or env.kind == "iface" then
        local p = env:ns_get_pfxs()
        if #p > 0 then
          return concat(p, "") .. id
        end
      end
    end
  end,
},
function(E)
  local env = setmetatable({
    kind = "global";
    loc = {};
  }, E)
  return env
end)

