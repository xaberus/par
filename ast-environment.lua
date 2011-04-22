
local function edump(self, indent, nx)
  if indent then
    if nx then
      indent = indent .. "[1;33m │[0;m"
    else
      indent = indent .. "  "
    end
  end
  local function subdump(tab, indent)
    local indent2 = indent .. " "
    local todo = {}
    for k, v in ipairs(tab) do
      local arg, name = unpack(v)
      if arg then
        todo[#todo+1] = v
      end
    end

    for w, a in ipairs(todo) do
      local t, name = unpack(a)
      if next(todo, w) then
        print(format("%s├┬ [1;34m%s[0;m", indent2, name))
      else
        print(format("%s╰┬ [1;34m%s[0;m", indent2, name))
      end
      for k, v in pairs(t) do
        local indent5
        if next(t, k) then
          if next(todo, w) then
            indent5 = indent2 .. "│├─"
          else
            indent5 = indent2 .. " ├─"
          end
        else
          if next(todo, w) then
            indent5 = indent2 .. "│╰─"
          else
            indent5 = indent2 .. " ╰─"
          end
        end
        local repr
        if v.repr then
          repr = v:repr()
        else
          repr = "<repr>" .. v["@tag"]
        end
        print(format("%s %s = %s", indent5, k, repr))
      end
    end
  end
  local function etout(env, key, indent, nx)
    local extra = ""
    local tab = {
      -- [[
      {env.syms, "symbols"};
      {env.types, "types"};
      {env.enums, "enums"};
      {env.structs, "structs"};
      {env.ifaces, "ifaces"};
      {env.labels, "labels"};
      --]]
    }
    local cnt = 0
    for k, v in ipairs(tab) do
      if v[1] then
       cnt = cnt+1
      end
    end
    if env.kind == "iface" then
      extra = extra .. format(" '%s'", env.id or "<id>")
      --[[for k, v in pairs(env) do
        extra = extra .. ", " .. tostring(k)
      end]]
    elseif env.kind == "ns" then
      extra = extra .. format(" '[1;34m%s[0;m'", env.pfx or "<pfx>")
    elseif env.kind == "param" then
      extra = extra .. format(" '[1;34m%s[0;m'", env.id or "<param>")
    end
--    local obs = "┼"
    local obs
    if nx then
      if #env > 0 then
        obs = "├─┯"
      else
        obs = "├─━"
      end
    else
      if #env > 0 then
        obs = "╰─┯"
      else
        obs = "╰─━"
      end
    end
    if key == "root" then
      obs = "╭"
    end
    local ibs = "[1;33m─╮[0;m"
    if cnt == 0 then
      ibs = ""
    elseif key == "root" then
      ibs = "[1;33m━╮[0;m"
    end
    if env._line then
      print(format("%s[1;33m%s[0;m%s %s [1;33m%s[0;m:[1;36m%s[0;m%s @ %d",
        indent, obs, ibs, key, env["@tag"], env.kind, extra, env._line))
    else
      print(format("%s[1;33m%s[0;m%s %s [1;33m%s[0;m:[1;36m%s[0;m%s",
        indent, obs, ibs, key, env["@tag"], env.kind, extra))
    end
    if obs == "├─┯" then
      subdump(tab, indent .. "[1;33m│ │[0;m")
    elseif obs == "╰─┯" then
      subdump(tab, indent .. "[1;33m  │[0;m")
    elseif obs == "├─━" then
      subdump(tab, indent .. "[1;33m│  [0;m")
    elseif obs == "╰─━" then
      subdump(tab, indent .. "   ")
    elseif obs == "╭" then
      subdump(tab, indent .. "[1;33m│[0;m")
    else
      subdump(tab, indent .. "  ╋" .. obs)
    end
  end

  local idnt
  if not indent then
    print("[1;31m┏━━━━━╾───────────────[0;m")
    etout(self, "root", "[1;31m┃[0;m ", self[1])
    idnt = "[1;31m┃[0;m"
  else
    idnt = indent .. ""
  end
  for k, v in ipairs(self) do -- only array part
    local t = type(v)
    if t == "string" then
      print(format("%s %s = '[1;34m%s[0;m'", idnt, tostring(k), v))
    elseif t == "table" then
      if v["@tag"] == "Environment" then
        local nx = self[k+1]
        local opt = ""
        etout(v, k, idnt .. " ", nx)
        edump(v, idnt, nx)
      end
    else
      print(format("%s %s = '[1;32m%s[0;m'", idnt, k, tostring(v)))
    end
  end
  if not indent then
    print("[1;31m┗━━━━━╾───────────────[0;m")
  end
end

Environment = Class("Environment", {
  child = function(self, kind, m)
    local e = setmetatable({}, getmetatable(self))
    e.parent = self
    e.kind = tassert(nil, kind, "environment without type")
    e.loc = self.loc
    if m then
      e._line = m._line
    end
    self[#self+1] = e
    return e
  end,

  sym_reg = function(self, id, sym)
    if not self.syms then
      self.syms = {}
    else
      if self.syms[id] then
        print(traceback())
      end
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
      ns.pfx = id .. "_"
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
        --e:edump()
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
      return id
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
      return id
    end
    print(traceback())
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
      return id
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
      return id
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
      return id
    end
  end,
  
  edump = edump
},
function(E)
  local env = setmetatable({
    kind = "global";
    loc = {};
  }, E)
  return env
end)

