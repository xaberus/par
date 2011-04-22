Interface = Class("Interface", {
  repr = function(self, indent)
    return "<iface>"
  end,
  dereference = function(self)
    return self
  end,
  get_field = function(self, fid)
    local e = self.scope[fid]
    if e then return
      e.field
    end
  end,

  set_struct = function(self, env, struct, tree)
    struct.id = tassert(nil, self.id, "AST/Interface no envelope for interface struct")
    struct.cid = tassert(nil, self.cid, "AST/Function no c id")
    struct.anon = nil
    self.struct = struct
    self.scope = struct.scope
  end,
  set_defs = function(self, env, tree)
    local m = {}
    self.methods = m
    for k, v in ipairs(tree) do
       m[#m+1] = v
    end
  end,
},
function(I, env, tree, id)
  --dump(tree)

  local self = mktab(env, tree, {id = id}, I)
  return self
end)
