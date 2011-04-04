Namespace = Class("Namespace", {
  repr = function(self, indent)
    return indent .. "namespace " .. self.id .. " {\n" .. self.src:repr(indent .. "  ") .. indent .. "};\n"
  end,
},
function(N, env, tree)
  --dump(tree, nil, nil, nil, "stmt")

  local self = mktab(env, tree, {}, N)
  local v = tree.ns.value

  local env = env:ns_reg(v, self)

  self.id = v
  self.src = Source(env, tree.list)

  return self
end)

