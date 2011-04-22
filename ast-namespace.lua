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
  tassert(tree._m, env.pfx, "namespace declared without prefix")

  self.env = env

  self.src = tree.src

  return self
end)

