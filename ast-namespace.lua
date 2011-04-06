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
  if tree.pfx then
    if env.pfx then
      tassert(tree._m, env.pfx == tree.pfx.value, "namespace declared with different prefix")
    else
      env.pfx = tree.pfx.value
    end
  else
    if not env.pfx then
      env.pfx = v .. "_"
    end
  end

  self.env = env

  self.src = Source(env, tree.list)

  return self
end)

