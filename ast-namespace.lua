Namespace = Class("Namespace", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "stmt")

    local self = {}
    local env = env:ns_reg(tree.ns.value, self)

    self.id = tree.ns
    self.src = Source(env, tree.list)

    return self
  end,
  repr = function(self, indent)
    return indent .. "namespace " .. self.id.value .. " {\n" .. self.src:repr(indent .. "  ") .. indent .. "};\n"
  end,
})

