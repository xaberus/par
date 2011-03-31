Namespace = Class("Namespace", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "stmt")
    local env = env:child("ns")
    local self = {}

    self.env = env

    self.id = tree.ns
    self.src = Source(env, tree.list)

    return self
  end,
  repr = function(self, indent)
    return indent .. "namespace " .. self.id.value .. " {\n" .. self.src:repr(indent .. "  ") .. indent .. "};\n"
  end,
})

