
Block = Class("Block", {
  add_decl = function(self, decl)
    self[#self+1] = decl
  end,
  add_stmt = function(self, decl)
    self[#self+1] = decl
  end,
  repr = function(self, indent)
    local tab = {}
    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent .. "  ")
    end
    return "{\n" .. concat(tab, "") .. indent .. "}"
  end,
  check_labels = function(self)
    local env = self.env
    for k, lbl in ipairs(self.labels) do
      if not lbl.ref then
        env:label_reg(lbl.id, lbl)
      end
    end
    for k, v in ipairs(self) do
      local tag = v["@tag"]
      if tag == "Statement" then
        v:check_labels(env);
      end
    end
  end,
},
function(B, env, tree)
  --dump(tree, nil, nil, nil, "block")
  local self = mktab(env, tree, {env = env}, B)

  local function get_labels(tree)
    local labels = {}
    for k, v in ipairs(tree) do
      if v.stmt then
        v.stmt:get_labels(labels);
      end
    end
    return labels
  end

  self.labels = get_labels(tree, labels)

  for k, v in ipairs(tree) do
    if v.decl then
      self:add_decl(v.decl)
    elseif v.stmt then
      self:add_stmt(v.stmt)
    else
      dump(v, true)
      tassert(env.loc[self], false, "AST/Block not reached")
    end
  end

  return self
end)

