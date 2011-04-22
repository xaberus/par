Source = Class("Source", {
  repr = function(self, indent)
    local tab = {}

    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent)
    end

    return concat(tab, "\n")
  end,
},
function(S, env, tree)
  local self = mktab(env, tree, {}, S)
  for k, v in ipairs(tree) do
    if v["@tag"] == "Function" then
      self[#self+1] = v
    elseif v["@tag"] == "Declaration" then
      self[#self+1] = v
    elseif v["@tag"] == "Namespace" then
      self[#self+1] = v
    elseif v["@tag"] == "Interface" then
      self[#self+1] = v
    else
      dump(v, true)
      tassert(tree._m, false, "AST/Source NIY")
    end
  end
  self.env = env
  return self
end)
