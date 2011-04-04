Declarator = Class("Declarator", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(D, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

Declaration = Class("Declaration", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(D, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)
