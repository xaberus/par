Parameter = Class("Parameter", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(P, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

Function = Class("Function", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(F, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

