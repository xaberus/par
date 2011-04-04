Designation = Class("Designation", {
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

Initializer = Class("Initializer", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(I, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

