StructDeclarator = Class("StructDeclarator", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(S, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

StructDeclaration = Class("StructDeclaration", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(S, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

Struct = Class("Struct", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(S, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

