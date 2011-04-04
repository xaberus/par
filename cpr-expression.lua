Expression = Class("Expression", {
  cpr = function(self, indent)
    local tab = {}
    return concat(tab, "")
  end,
},
function(E, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    if v.tag then
      self[k] = disown(v)
    end
  end
  return self
end)

