Source = Class("Source", {
  cpr = function(self, indent)
    local tab = {}
    dump(self)
    return concat(tab, "")
  end,
},
function(S, ast)
  local self = disown(ast)
  for k, v in pairs(self) do
    self[k] = disown(v)
  end
  return self
end)
