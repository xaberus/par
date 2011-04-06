Environment = Class("Environment", {
  cpr = function(self, wt, indent)
    wt:add(indent, "<env>", "\n")
  end,
},
function(E, self)
  --idump(self)
  for k, v in ipairs(self) do
    disown(v)
  end
  return self
end)

