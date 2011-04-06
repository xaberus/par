Block = Class("Block", {
  cpr = function(self, wt, indent)
    local indent2 = indent .. "  "
    wt:add("{\n")
    for k, v in ipairs(self) do
      v:cpr(wt, indent2)
    end
    wt:add(indent, "}")
  end,
},
function(B, self)
  --idump(self)
  disown(self.env)
  for k, v in ipairs(self) do
    disown(v)
  end
  return self
end)
