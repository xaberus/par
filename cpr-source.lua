Source = Class("Source", {
  cpr = function(self, wt, indent)
    --wt:add(indent, "/* Source */", "\n")
    for k, v in ipairs(self) do
      v:cpr(wt, indent)
    end
  end,
},
function(S, self)
  --idump(self)
  disown(self.env)
  for k, v in ipairs(self) do
    disown(v)
  end
  return self
end)
