Namespace = Class("Namespace", {
  cpr = function(self, wt, indent)
    wt:add(indent, "/* start namespace ", self.id, " : ", self.env.pfx, " */\n")
    self.src:cpr(wt, indent .. "  ")
    wt:add(indent, "/* end namespace ", self.id, " */\n")
  end,
},
function(N, self)
  --idump(self)
  disown(self.env)
  disown(self.src)
  return self
end)

