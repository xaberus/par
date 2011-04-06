Enumerator = Class("Enumerator", {
  cpr = function(self, wt, indent)
    wt:add(self.cid or self.id)
    if self.value then
      wt:add(" = ")
      self.value:cpr(wt, indent)
    end
  end,
},
function(E, self)
  --idump(self)
  if self.value then
    disown(self.value)
  end
  return self
end)

Enum = Class("Enum", {
  cpr = function(self, wt, indent)
    wt:add("enum ", self.cid or self.id)
    if #self > 0 then
      local indent2 = indent .. "  "
      wt:add(" {\n")
      for k, v in ipairs(self) do
        wt:add(indent2)
        v:cpr(wt, indent2)
        wt:add(",\n")
      end
      wt:add(indent, "}")
    end
  end,
},
function(E, self)
  --idump(self)
  disown(self.ctype)
  for k, v in ipairs(self) do
    disown(v)
  end
  return self
end)
