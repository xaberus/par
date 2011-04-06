Designation = Class("Designation", {
  cpr = function(self, wt, indent)
    for k, v in ipairs(self) do
      if k > 1 then
        wt:add(" = ")
      end
      if v.name then
        wt:add(".", v.name)
      elseif v.index then
        wt:add("[")
        v.index:cpr(wt, indent)
        wt:add("]")
      end
    end
  end,
},
function(D, self)
  for k, v in ipairs(self) do
    if v.index then
      disown(v.index)
    end
  end
  --idump(self)
  return self
end)

Initializer = Class("Initializer", {
  cpr = function(self, wt, indent)
    if self.expr then
      self.expr:cpr(wt, indent)
    elseif self.list then
      --wt:add("<init>")
      local indent2 = indent .. "  "
      wt:add("{\n")
      for k,v in ipairs(self.list) do
        wt:add(indent2)
        if v.desgn then
          v.desgn:cpr(wt, indent2)
          wt:add(" = ")
        end
        v.expr:cpr(wt, indent2)
        wt:add(",\n")
      end
      wt:add(indent, "}")
    end
  end,
},
function(I, self)
  --idump(self)
  if self.list then
    for k, v in ipairs(self.list) do
      disown(v.expr)
      disown(v.desgn)
    end
  else
    disown(self.expr)
  end
  return self
end)

