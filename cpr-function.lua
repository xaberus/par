Parameter = Class("Parameter", {
  cpr = function(self, wt, indent)
    self.ctype:cpr(wt, indent, self)
    --wt:add(indent)
  end,
},
function(P, self)
  --idump(self)
  disown(self.ctype)
  return self
end)

Function = Class("Function", {
  cpr = function(self, wt, indent)
    --wt:add(indent, "/* Function */", "\n")
    wt:add(indent)

    if self.rctype.reg ~= "c" then
      if self.spec then
        for k, v in ipairs(self.spec) do
          if k > 1 then
            wt:add(" ", v)
          else
            wt:add(v)
          end
        end
        wt:add(" ")
      end

      self.rctype:cpr(wt, indent, self.rctype)

      wt:add(" ", self.cid or self.id, "(")
      for k, v in ipairs(self.params) do
        if k > 1 then
            wt:add(", ")
        end
        v.ctype:cpr(wt, indent, self)
        wt:add(" ")
        v:cpr(wt, indent, self)
      end
      if self.vararg then
        wt:add(",", "...")
      end
      wt:add(")")
    else
      self.ctype:cpr(wt, indent, self)
    end

    if self.block and #self.block then
      wt:add("\n")
      wt:add(indent)
      self.block:cpr(wt, indent)
      wt:add("\n")
    elseif self.forward then
      wt:add(";\n")
    end
  end,
},
function(F, self)
  --idump(self)
  disown(self.rctype)
  disown(self.ctype)
  for k, v in ipairs(self.params) do
    disown(v)
  end
  if self.block then
    disown(self.block)
  end
  return self
end)

