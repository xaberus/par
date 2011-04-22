--[[Method = Class("Method", {
  prototype = function(self, wt, indent)
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
      self.cself.ctype:cpr(wt, indent, self)
      wt:add(" ")
      self.cself:cpr(wt, indent, self)
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
    wt:add(";\n")
  end
},
function(M, self)
  --idump(self)
  disown(self.rctype)
  disown(self.ctype)
  disown(self.cself)
  for k, v in ipairs(self.params) do
    disown(v)
  end
  if self.block then
    disown(self.block)
  end
  return self
end)]]

Interface = Class("Interface", {
  cpr = function(self, wt, indent)
    local indent2 = indent .. "  "

    wt:add(indent, "/* start interface ", self.cid or self.id, " */\n")
    wt:add(indent2)
    self.struct:cpr(wt, indent2)
    wt:add(";\n")
    --self:structify(wt, indent2, self.cid or self.id) -- emit full prototype
    for k, v in ipairs(self.methods) do
      v:cpr(wt, indent2)
    end
    wt:add(indent, "/* end interface ", self.cid or self.id, " */\n")
  end,
},
function(I, self)
  --idump(self)

  for k, v in ipairs(self) do
    disown(v)
  end

  disown(self.ctype)
  disown(self.env)
  disown(self.struct)

  for k, v in ipairs(self.methods) do
    disown(v)
  end

  return self
end)
