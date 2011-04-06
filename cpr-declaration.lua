Declarator = Class("Declarator", {
  cpr = function(self, wt, indent)
    --wt:add(indent, "/* Declarator */", "\n")
    self.ctype:cpr(wt, indent, self)
    if self.init then
      wt:add(" = ") self.init:cpr(wt, indent)
    end
  end,
},
function(D, self)
  --idump(self)
  disown(self.ctype)
  if self.init then disown(self.init) end
  return self
end)

Declaration = Class("Declaration", {
  cpr = function(self, wt, indent, inf)
    --wt:add(indent, "/* Declaration */", "\n")
    if self.pure then
      wt:add(inf or indent)
      if self.ctype.reg ~= "c" then
        self.ctype:cpr(wt, indent, self)
        wt:add(" ")
        for k, v in ipairs(self) do
          if k> 1 then
            wt:add(", ")
          end
          v:cpr(wt, indent)
        end
        wt:add(";")
        wt:add("\n") --strippable newline
      else
        for k, v in ipairs(self) do
          v:cpr(wt, indent)
          wt:add(";")
          wt:add("\n") --strippable newline
        end
      end
    elseif self.enum then
      wt:add(inf or indent)
      self.enum:cpr(wt, indent)
      wt:add(";")
      wt:add("\n") --strippable newline
    elseif self.struct then
      wt:add(inf or indent)
      self.struct:cpr(wt, indent)
      wt:add(";")
      wt:add("\n") --strippable newline
    elseif self.tdef then
      wt:add(inf or indent)
      wt:add("typedef ")
      self.ctype:cpr(wt, indent, self)
      if self.ctype.reg ~= "c" then
        wt:add(" ")
        wt:add(self.cid or self.id)
      end
      wt:add(";")
      wt:add("\n") --strippable newline
    end
  end,
},
function(D, self)
  --idump(self)
  for k, v in ipairs(self) do
    if v.tag then
      disown(v)
    end
  end
  if self.ctype then disown(self.ctype) end
  if self.enum then disown(self.enum) end
  if self.struct then disown(self.struct) end
  return self
end)
