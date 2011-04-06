StructDeclarator = Class("StructDeclarator", {
  cpr = function(self, wt, indent)
    --wt:add(indent)
    self.ctype:cpr(wt, indent, self)
    if self.width then
      wt:add(" : ")
      self.width:cpr(wt, indent)
    end
  end,
},
function(S, self)
  --idump(self)
  if self.width then
    disown(self.width)
  end
  disown(self.ctype)
  return self
end)

StructDeclaration = Class("StructDeclaration", {
  cpr = function(self, wt, indent)
    if not self.anon then 
      wt:add(indent)
      self.ctype:cpr(wt, indent, self)
      wt:add(" ")
      for k, v in ipairs(self) do
        if k > 1 then
          wt:add(", ")
        end
        v:cpr(wt, indent)
      end
      wt:add(";\n")
    else
      wt:add(indent)
      self.anon:cpr(wt, indent)
      wt:add(";\n")
    end
  end,
},
function(S, self)
  --idump(self)
  for k, v in ipairs(self) do
    disown(v)
  end
  if self.anon then
    disown(self.anon)
  end
  --disown(self.ctype)
  return self
end)

Struct = Class("Struct", {
  cpr = function(self, wt, indent)
    if self.ref then
      local ref = self.ref

      if ref.union then
        wt:add("union ", ref.cid or ref.id)
      else
        wt:add("struct ", ref.cid or ref.id)
      end
    else
      if self.union then
        wt:add("union ")
      else
        wt:add("struct ")
      end

      if not self.anon then
        wt:add(self.cid or self.id)
        wt:add(" ")
      end

      wt:add("{\n")
      do
        local indent2 = indent .. "  "
        for k, v in ipairs(self) do
          v:cpr(wt, indent2)
        end
      end
      wt:add(indent, "}")
    end
  end,
},
function(S, self)
  --idump(self)
  for k, v in ipairs(self) do
    disown(v)
  end
  return self
end)

