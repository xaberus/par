Statement = Class("Statement", {
  cpr = function(self, wt, indent)
    --wt:add(indent)
    local kind = self.kind
    local function dostmt(wt, dstmt, indent)
      local k = dstmt.kind
      if k ~= "block" and k~= "empty" then
        wt:add("\n")
        dstmt:cpr(wt, indent .. "  ")
        return false
      else
        if k == "empty" then
          wt:add(";")
          wt:add("\n")
          return false
        elseif k == "block" then
          wt:add(" ")
          dstmt.block:cpr(wt, indent)
          wt:add("\n")
          return true
        end
      end
    end
    --dump(self, true)
    if kind == "if" then
      wt:add(indent, "if (")
      self.expr:cpr(wt, indent)
      wt:add(")")
      dostmt(wt, self.dstmt, indent)

    elseif kind == "ifelse" then
      local cl = self
      local k
      while cl do
        if not k then
          wt:add(indent)
        else
          wt:add(" ")
        end
        wt:add("if (")
        cl.expr:cpr(wt, indent)
        wt:add(")")
        if dostmt(wt, cl.dstmt, indent) then
          wt:pop()
          wt:add(" ")
        else
          wt:add(indent)
        end
        wt:add("else")
        k = cl.estmt.kind
        if k == "if" then
          local ie = cl.estmt
          wt:add(" if (")
          ie.expr:cpr(wt, indent)
          wt:add(")")
          dostmt(wt, ie.dstmt, indent)
          break;
        elseif k == "ifelse" then
          cl = cl.estmt
        else
          dostmt(wt, cl.estmt, indent)
          break;
        end
      end

    elseif kind == "expr" then
      wt:add(indent)
      self.expr:cpr(wt, indent)
      wt:add(";\n")
    elseif kind == "do" then
      wt:add(indent, "do")
      dostmt(wt, self.dstmt, indent)
      wt:pop() -- strip newline
      wt:add(" while (")
      self.expr:cr(wt, indent)
      wt:add(");\n")
    elseif kind == "for" then
      wt:add(indent, "for (")
      if self.decl then
        self.decl:cpr(wt, indent, "")
        wt:pop() -- strip newline
      else
        if self.start then
          self.start:cpr(wt, indent)
        end
        wt:add(";")
      end
      wt:add(" ")
      self.cond:cpr(wt, indent)
      wt:add(";")
      if self.run then
        wt:add(" ")
        self.run:cpr(wt, indent)
      end
      wt:add(")")
      dostmt(wt, self.dstmt, indent)
    elseif kind == "while" then
      wt:add(indent, "while (")
      self.expr:cpr(wt, indent)
      wt:add(")")
      dostmt(wt, self.dstmt, indent)
    elseif kind == "block" then
      wt:add(indent)
      self.block:cpr(wt, indent)
      wt:add("\n")
    elseif kind == "return" then
      wt:add(indent, "return")
      if self.expr then
        wt:add(" ")
        self.expr:cpr(wt, indent)
      end
      wt:add(";\n")
    elseif kind == "break" then
      wt:add(indent, "break;\n")
    elseif kind == "cont" then
      wt:add(indent, "continue;\n")
    elseif kind == "label" then
      wt:add(indent, self.id, ":\n")
    elseif kind == "goto" then
      wt:add(indent, "goto ", self.id, ";\n")
    elseif kind == "switch" then
      wt:add(indent, "switch (")
      self.expr:cpr(wt, indent)
      wt:add(") {\n")
      --dostmt(tab, self.dstmt, indent)
      local indent2 = indent .. "  "
      local indent4 = indent2 .. "  "
      for k, v in ipairs(self.dstmt.block) do
        if v.kind == "case" or v.kind == "default" then
          v:cpr(wt, indent2)
        else
          v:cpr(wt, indent4)
        end
      end
      wt:add(indent, "}\n")
    elseif kind == "default" then
      wt:add(indent, "default:\n")
    elseif kind == "case" then
      wt:add(indent, "case ")
      self.expr:cpr(wt, indent)
      wt:add(":\n")
    end
  end,
},
function(S, self)
  --idump(self)
  -- if / if else / return
  if self.expr then disown(self.expr) end
  if self.dstmt then disown(self.dstmt) end
  if self.estmt then disown(self.estmt) end
  -- for
  if self.start then disown(self.start) end
  if self.cond then disown(self.cond) end
  if self.run then disown(self.run) end
  if self.decl then disown(self.decl) end
  -- {}
  if self.block then disown(self.block) end
  -- break
  if self.env then disown(self.env) end
  -- label / goto
  if self.ref then disown(self.ref.env) end
  return self
end)

