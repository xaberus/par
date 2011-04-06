Expression = Class("Expression", {
  cprh = {
    ["eq"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" == ")
      b:cpr(wt, indent)
    end,
    ["shr"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" >> ")
      b:cpr(wt, indent)
    end,
    ["deref"] = function(self, op, a, b, c, wt, indent)
      wt:add("*")
      a:cpr(wt, indent)
    end,
    ["cast"] = function(self, op, a, b, c, wt, indent)
      wt:add("(")
      b:cpr(wt, indent, self)
      wt:add(") ")
      a:cpr(wt, indent)
    end,
    ["ls"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" < ")
      b:cpr(wt, indent)
    end,
    ["land"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" && ")
      b:cpr(wt, indent)
    end,
    ["sizeof"] = function(self, op, a, b, c, wt, indent)
      if a.op and a.op == "par" then
        wt:add("sizeof")
        a:cpr(wt, indent)
      else
        wt:add("sizeof ")
        a:cpr(wt, indent)
      end
    end,
    ["fsizeof"] = function(self, op, a, b, c, wt, indent)
      wt:add("sizeof(")
      a:cpr(wt, indent, self)
      wt:add(")")
    end,
    ["ne"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" != ")
      b:cpr(wt, indent)
    end,
    ["udec"] = function(self, op, a, b, c, wt, indent)
      wt:add("--")
      a:cpr(wt, indent)
    end,
    ["shl"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" << ")
      b:cpr(wt, indent)
    end,
    ["div"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" / ")
      b:cpr(wt, indent)
    end,
    ["ge"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" >= ")
      b:cpr(wt, indent)
    end,
    ["unp"] = function(self, op, a, b, c, wt, indent)
      wt:add("+")
      a:cpr(wt, indent)
    end,
    ["mul"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" * ")
      b:cpr(wt, indent)
    end,
    ["bor_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" |= ")
      b:cpr(wt, indent)
    end,
    ["band"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" & ")
      b:cpr(wt, indent)
    end,
    ["cond"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" ? ")
      b:cpr(wt, indent)
      wt:add(" : ")
      c:cpr(wt, indent)
    end,
    ["inc"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add("++")
    end,
    ["band_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" &= ")
      b:cpr(wt, indent)
    end,
    ["gt"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" > ")
      b:cpr(wt, indent)
    end,
    ["shr_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" >>= ")
      b:cpr(wt, indent)
    end,
    ["shl_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" <<= ")
      b:cpr(wt, indent)
    end,
    ["bnot"] = function(self, op, a, b, c, wt, indent)
      wt:add("~")
      a:cpr(wt, indent)
    end,
    ["mod_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" %= ")
      b:cpr(wt, indent)
    end,
    ["addr"] = function(self, op, a, b, c, wt, indent)
      wt:add("&")
      a:cpr(wt, indent)
    end,
    ["bxor_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" ^= ")
      b:cpr(wt, indent)
    end,
    ["lnot"] = function(self, op, a, b, c, wt, indent)
      wt:add("!")
      a:cpr(wt, indent)
    end,
    ["mul_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" *= ")
      b:cpr(wt, indent)
    end,
    ["par"] = function(self, op, a, b, c, wt, indent)
      wt:add("(")
      a:cpr(wt, indent)
      wt:add(")")
    end,
    ["mod"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" % ")
      b:cpr(wt, indent)
    end,
    ["unm"] = function(self, op, a, b, c, wt, indent)
      wt:add("-")
      a:cpr(wt, indent)
    end,
    ["sub"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" - ")
      b:cpr(wt, indent)
    end,
    ["div_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" /= ")
      b:cpr(wt, indent)
    end,
    ["asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" = ")
      b:cpr(wt, indent)
    end,
    ["lor"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" || ")
      b:cpr(wt, indent)
    end,
    ["bor"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" | ")
      b:cpr(wt, indent)
    end,
    ["sub_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" -= ")
      b:cpr(wt, indent)
    end,
    ["le"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" <= ")
      b:cpr(wt, indent)
    end,
    ["bxor"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" ^ ")
      b:cpr(wt, indent)
    end,
    ["acc"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      if self.a.ctype.pointer then
        wt:add("->")
      else
        wt:add(".")
      end
      wt:add(b)
    end,
    ["dec"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add("--")
    end,
    ["call"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add("(")
      b:cpr(wt, indent)
      wt:add(")")
    end,
    ["add_asgn"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" += ")
      b:cpr(wt, indent)
    end,
    ["index"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add("[")
      b:cpr(wt, indent)
      wt:add("]")
    end,
    ["add"] = function(self, op, a, b, c, wt, indent)
      a:cpr(wt, indent)
      wt:add(" + ")
      b:cpr(wt, indent)
    end,
  },

  cpr = function(self, wt, indent)
    --wt:add("<expr>")
    if self.identifier then
      wt:add(self.cid or self.identifier)
    elseif self.constant then
      wt:add(self.constant)
    elseif self.list then
        for k, v in ipairs(self.list) do
          if k > 1 then
            wt:add(", ")
          end
          v:cpr(wt, indent)
        end
    elseif self.op then
      local op = self.op
      local a = self.a
      local b = self.b
      local c = self.c
      return self.cprh[self.op](self, op, a, b, c, wt, indent)
    end
  end,
},
function(E, self)
  --idump(self)
  if self.list then
    for k, v in ipairs(self.list) do
      disown(v)
    end
  else
    if self.identifier then
      disown(self.ref)
    elseif self.op then
      local o = {self.a, self.b, self.c}
      for k, v in ipairs(o) do
        if type(v) == "table" then
          disown(v)
        end
      end
    end
    disown(self.ctype)
  end
  return self
end)

