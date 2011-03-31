Statement = Class("Statement", {
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "stmt")
    local self = {}
    if tree.ifcl then
      self.expr = Expression(env, tree.ifcl)
      self.dstmt = Statement(env, tree.dstmt);
      self.opened = tree.opened
      if tree.estmt then
        self.kind = "ifelse"
        self.estmt = Statement(env, tree.estmt);
      else
        self.kind = "if"
      end
    elseif tree.docl then
      local env = env:child("loop")
      self.kind = "do"
      self.expr = Expression(env, tree.docl)
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.forcl then
      local env = env:child("loop")
      local m = tree.forcl
      self.kind = "for"
      if m == "ee" then
        self.start = Expression(env, tree.init)
        self.cond = Expression(env, tree.cond)
      elseif m == "eee" then
        if(tree.init) then
          self.start = Expression(env, tree.init)
        end
        self.cond = Expression(env, tree.cond)
        self.run = Expression(env, tree.incr)
      elseif m == "de" then
        self.decl = Declaration(env, tree.decl)
        self.cond = Expression(env, tree.cond)
      elseif m == "dee" then
        self.decl = Declaration(env, tree.decl)
        self.cond = Expression(env, tree.cond)
        self.run = Expression(env, tree.incr)
      end
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.whilecl then
      local env = env:child("loop")
      self.kind = "while"
      self.expr = Expression(env, tree.whilecl)
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.switchcl then
      local env = env:child("switch")
      self.kind = "switch"
      self.expr = Expression(env, tree.switchcl)
      self.dstmt = Statement(env, tree.dstmt)
    elseif tree.default then
      tassert(nil, 
        env.kind == "switch"
        or env.parent.kind == "switch")
      self.kind = "default"
    elseif tree.case then
      tassert(nil, 
        env.kind == "switch"
        or env.parent.kind == "switch")
      self.kind = "case"
      self.expr = Expression(env, tree.case)
    elseif tree.expr then
      self.kind = "expr"
      self.expr = Expression(env, tree.expr)
    elseif tree.empty then
      self.kind = "empty"
    elseif tree.compound then
      self.kind = "block"
      local env = env:child("block")
      self.block = Block(env, tree.compound)
    elseif tree.retstmt then
      self.kind = "return"
      if type(tree.retstmt) == "table" then
        self.expr = Expression(env, tree.retstmt)
      end
    elseif tree.breakstmt then
      local be = env
      while be do
        local k = be.kind
        if k == "switch" or k == "loop" then
          break
        else
          be = be.parent
        end
      end
      self.kind = "break"
      self.env = tassert(tree.breakstmt, be, "no scope to break out to")
    elseif tree.contstmt then
      local le = env
      while le do
        if le.kind == "loop" then
          break
        else
          le = le.parent
        end
      end
      tassert(tree.contstmt, le, "continue not in a loop scope")
      self.kind = "cont"
      self.env = le
    elseif tree.label then
      self.kind = "label"
      self.ref = env:label_get_r(tree.label.value)
      self.id = tree.label
    elseif tree.gotostmt then
      self.kind = "goto"
      self.id = tree.gotostmt
      self.ref = tassert(nil, env:label_get_r(self.id.value), 
        "no label %s in this scope", self.id.value)
    else
      dump(tree, nil, nil, nil, "stmt")
      tassert(nil, false)
    end
    return self
  end,
  repr = function(self, indent)
    local kind = self.kind
    local function dostmt(tab, dstmt, indent)
      local k = dstmt.kind
      if k ~= "block" and k~= "empty" then
        tab[#tab+1] = "\n"
        tab[#tab+1] = dstmt:repr(indent .. "  ")
        return false
      else
        if k == "empty" then
          tab[#tab+1] = ";\n"
          return false
        elseif k == "block" then
          tab[#tab+1] = " "
          tab[#tab+1] = dstmt.block:repr(indent)
          tab[#tab+1] = "\n"
          return true
        end
      end
    end
    local tab = {}
    --dump(self, true)
    if kind == "if" then
      tab[#tab+1] = indent
      tab[#tab+1] = "if"
      tab[#tab+1] = " ("
      tab[#tab+1] = self.expr:repr(indent)
      tab[#tab+1] = ")"
      dostmt(tab, self.dstmt, indent)

    elseif kind == "ifelse" then
      local cl = self
      local k
      while cl do
        if not k then
          tab[#tab+1] = indent
        else
          tab[#tab+1] = " "
        end
        tab[#tab+1] = "if"
        tab[#tab+1] = " ("
        tab[#tab+1] = cl.expr:repr(indent)
        tab[#tab+1] = ")"
        if dostmt(tab, cl.dstmt, indent) then
          tab[#tab] = " "
        else
          tab[#tab+1] = indent
        end
        tab[#tab+1] = "else"
        k = cl.estmt.kind
        if k == "if" then
          local ie = cl.estmt
          tab[#tab+1] = " "
          tab[#tab+1] = "if"
          tab[#tab+1] = " ("
          tab[#tab+1] = ie.expr:repr(indent)
          tab[#tab+1] = ")"
          dostmt(tab, ie.dstmt, indent)
          break;
        elseif k == "ifelse" then
          cl = cl.estmt
        else
          dostmt(tab, cl.estmt, indent)
          break;
        end
      end

    elseif kind == "expr" then
      tab[#tab+1] = indent
      tab[#tab+1] = self.expr:repr(indent)
      tab[#tab+1] = ";\n"
    elseif kind == "do" then
      tab[#tab+1] = indent
      tab[#tab+1] = "do"
      dostmt(tab, self.dstmt, indent)
      tab[#tab] = " " -- replace newline
      tab[#tab+1] = "while"
      tab[#tab+1] = " ("
      tab[#tab+1] = self.expr:repr(nil)
      tab[#tab+1] = ")"
      tab[#tab+1] = ";\n"
    elseif kind == "for" then
      tab[#tab+1] = indent
      tab[#tab+1] = "for"
      tab[#tab+1] = " ("
      if self.decl then
        tab[#tab+1] = self.decl:repr(nil)
      else
        if self.start then
          tab[#tab+1] = self.start:repr(nil)
        end
        tab[#tab+1] = ";"
      end
      tab[#tab+1] = " "
      tab[#tab+1] = self.cond:repr(nil)
      tab[#tab+1] = ";"
      if self.run then
        tab[#tab+1] = " "
        tab[#tab+1] = self.run:repr(nil)
      end
      tab[#tab+1] = ")"
      dostmt(tab, self.dstmt, indent)
    elseif kind == "while" then
      tab[#tab+1] = indent
      tab[#tab+1] = "while"
      tab[#tab+1] = " ("
      tab[#tab+1] = self.expr:repr(nil)
      tab[#tab+1] = ")"
      dostmt(tab, self.dstmt, indent)
    elseif kind == "block" then
      tab[#tab+1] = indent .. self.block:repr(indent) .. "\n"
    elseif kind == "return" then
      tab[#tab+1] = indent
      tab[#tab+1] = "return"
      if self.expr then
        tab[#tab+1] = " "
        tab[#tab+1] = self.expr:repr(indent)
      end
      tab[#tab+1] = ";\n"
    elseif kind == "break" then
      tab[#tab+1] = indent
      tab[#tab+1] = "break"
      tab[#tab+1] = ";\n"
    elseif kind == "cont" then
      tab[#tab+1] = indent
      tab[#tab+1] = "continue"
      tab[#tab+1] = ";\n"
    elseif kind == "label" then
      tab[#tab+1] = self.id.value
      tab[#tab+1] = ":\n"
    elseif kind == "goto" then
      tab[#tab+1] = indent
      tab[#tab+1] = "goto"
      tab[#tab+1] = " "
      tab[#tab+1] = self.id.value
      tab[#tab+1] = ";\n"
    elseif kind == "switch" then
      tab[#tab+1] = indent
      tab[#tab+1] = "switch"
      tab[#tab+1] = "("
      tab[#tab+1] = self.expr:repr(nil)
      tab[#tab+1] = ") "
      tab[#tab+1] = "{\n"
      --dostmt(tab, self.dstmt, indent)
      for k, v in ipairs(self.dstmt.block) do
        if v.kind == "case" or v.kind == "default" then
          tab[#tab+1] = v:repr(indent .. "  ")
        else
          tab[#tab+1] = v:repr(indent .. "    ")
        end
      end
      tab[#tab+1] = indent
      tab[#tab+1] = "}\n"
    elseif kind == "default" then
      tab[#tab+1] = indent
      tab[#tab+1] = "default"
      tab[#tab+1] = ":\n"
    elseif kind == "case" then
      tab[#tab+1] = indent
      tab[#tab+1] = "case"
      tab[#tab+1] = " "
      tab[#tab+1] = self.expr:repr(indent)
      tab[#tab+1] = ":\n"
    end
    return concat(tab, "")
  end,
})

