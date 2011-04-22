Statement = Class("Statement", {
  h = {
    ["if"] = function(env, tree, self)
      self.expr = tree.ifcl
      self.dstmt = tree.dstmt
      self.opened = tree.opened
      if tree.estmt then
        self.kind = "ifelse"
        self.estmt = tree.estmt
      else
        self.kind = "if"
      end
    end,
    ["do"] = function(env, tree, self)
      self.kind = "do"
      self.expr = tree.docl
      self.dstmt = tree.dstmt
    end,
    ["for"] = function(env, tree, self)
      local m = tree.forcl
      self.kind = "for"
      if m == "ee" then
        self.start = tree.init
        self.cond = tree.cond
      elseif m == "eee" then
        if(tree.init) then
          self.start = tree.init
        end
        self.cond = tree.cond
        self.run = tree.incr
      elseif m == "de" then
        self.decl = tree.decl
        self.cond = tree.cond
      elseif m == "dee" then
        self.decl = tree.decl
        self.cond = tree.cond
        self.run = tree.incr
      end
      self.dstmt = tree.dstmt
    end,
    ["while"] = function(env, tree, self)
      self.kind = "while"
      self.expr = tree.whilecl
      self.dstmt = tree.dstmt
    end,
    ["switch"] = function(env, tree, self)
      self.kind = "switch"
      self.expr = tree.switchcl
      self.dstmt = tree.dstmt
    end,
    ["default"] = function(env, tree, self)
      tassert(env.loc[self],
        env.kind == "switch"
        or env.parent.kind == "switch")
      self.kind = "default"
    end,
    ["case"] = function(env, tree, self)
      tassert(env.loc[self],
        env.kind == "switch"
        or env.parent.kind == "switch")
      self.kind = "case"
      self.expr = tree.case
    end,
    ["expr"] = function(env, tree, self)
      self.kind = "expr"
      self.expr = tree.expr
    end,
    ["@"] = function(env, tree, self)
      self.kind = "empty"
    end,
    ["{}"] = function(env, tree, self)
      self.kind = "block"
      self.block = tree.compound
    end,
    ["return"] = function(env, tree, self)
      self.kind = "return"
      if type(tree.retstmt) == "table" then
        self.expr = tree.retstmt
      end
    end,
    ["break"] = function(env, tree, self)
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
    end,
    ["continue"] = function(env, tree, self)
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
    end,
    ["label"] = function(env, tree, self)
      local v = tree.label.value
      self.kind = "label"
      --self.ref = env:label_get_r(v)
      self.id = v
    end,
    ["goto"] = function(env, tree, self)
      local v = tree.gotostmt.value
      self.kind = "goto"
      self.id = v
      --self.ref = tassert(tree.gotostmt, env:label_get_r(v), 
        --"no label %s in this scope", v)
    end,
  },
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
      tab[#tab+1] = self.id
      tab[#tab+1] = ":"
      if indent then
        tab[#tab+1] = "\n"
      end
    elseif kind == "goto" then
      tab[#tab+1] = indent
      tab[#tab+1] = "goto"
      tab[#tab+1] = " "
      tab[#tab+1] = self.id
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
  get_labels = function(self, labels)
    local kind = self.kind
    if kind == "label" then
      labels[#labels+1] = self
    elseif kind == "block" then
      for k, v in ipairs(self.block.labels) do
        labels[#labels+1] = v
      end
    end
  end,
  check_labels = function(self, env)
    local kind = self.kind
    if kind == "goto" then
      if not self.ref then
        ref = tassert(env.loc[self], env:label_get_r(self.id), "undefined label '%s'", self.id)
      end
    elseif kind == "block" then
      self.block:check_labels()
    end
  end,
},
function(S, env, tree)
  --dump(tree, nil, nil, nil, "stmt")
  local self = mktab(env, tree, {}, S)
  S.h[tree.k](env, tree, self)
  return self
end)

