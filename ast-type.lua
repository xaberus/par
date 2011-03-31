Type = Class("Type", {
  set_int_flag = function(self, flag, ...)
    tassert(nil, not self.reg or self.reg == "i")
    if not self.reg then
      self.reg = "i"
      self.int = {}
      self.con = {}
    end

    local function mkadd(int, con, flag)
      if not int[flag] then
        con[flag] = true
      else
        con[flag] = nil
      end

      if false then
      elseif flag == "unsigned" then
        con["signed"] = nil
      elseif flag == "signed" then
        con["unsigned"] = nil
      elseif flag == "char" then
        con["int"] = nil
        con["short"] = nil
        con["long"] = nil
      elseif flag == "short" then
        con["char"] = nil
        con["int"] = nil
        con["long"] = nil
      elseif flag == "long" then
        con["char"] = nil
        con["short"] = nil
      elseif flag == "int" then
        con["char"] = nil
        con["short"] = nil
      end
    end

    if flag == "signed" then
      tassert(nil, not self.int.unsigned, "signed vs. unsigned")
      self.int.signed = true
    elseif flag == "unsigned" then
      tassert(nil, not self.int.signed, "signed vs. unsigned")
      self.int.unsigned = true
    elseif flag == "char" then
      tassert(nil, not self.int.short, "char vs. char")
      tassert(nil, not self.int.short, "char vs. short")
      tassert(nil, not self.int.long, "char vs. long")
      tassert(nil, not self.int.int, "char vs. int")
      self.int.char = true
    elseif flag == "short" then
      tassert(nil, not self.int.char, "short vs. char")
      tassert(nil, not self.int.short, "short vs. short")
      tassert(nil, not self.int.long, "short vs. long")
      self.int.short = true
    elseif flag == "long" then
      tassert(nil, not self.int.char, "long vs. char")
      tassert(nil, not self.int.short, "long vs. short")
      tassert(nil, not self.int.long, "long vs. long")
      self.int.long = true
    elseif flag == "int" then
      tassert(nil, not self.int.char, "int vs. char")
      tassert(nil, not self.int.int, "int vs. int")
      self.int.int = true
    end

    mkadd(self.int, self.con, flag)
    for i, v in ipairs({...}) do
      mkadd(self.int, self.con, v)
    end
    --dump({self.int, self.con, "#####", flag, ...})

  end,

  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "ctype")

    local self = setmetatable({
      qual = {}
    }, Type)

    if tree.functype then
      self.reg = "c"
      self.rctype = Type(env, tree.functype.ret)
      local list = {}
      for k, v in ipairs(tree.functype.list) do
        list[#list+1] = Type(env, v)
      end
      self.list = list
    else
      for v, q in ipairs(tree.sqlist.qual) do
        if q.tag == "token" then
          local v = q.value
          if v == "const" then
            tassert(nil, not self.qual.volatile, "const vs. volatile")
            self.qual.const = true
          elseif v == "volatile" then
            tassert(nil, not self.qual.const, "volatile vs. const")
            self.qual.volatile = true
          elseif v == "restrict" then
            tassert(nil, false, "restrict is only valid for pointers")
          else
            dump(self, true)
            tassert(nil, false)
          end
        else
          dump(self, true)
          tassert(nil, false)
        end
      end

      for v, s in ipairs(tree.sqlist.spec) do
        if s.tag == "token" then
          local v = s.value
          if v == "int" then
            self:set_int_flag("int", "signed")
          elseif v == "char" then
            self:set_int_flag("char", "signed")
          elseif v == "short" then
            self:set_int_flag("short", "signed", "int")
          elseif v == "long" then
            self:set_int_flag("long", "signed", "int")
          elseif v == "signed" then
            self:set_int_flag("signed", "int")
          elseif v == "unsigned" then
            self:set_int_flag("unsigned", "int")
          elseif v == "int8" then
            self.reg = "x"
            self.fixed = s
          elseif v == "int16" then
            self.reg = "x"
            self.fixed = s
          elseif v == "int32" then
            self.reg = "x"
            self.fixed = s
          elseif v == "int64" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint8" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint16" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint32" then
            self.reg = "x"
            self.fixed = s
          elseif v == "uint64" then
            self.reg = "x"
            self.fixed = s
          elseif v == "void" then
            self.reg = "x"
            self.fixed = s
          else
            local ref = env:type_get_r(v)
            if ref then
              self.reg = "r"
              self.ref = ref
              self.id = s
            else
              dump(tree, true)
              tassert(nil, false)
            end
          end
        elseif s.struct or s.union then
          tassert(nil, not self.reg, "impossible combination of specifiers")
          local struct = Struct(env, s)
          self.reg = "s"
          self.struct = struct
        else
          dump(tree, true)
          tassert(nil, false)
        end
      end
    end


    if self.reg == "i" then
      local con = self.con
      for k, v in pairs(self.int) do
        con[k] = v
      end
      self.complete = true
    elseif self.reg == "x" then
      self.complete = true
    elseif self.reg == "r" then
      self.complete = true
    elseif self.reg == "s" then
      self.complete = true
    elseif self.reg == "c" then
      self.complete = true
    else
      dump(self, true)
      tassert(nil, false, "type machine: NIY")
    end

    if tree.adecl then
      self.abs = true
      local ad = tree.adecl
      if ad.pointer then
        --dump(ad, true)
        local ptr = {}
        self.pointer = ptr
        for j, p in ipairs(ad.pointer) do
          local quals = {}
          --dump(ad.pointer, true)

          if p.qual then
            for k, q in ipairs(p.qual) do
              local n = q.value
              if n == "const" then
                quals.const = true
              elseif n == "volatile" then
                quals.volatile = true
              elseif n == "restrict" then
                quals.restrict = true
              else
                dump(self, true)
                tassert(nil, false)
              end
            end
          end
          ptr[#ptr+1] = quals
        end
      end
      if ad.array then
        --dump(ad, true)
        local arr = {}
        self.array = arr
        for j, a in ipairs(ad.array) do
          local quals = {}

          if a.qual then
            for k, q in ipairs(a.qual) do
              local n = q.value
              if n == "const" then
                quals.const = true
              else
                tassert(nil, false,
                  "qualifier '%s' is not valid in this context", n)
              end
            end
          end

          if a.static then
            quals.static = true
          end

          local s = a.expr
          if not s then
            quals.flexible = true
          else
            local expr = tassert(nil, Expression(env, s))

            if expr:is_constant() or env.kind == "block" then
              quals.size = expr
            elseif expr:is_symbol() then
              local sym = tassert(nil, expr:symbol_eval(), "not a symbol?")
              quals.size = expr
              quals.ref = sym.ref
              quals.vla = true
            else
              tassert(tree_get_any_token(s), false,
                "invalid expression '%s' in this context", expr:repr(""))
            end
          end
          arr[#arr+1] = quals
        end
      end
    end

    return self
  end,
  repr = function(self, indent)
    local ret = ""
    if self.complete then
      local first = true
      for k, v in pairs(self.qual) do
        if not first then
          ret = ret .. " " .. k
        else
          ret = ret .. k
          first = false
        end
      end
      if not first then
        ret = ret .. " "
      end
      if self.reg == "i" then
        for k, v in pairs(self.int) do
          if not first then
            ret = ret .. " " .. k
          else
            ret = ret .. k
            first = false
          end
        end
      elseif self.reg == "s" then
        ret = ret .. self.struct:repr(indent)
      elseif self.reg == "x" then
        ret = ret .. self.fixed.value
      elseif self.reg == "r" then
        ret = ret .. self.id.value
      elseif self.reg == "c" then
        local tab = {}
        for k, v in ipairs(self.list) do
          tab[#tab+1] = v:repr(indent)
        end
        ret = ret .. "[" .. self.rctype:repr(indent)  .. " : " .. concat(tab, ", ") .. "]"
      end

      if self.abs then
        if self.pointer then
          local tab = {}
          for k, v in ipairs(self.pointer) do
            if next(v) then
              local qt = {}
              for q, b in pairs(v) do
                qt[#qt+1] = q
              end
              tab[#tab+1] = "* " .. concat(qt, " ")
            else
              tab[#tab+1] = "*"
            end
          end
          ret = ret .. " " .. concat(tab, " ")
        end
        if self.array then
          local tab = {}
          for k, v in ipairs(self.array) do
            local prt = {}
            if v.static then
              prt[#prt+1] = "static"
            end
            if v.const then
              prt[#prt+1] = "const"
            end
            if not v.flexible then
              prt[#prt+1] = v.size:repr(indent)
            end
            ret = ret .. "[" .. concat(prt, " ") .. "]"
          end
          --dump(self.array, true)
        end
      end

    else
      dump(self, true)
      ret = ret .. "<incomplete type>"
    end

    return ret
  end,
  compare = function(a, b)
    --dump({a,b}, true, nil, nil, "inref")

    if a.reg == "r" then
      a = a.ref.ctype
    end
    if b.reg == "r" then
      b = b.ref.ctype
    end

    --dump({a,b}, true, nil, nil, "outref")
    --dump({a,b}, nil, nil, nil, "outref")

    if a.complete ~= b.complete then return false end
    if a.abs ~= b.abs then return false end

    if not deepcompare(a.qual, b.qual) then return false end

    if a.reg == "i" then
      if not deepcompare(a.con, b.con) then return false end
    end

    if a.abs then
      if not deepcompare(a.pointer, b.pointer) then return false end
      if not deepcompare(a.array, b.array) then return false end
    end

    if a.reg ~= b.reg then return false end
    return true
  end,
})

