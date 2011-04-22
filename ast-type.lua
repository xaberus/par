local function deepcopy(object)
    local lookup_table = {}
    local function _copy(object)
        if type(object) ~= "table" then
            return object
        elseif lookup_table[object] then
            return lookup_table[object]
        end
        local new_table = {}
        lookup_table[object] = new_table
        for index, value in pairs(object) do
            new_table[_copy(index)] = _copy(value)
        end
        return setmetatable(new_table, getmetatable(object))
    end
    return _copy(object)
end

Type = Class("Type", {
  intmap = {
    ["char"] = {
      ["unsigned"] = "uint8";
      ["signed"] = "int8";
      "signed";
    };
    ["short"] = {
      ["int"] = {
        ["unsigned"] = "uint16";
        ["signed"] = "int16";
        "signed";
      };
      ["unsigned"] = {
        ["int"] = "uint16";
        "int";
      };
      ["signed"] = {
        ["int"] = "int16";
        "int";
      };
      "signed";
    };
    ["int"] = {
      ["short"] = {
        ["unsigned"] = "uint16";
        ["signed"] = "int16";
        "signed";
      };
      ["long"] = {
        ["unsigned"] = "uint64";
        ["signed"] = "int64";
        "signed";
      };
      ["unsigned"] = {
        ["short"] = "uint16";
        ["long"] = "uint64";
        ["@"] = "uint32";
        "@";
      };
      ["signed"] = {
        ["short"] = "int16";
        ["long"] = "int64";
        ["@"] = "int32";
        "@";
      };
      "signed";
    };
    ["long"] = {
      ["int"] = {
        ["unsigned"] = "uint64";
        ["signed"] = "int64";
        "signed";
      };
      ["unsigned"] = {
        ["int"] = "uint64";
        "int";
      };
      ["signed"] = {
        ["int"] = "int64";
        "int";
      };
      "signed";
    };
    ["unsigned"] = {
      ["char"] = "uint8";
      ["int"] = {
        ["short"] = "uint16";
        ["@"] = "uint32";
        ["long"] = "uint64";
        "@"
      };
      ["short"] = {
        ["int"] = "uint16";
        "int";
      };
      "int";
    };
    ["signed"] = {
      ["char"] = "int8";
      ["int"] = {
        ["short"] = "int16";
        ["@"] = "int32";
        ["long"] = "int64";
        "@"
      };
      ["short"] = {
        ["int"] = "int16";
        "int";
      };
      "int";
    };
    ["uint8"] = "uint8";
    ["uint16"] = "uint16";
    ["uint32"] = "uint32";
    ["uint64"] = "uint64";
    ["int8"] = "int8";
    ["int16"] = "int16";
    ["int32"] = "int32";
    ["int64"] = "int64";
  },
  fpnmap = {
    ["float"] = "float";
    ["double"] = "double";
  },

  abstract_decl = function(self, env, tree)
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
                tassert(env.loc[self], false, "AST/Type not reached")
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
                tassert(env.loc[self], false,
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
            local expr = tassert(env.loc[self], s, "AST/Type no expression")

            if expr:is_constant() or env.kind == "block" then
              quals.size = expr
            elseif expr:is_symbol() then
              local sym = tassert(env.loc[self], expr:symbol_eval(), "not a symbol?")
              quals.size = expr
              quals.ref = sym.ref
              quals.vla = true
            else
              tassert(env.loc[expr], false,
                "invalid expression '%s' in this context", expr:repr(""))
            end
          end
          arr[#arr+1] = quals
        end
      end
    end
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
      if self.reg == "s" then
        ret = ret .. self.struct:repr(indent)
      elseif self.reg == "e" then
        ret = ret .. self.enum:repr(indent)
      elseif self.reg == "x" then
        ret = ret .. self.fixed
      elseif self.reg == "r" then
        ret = ret .. self.id
      elseif self.reg == "v" then
        ret = ret .. "void"
      elseif self.reg == "b" then
        ret = ret .. "bool"
      elseif self.reg == "c" then
        local tab = {}
        for k, v in ipairs(self.list) do
          tab[#tab+1] = v:repr(indent)
        end
        ret = ret .. "[" .. self.rctype:repr(indent)  .. " : " .. concat(tab, ", ") .. "]"
      elseif self.reg == "i" then
        ret = ret .. "interface " .. self.id
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

    while a.reg == "r" do
      a = a.ref.ctype
    end
    while b.reg == "r" do
      b = b.ref.ctype
    end

    --dump({a,b}, true, nil, nil, "outref")
    --dump({a,b}, nil, nil, nil, "outref")

    tassert(nil, a.complete and b.complete, "cannot compare incomplete types")

    if a.abs ~= b.abs then return false end

    if not deepcompare(a.qual, b.qual) then return false end

    if a.reg ~= b.reg then return false end

    if a.reg == "x" or a.reg == "f" then
      if a.fixed ~= b.fixed then return false end
    end

    if a.abs then
      if not deepcompare(a.pointer, b.pointer) then return false end
      if not deepcompare(a.array, b.array) then return false end
    end

    if a.reg ~= b.reg then return false end
    return true
  end,

  is_integer = function(self)
    while self.reg == "r" do
      self = self.ref.ctype
    end
    return self.reg == "x" and self.intsmap[self.rex.fixed] ~= nil
  end,

  get_struct = function(self, env)
    while self.reg == "r" do
      self = self.ref.ctype
    end
    if self.reg == "s" then
      return self.struct
    elseif self.reg == "i" then
      return tassert(nil, env:iface_get_r(self.id), "no interface '%s' in this scope", self.id).struct
    end
  end,

  get_enum = function(self, env)
    while self.reg == "r" do
      self = self.ref.ctype
    end
    if self.reg == "e" then
      return self.enum
    end
  end,

  get_function = function(self, env)
    while self.reg == "r" do
      self = self.ref.ctype
    end
    --dump(self.rctype)
    return self
  end,

  get_arith_type = function(T, a, b)
    if a.reg == "x" and b.reg == "x" then
      --print(a.fixed, "&", b.fixed)
      local n = T.arithmap[a.fixed][b.fixed]
      if a.fixed == n then
        return a
      else
        return b
      end
    elseif a.reg == "e" and b.reg == "e" then -- TODO FSA
      return T.int_res
    elseif a.reg == "x" and b.reg == "e" then
      return T.int_res
    elseif a.reg == "e" and b.reg == "x" then
      return T.int_res
    end
    print(a.reg, a:repr(""), b.reg, b:repr(""))
  end,

  to_pointer = function(self)
    local t = deepcopy(self)
    t.abs = true
    if not t.pointer then
      t.pointer = {{}}
    else
      t.pointer[t.pointer+1] = {}
    end
    return t
  end,

  strip_pointer = function(self)
    local t = deepcopy(self)
    tassert(nil, t.abs and t.pointer, "not a pointer")
    if #t.pointer > 1 then
      remove(t.pointer, #t.pointer)
    else
      t.pointer = nil
    end
    return t
  end,

  strip_dep = function(self)
    if self.abs then
      if self.array then
        local t = deepcopy(self)
        for k, a in ipairs(t.array) do
          if a.vla then
            a.size = nil
            a.ref = nil
            a.vla = nil
            a.flexible = true
          end
        end
      end
    end
    return self
  end,
},
-----------------------------------------------
-----------------------------------------------
-----------------------------------------------

function(T, env, tree)
  --dump(tree, nil, nil, nil, "ctype")

  local self = mktab(env, tree, {qual = {}}, T)

  if tree.functype then
    self.reg = "c"
    self.rctype = tree.functype.ret
    local list = {}
    for k, v in ipairs(tree.functype.list) do
      list[#list+1] = v
    end
    self.list = list
    self.complete = true
  elseif tree.iface then
    self.reg = "i"
    self.id = tree.iface.value
    self.cid = tassert(nil, env:ns_get_iface(self.id), "AST/Type no c id")
    self.complete = true
  else
    for v, q in ipairs(tree.sqlist.qual) do
      if q["@tag"] == "token" then
        local v = q.value
        if v == "const" then
          tassert(q, not self.qual.volatile, "const vs. volatile")
          self.qual.const = true
        elseif v == "volatile" then
          tassert(q, not self.qual.const, "volatile vs. const")
          self.qual.volatile = true
        elseif v == "restrict" then
          tassert(q, false, "restrict is only valid for pointers")
        else
          dump(self, true)
          tassert(q, false, "AST/Type NIY")
        end
      else
        dump(self, true)
        tassert(env.loc[self], false, "AST/Type not reached")
      end
    end

    local intmap, intp, int = T.intmap, false, nil
    local fpnmap, fpnp, fpn = T.fpnmap, false, nil

    for k, s in ipairs(tree.sqlist.spec) do
      if s["@tag"] == "token" then
        local v = s.value

        tassert(s, type(intmap) ~= "string",
          "attempted to add specifier '%s' to complete type '%s'", v, intmap)

        if intp then
          int = tassert(s, intmap[v], "unknown specifier '%s' in this context", v)
          intmap = int
        else
          int = intmap[v]
          if int then
            tassert(s, k == 1, "unexpected '%s'", v)
            intp = true
            intmap = int
          end
        end

        if fpnp then
          fpn = tassert(s, fpnmap[v], "unknown specifier '%s' in this context", v)
          fpnmap = fpn
        else
          fpn = fpnmap[v]
          if fpn then
            tassert(s, k == 1, "unexpected '%s'", v)
            fpnp = true
            fpnmap = fpn
          end
        end


        if not int and not fpn then
          tassert(s, #tree.sqlist.spec == 1, "more than one type in type name")
          if v == "void" then
            self.reg = "v"
            self.complete = true
            break
          elseif v == "bool" then
            self.reg = "b"
            self.complete = true
            break
          else
            --dump(tree)
            self.reg = "r"
            local ref = tassert(s, env:type_get_r(v), "reference to undefined type '%s'", v)
            --dump(self.ref)
            if v ~= "selftype" then
              self.id = v
              self.cid = tassert(nil, env:ns_get_type(v), "AST/Type no c id")
              self.complete = true
              self.ref = ref
            else
              self.reg = "i"
              self.id = ref.id
              self.cid = tassert(nil, ref.cid, "AST/Type no c id")
              self.complete = true
            end
            break
          end
        end
      elseif s["@tag"] == "Struct" then
        tassert(s, #tree.sqlist.spec == 1, "more than one type in type name")
        local struct = s
        self.reg = "s"
        self.struct = struct
        self.complete = true
        break
      elseif s["@tag"] == "Enum" then
        tassert(s, #tree.sqlist.spec == 1, "more than one type in type name")
        local enum = s -- create a new type here
        self.reg = "e"
        self.enum = enum
        self.complete = true
      else
        dump(tree)
        tassert(nil, false, "AST/Type NIY")
      end
    end
    if intp then
      -- resolve type
      while type(intmap) ~= "string" do
        intmap = intmap[intmap[1]]
      end
      self.reg = "x"
      self.fixed = intmap
      self.complete = true
    elseif fpnp then
      -- resolve type
      while type(fpnmap) ~= "string" do
        fpnmap = fpnmap[fpnmap[1]]
      end
      self.reg = "f"
      self.fixed = fpnmap
      self.complete = true
    end

  end

  T.abstract_decl(self, env, tree)

  --dump(self, true, nil, nil, "Type")

  return self
end)

Type.int_literal = setmetatable({
  ["complete"] = "true";
  ["reg"] = "x";
  ["fixed"] = "int64";
  ["qual"] = {
    ["const"] = "true";
  };

}, Type)

Type.uint_literal = setmetatable({
  ["complete"] = "true";
  ["reg"] = "x";
  ["fixed"] = "uint64";
  ["qual"] = {
    ["const"] = "true";
  };

}, Type)


Type.char_literal = setmetatable({
  ["complete"] = "true";
  ["reg"] = "x";
  ["fixed"] = "int8";
  ["qual"] = {
    ["const"] = "true";
  };
}, Type)

Type.cstr_literal = setmetatable({
  ["pointer"] = {
    [1] = {
      ["const"] = "true"; -- right?
    };
  };
  ["qual"] = {
    ["const"] = "true";
  };
  ["int"] = {
    ["char"] = "true";
  };
  ["complete"] = "true";
  ["abs"] = "true";
  ["reg"] = "x";
  ["complete"] = "true";
  ["reg"] = "x";
  ["fixed"] = "int8";
}, Type)

Type.bool_res = setmetatable({
  ["complete"] = "true";
  ["reg"] = "b";
  ["qual"] = {};
}, Type);

Type.int_res = setmetatable({
  ["complete"] = "true";
  ["reg"] = "x";
  ["fixed"] = "int64";
  ["qual"] = {};

}, Type)



Type.arithmap = {
  ["int8"] = {
    ["int8"] = "int8";
    ["int16"] = "int16";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "int8";
    ["uint16"] = "int16";
    ["uint32"] = "int32";
    ["uint64"] = "int64";
  };
  ["int16"] = {
    ["int8"] = "int16";
    ["int16"] = "int16";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "int16";
    ["uint16"] = "int16";
    ["uint32"] = "int32";
    ["uint64"] = "int64";
  };
  ["int32"] = {
    ["int8"] = "int32";
    ["int16"] = "int32";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "int32";
    ["uint16"] = "int32";
    ["uint32"] = "int32";
    ["uint64"] = "int64";
  };
  ["int64"] = {
    ["int8"] = "int32";
    ["int16"] = "int32";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "int64";
    ["uint16"] = "int64";
    ["uint32"] = "int64";
    ["uint64"] = "int64";
  };

  ["uint8"] = {
    ["int8"] = "int8";
    ["int16"] = "int16";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "uint8";
    ["uint16"] = "uint16";
    ["uint32"] = "uint32";
    ["uint64"] = "uint64";
  };
  ["uint16"] = {
    ["int8"] = "int16";
    ["int16"] = "int16";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "uint16";
    ["uint16"] = "uint16";
    ["uint32"] = "uint32";
    ["uint64"] = "uint64";
  };
  ["uint32"] = {
    ["int8"] = "int32";
    ["int16"] = "int32";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "uint32";
    ["uint16"] = "uint32";
    ["uint32"] = "uint32";
    ["uint64"] = "uint64";
  };
  ["uint64"] = {
    ["int8"] = "int32";
    ["int16"] = "int32";
    ["int32"] = "int32";
    ["int64"] = "int64";

    ["uint8"] = "uint64";
    ["uint16"] = "uint64";
    ["uint32"] = "uint64";
    ["uint64"] = "uint64";
  };
}

