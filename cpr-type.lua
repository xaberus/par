Type = Class("Type", {
  typemap = {
    ["int8"] = "char";
    ["int16"] = "short";
    ["int32"] = "int";
    ["int64"] = "long";
    ["uint8"] = "unsigned char";
    ["uint16"] = "unsigned short";
    ["uint32"] = "unsigned";
    ["uint64"] = "unsigned long";
  },
  cprhmap = {
    ["s"] = function(self, wt, indent)
      self.struct:cpr(wt, indent)
    end,
    ["e"] = function(self, wt, indent)
      self.enum:cpr(wt, indent)
    end,
    ["x"] = function(self, wt, indent)
      wt:add(self.typemap[self.fixed])
    end,
    ["f"] = function(self, wt, indent)
      wt:add(self.fixed)
    end,
    ["r"] = function(self, wt, indent)
      wt:add(self.cid or self.id)
    end,
    ["v"] = function(self, wt, indent)
      wt:add("void")
    end,
    ["b"] = function(self, wt, indent)
      wt:add("bool")
    end,
    ["c"] = function(self, wt, indent, parent)
      local mirt -- most inner return type
      local fns = {}

      local mirt = self.rctype
      fns[#fns+1] = self
      while mirt.reg == "c" do
        fns[#fns+1] = mirt
        mirt = mirt.rctype
      end

      --idump(parent)

      mirt:cpr(wt, indent, mirt)
      wt:add(" ")
      local l = #fns
      for i = 0, l-1 do
        local fn = fns[l-i]
        if i == l-1 and parent then
          if fn.pointer then
            wt:add("(")
            fn:cpr_pointer(wt, indent)
            wt:add(" ")
            wt:add(parent.cid or parent.id)
            wt:add(")")
          else
            wt:add(parent.cid or parent.id)
          end
          if fn.array then
            fn:cpr_array(wt, indent)
          end
        else
          -- c forbids functions returning arrays...
          wt:add("(")
          fn:cpr_pointer(wt, indent)
        end
      end

      for i = 1, l do
        local fn = fns[i]
        wt:add("(")
        if i == 1 and parent.tag == "Function" then
          for k, v in ipairs(parent.params) do
            if k > 1 then
              wt:add(", ")
            end
            v.ctype:cpr(wt, indent, self)
            wt:add(" ")
            v:cpr(wt, indent, parent)
          end
        else
          for k, v in ipairs(fn.list) do
            if k > 1 then
              wt:add(", ")
            end
            v:cpr(wt, indent, mirt)
          end
        end
        wt:add(")")
        if i < l then
          wt:add(")")
        end
      end


      --[[wt:add("[")
      self.rctype:cpr(wt, indent, parent)
      wt:add(" : ")
      for k, v in ipairs(self.list) do
        if k > 1 then
          wt:add(",")
        end
        v:cpr(wt, indent, parent)
      end
      wt:add("]")]]
    end,
  },
  cprh = {
    ["*"] = function(self, wt, indent, parent)
      if self.reg ~= "c" then
        self:cpr_qual(wt, indent)
      end
      self.cprhmap[self.reg](self, wt, indent, parent)

      if self.reg ~= "c" then
        if self.pointer then
          wt:add(" ")
          self:cpr_pointer(wt, indent)
        end
        if self.array then
          self:cpr_array(wt, indent)
        end
      end
    end
  },
  cpr_qual = function(self, wt, indent)
    local first = true
    for k, v in pairs(self.qual) do
      wt:add(k)
      wt:add(" ")
    end
  end,
  cpr_array = function(self, wt, indent)
    for k, v in ipairs(self.array) do
      wt:add("[")
      if v.static then
        wt:add("static")
      end
      if v.const then
        wt:add("const")
      end
      if not v.flexible then
        v.size:cpr(wt, indent)
      end
      wt:add("]")
    end
  end,
  cpr_pointer = function(self, wt, indent)
    for k, v in ipairs(self.pointer) do
      if next(v) then
        local qt = {}
        for q, b in pairs(v) do
          qt[#qt+1] = q
        end
        wt:add("* ", concat(qt, " "))
      else
        wt:add("*")
      end
    end
  end,
  cpr = function(self, wt, indent, parent)
    assert(parent, "no parent")
    local fn = self.cprh[parent.tag]
    if not fn then
      --idump(parent)
      fn = self.cprh["*"]
    end
    fn(self, wt, indent, parent)
  end,
},
function(T, self)
  --idump(self)
  if self.struct then
    disown(self.struct)
  elseif self.enum then
    disown(self.enum)
  elseif self.list then
    for k, v in ipairs(self.list) do
      disown(v)
    end
    disown(self.rctype)
  elseif self.ref then
    disown(self.ref)
  end
  if self.array then
    for k, v in ipairs(self.array) do
      if not v.flexible then
        disown(v.size)
      end
    end
  end
  return self
end)

local function decltype(self, wt, indent, parent)
  if self.reg ~= "c" then
    if self.pointer then
      self:cpr_pointer(wt, indent)
      wt:add(" ")
    end
    wt:add(parent.cid or parent.id)
    if self.array then
      self:cpr_array(wt, indent)
    end
  else
    self:cpr_qual(wt, indent)
    self.cprhmap[self.reg](self, wt, indent, parent)
  end
end

Type.cprh["Parameter"] = decltype
Type.cprh["Declarator"] = decltype
Type.cprh["StructDeclarator"] = decltype

local function basetype(self, wt, indent, parent)
  if self.reg ~= "c" then
    self:cpr_qual(wt, indent)
    self.cprhmap[self.reg](self, wt, indent, parent)
  end
end

Type.cprh["Function"] = function(self, wt, indent, parent)
  if self.reg == "c" then
    decltype(self, wt, indent, parent)
  else
    basetype(self, wt, indent, parent)
  end
end

Type.cprh["Declaration"] = function(self, wt, indent, parent)
  if parent.tdef and self.reg == "c" then
    decltype(self, wt, indent, parent)
  else
    basetype(self, wt, indent, parent)
  end
end
Type.cprh["StructDeclaration"] = basetype
