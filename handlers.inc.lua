
function declaration_handler(tree, variant, typedefs)
  local function decide_typedef(tree)
    local function deep_lookup(tree, key)
      for k,v in pairs(tree) do
        if k == "declaration_specifiers" then
          if deep_lookup(v, key) then return true end
        elseif k == "storage_class_specifier" then
          if deep_lookup(v, key) then return true end
        elseif k == key then 
          --dump(v,nil,nil,true)
          return true
        end
      end
      return false
    end
    local function get_name(tree)
      for k,v in pairs(tree) do
        --print("%s/%s\n", tree.tag, k)
        local r = nil
        if k == "init_declarator_list" then
          r = get_name(v)
        elseif k == "init_declarator" then
          r = get_name(v)
        elseif k == "declarator" then
          r = get_name(v)
        elseif k == "direct_declarator" then
          r = get_name(v)
        elseif k == "identifier" then
          --dump(v,nil,nil,true)
          r = v
        end

        if r then return r end
      end
      return nil
    end

    if deep_lookup(tree, "typedef") then
      --dump(tree)
      local r = get_name(tree)
      --dump(r)
      return r.value
    end
  end

  if variant == 1 then
    --dump(tree)
  elseif variant == 2 then
    local r = decide_typedef(tree)
    --dump(tree)
    if r then
      typedefs[r] = tree
      --dump(typedefs, 0, 0)
    end
  elseif variant == 3 then
    --dump(tree,nil,nil,nil,io.stderr)
  else
    error("not reached")
  end
end

local green = "[1;32m"
local yellow = "[1;33m"
local red = "[1;31m"
local blue = "[1;34m"
local white = "[1;37m"
local clear = "[0;m"

type_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__add = function(a, b)
		for i,v in ipairs(b.t) do
			a.t[#a.t+1] = v
		end
		return a
	end,
	__mul = function(a, b)
		
	end,
	__tostring = function(self)
		local ret = green .. "Type(" ..red
		for i, v  in ipairs(self.t) do
			if i>1 and i<#self.t then
			ret = ret .. " "
			end
			if v.spec then
				ret = ret .. v.spec.n
			elseif v.ref then
				local r = v.ref
				ret = ret .. " "
				if r.k == "pointer" then
					for i=1, r.d do
						ret = ret .. "*"
					end
				end
			elseif v.array then
				local r = v.array
				ret = ret .. " "
				if r.v then
					ret = ret .. "[]"
				else
					ret = ret .. string.format("[%d]", r.n)
				end
			end
		end
		return ret ..green .. ")" .. clear
	end,
	last = function(self, create)
		local l = self.t[#self.t]
		if not l and create then
			l = {}
			table.insert(self.t, l)
		end
		return l
	end,
	
	add_type_specifier = function(self, name)
		local t = self:last(true)
		t.spec = {n = name}
		return self
	end,
	add_ref = function(self, kind, extra)
		local t = self:last(true)
		if not t.ref then
			t.ref = {k = kind, d=1, e = extra}
		else
			t.ref.d = t.ref.d + 1
		end
		return self
	end,
	add_vararray = function(self)
		local t = self:last(true)
		t.array = {n = 0, v = true}
		return self
	end,
	add_array = function(self, n)
		local t = self:last(true)
		t.array = {n = n, v = false}
		return self
	end,
	
	finalize = function(self)
		--dump(self)
		--print(tostring(self))
		return self
	end,
}

setmetatable(type_meta, {
  __call = function()
		local ret = {t = {}}
		return setmetatable(ret, type_meta)
  end,
})

param_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__add = function(a, b)
		for i,v in ipairs(b) do
			a[#a+1] = v
		end
		return a
	end,
	__tostring = function(self)
		local ret = yellow .. "Param("
		for i,v in ipairs(self) do
			if i>1 then
				ret = ret .. ", "
			end
			ret = ret .. tostring(v.t) .. " " .. v.id
		end
		return ret .. yellow .. ")" .. clear
	end,
	add_decl = function(self, t, id)
		self[#self+1] = {t = t, id = id}
		return self
	end,
	
	finalize = function(self)
		--dump(self)
		--print(tostring(self))
		return self
	end,
}

setmetatable(param_meta, {
  __call = function()
		local ret = {}
		return setmetatable(ret, param_meta)
  end,
})


decl_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__add = function(a, b)
		for i,v in ipairs(b) do
			a[#a+1] = v
		end
		return a
	end,
	__tostring = function(self)
		local ret = blue .. "Decl(" .. tostring(self.t)
		for i,v in ipairs(self) do
			ret = ret .. ", "
			ret = ret .. tostring(v.id)
			if v.init then
				ret = ret .. " = "
				ret = ret .. tostring(v.init)
			end
		end
		return ret .. blue .. ")" .. clear
	end,
	add_id = function(self, id, init)
		self[#self+1] = {id = id, init = init}
		return self
	end,
	add_type = function(self, t)
		self.t = t
		return self
	end,
	
	finalize = function(self)
		--dump(self)
		--print(tostring(self))
		return self
	end,
}

setmetatable(decl_meta, {
  __call = function()
		local ret = {}
		return setmetatable(ret, decl_meta)
  end,
})


block_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__add = function(a, b)
		for i,v in ipairs(b) do
			a[#a+1] = v
		end
		return a
	end,
	__tostring = function(self)
		local ret = white .. "Block {\n"
		for i,v in ipairs(self) do
			if v.decl then
				ret = ret .. tostring(v.decl) .. "\n"
			else
				ret = ret .. tostring(v.stmt) .. "\n"
			end
		end
		return ret .. white .. "}" .. clear
	end,
	add_decl = function(self, decl)
		self[#self+1] = {decl = decl}
		return self
	end,
	add_stmt = function(self, stmt)
		self[#self+1] = {stmt = stmt}
		return self
	end,
	
	finalize = function(self)
		--dump(self)
		--print(tostring(self))
		return self
	end,
}

setmetatable(block_meta, {
  __call = function()
		local ret = {}
		return setmetatable(ret, block_meta)
  end,
})

stmt_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__tostring = function(self)
		return "Stmt(" .. tostring(self.expr) .. ")"
	end,
	add_expr = function(self, expr)
		self.expr = expr
		return self
	end,

	
	finalize = function(self)
		--dump(self)
		print(tostring(self))
		return self
	end,
}

setmetatable(stmt_meta, {
  __call = function()
		local ret = {}
		return setmetatable(ret, stmt_meta)
  end,
})

trans_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__add = function(a, b)
		for i,v in ipairs(b) do
			a[#a+1] = v
		end
		return a
	end,
	__tostring = function(self)
		local ret = blue .. "Trans("
		return ret .. blue .. ")" .. clear
	end,
	add_decl = function(self, decl)
		self[#self+1] = {decl = decl}
		return self
	end,
	add_fun = function(self, fun)
		self[#self+1] = {fun = fun}
		return self
	end,
	
	finalize = function(self)
		--dump(self)
		print(tostring(self))
		return self
	end,
}

setmetatable(trans_meta, {
  __call = function()
		local ret = {}
		return setmetatable(ret, trans_meta)
  end,
})

func_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
	__tostring = function(self)
		local ret = tostring(self.ret) .. blue .. " Func " 
		ret = ret .. tostring(self.param)  .. " "
		ret = ret .. tostring(self.comp)
		return ret .. clear
	end,
	add_ret = function(self, t)
		self.ret = t
		return self
	end,
	add_id = function(self, id)
		self.id = id
		return self
	end,
	add_param = function(self, param)
		self.param = param
		return self
	end,
	add_comp = function(self, comp)
		self.comp = comp
		return self
	end,
	
	finalize = function(self)
		--dump(self)
		print(tostring(self))
		return self
	end,
}

setmetatable(func_meta, {
  __call = function()
		local ret = {}
		return setmetatable(ret, func_meta)
  end,
})

expr_meta = {
	__index = function(self,k)
		return getmetatable(self)[k]
  end,
  tonumber = function(self)
		if self.constant then
			if
				self.constant.t == "dec" or
				self.constant.t == "oct" or
				self.constant.t == "hex" then
				return tonumber(self.constant.v)
			end
		end
		error("not reached")
  end,
  __tostring = function(self)
		if self.constant then
			return tostring(self.constant.v)
		elseif self.id then
			return tostring(self.id)
		elseif self.op then
			local op = self.op
			if op == "mul" then
				return tostring(self.left) .. " * " .. tostring(self.right)
			elseif op == "div" then
				return tostring(self.left) .. " / " .. tostring(self.right)
			elseif op == "mod" then
				return tostring(self.left) .. " % " .. tostring(self.right)
			elseif op == "add" then
				return tostring(self.left) .. " + " .. tostring(self.right)
			elseif op == "sub" then
				return tostring(self.left) .. " - " .. tostring(self.right)
			elseif op == "asgn" then
				return tostring(self.left) .. " = " .. tostring(self.right)
			else
				dump(self)
			end
		elseif self.un then
			local un = self.un
			if un == "unm" then
				return "-" .. tostring(self.right)
			else
				dump(self)
			end
		else
			error("not reached")
		end
  end,
  
  __mul = function(a, b)
		return setmetatable({
			op = "mul",
			left = a,
			right = b,
		}, expr_meta)
  end,
  __div = function(a, b)
		return setmetatable({
			op = "mul",
			left = a,
			right = b,
		}, expr_meta)
  end,
  __add = function(a, b)
		return setmetatable({
			op = "add",
			left = a,
			right = b,
		}, expr_meta)
  end,
  asgn = function(a, b)
		return setmetatable({
			op = "asgn",
			left = a,
			right = b,
		}, expr_meta)
  end,
  __unm = function(a)
		return setmetatable({
			un = "unm",
			right = a,
		}, expr_meta)
  end, 
 
	finalize = function(self)
		--dump(self)
		--print(tostring(self))
		return self
	end,
	gen_constant = function(t, v)
		return setmetatable({
			constant = {t = t, v = v},
		}, expr_meta)
	end,
	gen_id = function(name)
		return setmetatable({
			id = name,
		}, expr_meta)
	end,
}

