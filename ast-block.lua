
Block = Class("Block", {
  add_decl = function(self, decl)
    self[#self+1] = decl
  end,
  add_stmt = function(self, decl)
    self[#self+1] = decl
  end,
  constructor = function(env, tree)
    --dump(tree, nil, nil, nil, "block")
    local self = setmetatable({env = env}, Block)

    local function tree_get_labels(env, tree)
      for k, v in ipairs(tree) do
        if v.stmt then
          local stmt = v.stmt
          if stmt.label then
            local lbl = env:label_get_r(stmt.label.value)
            if lbl then
              --dump(lbl)
              tassert(stmt.label, not env:label_get(stmt.label.value))
              lbl.env = env
            else
              --dump(stmt)
              lbl = {}
              lbl.kind = "label"
              lbl.id = stmt.label
              lbl.env = env
              env:label_reg(lbl.id.value, lbl)
            end
          elseif v.stmt.compound then
            tree_get_labels(env, stmt.compound)
          end
        end
      end
    end

    -- register labels in advance
    tree_get_labels(env, tree)
    --dump(env)

    for k, v in ipairs(tree) do
      if v.decl then
        self:add_decl(Declaration(env, v.decl))
      elseif v.stmt then
        self:add_stmt(Statement(env, v.stmt))
      else
        dump(v, true)
        tassert(nil, false)
      end
    end

    return self
  end,
  repr = function(self, indent)
    local tab = {}
    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent .. "  ")
    end
    return "{\n" .. concat(tab, "") .. indent .. "}"
  end,
})

