Source = Class("Source", {
  constructor = function(env, tree)
    local self = {}
    for k, v in ipairs(tree) do
      if v.funcdef then
        self[#self+1] = Function(env, v)
      elseif v.decl or v.tdef or v.enum or v.struct or v.union then
        self[#self+1] = Declaration(env, v)
      elseif v.ns then
        self[#self+1] = Namespace(env, v)
      else
        dump(v)
        assert(false, "AST/nr")
      end
    end
    self.env = env
    return self
  end,
  repr = function(self, indent)
    local tab = {}

    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent)
    end

    return concat(tab, "\n")
  end,
})
