Source = Class("Source", {
  repr = function(self, indent)
    local tab = {}

    for k, v in ipairs(self) do
      tab[#tab+1] = v:repr(indent)
    end

    return concat(tab, "\n")
  end,
},
function(S, env, tree)
  local self = mktab(env, tree, {}, S)
  for k, v in ipairs(tree) do
    if v.funcdef then
      self[#self+1] = Function(env, v)
    elseif v.decl or v.tdef or v.enum or v.struct or v.union then
      self[#self+1] = Declaration(env, v)
    elseif v.ns then
      self[#self+1] = Namespace(env, v)
    else
      dump(v)
      tassert(tree._m, false, "AST/Source NIY")
    end
  end
  self.env = env
  return self
end)
