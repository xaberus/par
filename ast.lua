
return function(env, tree)
  for k,v in ipairs(tree) do
    if v.funcdef then
      print(clex.Function(env, v):repr(""))
    elseif v.decl or v.tdef or v.enum or v.struct or v.union then
      print(clex.Declaration(env, v):repr(""))
    else
      dump(v)
      assert(false, "AST/nr")
    end
  end
end
