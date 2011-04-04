local function Class(name, meta, call)
  meta.__index = meta
  meta.tag = name

  return setmetatable(
    meta,
    {
      __call = call or function(self, ...)
        local t = self.constructor(...)
        return setmetatable(t, self)
      end
    }
  )
end

return Class
