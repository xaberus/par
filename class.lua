local function Class(name, meta)
  meta.__index = meta
  meta.tag = name

  return setmetatable(
    meta,
    {
      __call = function(self, ...)
        local t = self.constructor(...)
        return setmetatable(t, self)
      end
    }
  )
end

return Class
