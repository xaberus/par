local function Class(name, meta, call)
  meta.__index = meta
  meta.tag = name

  return setmetatable(
    meta,
    { __call = call }
  )
end

return Class
