function indent(lvl)
  return string.rep ("  ", lvl)
end

function tagdump(obj, depth, lvl, unfold, fp)
  -- [[
  --if not dump_subtree(obj) then
    do
      fp:write(indent(lvl) .. tostring(obj["tag"]).. " = {\n")
      for k,v in pairs(obj) do
        if k ~= "tag" and string.byte(k,1) ~= 95 then
          if type(v) == "table" then
            if v.tag and (not unfold) then
              if v.tag == "token" then
                if k == "identifier" then
                  dump(v.value,depth,lvl+1, unfold, fp)
                else
                  fp:write(indent(lvl+1) ..  tostring(k) .. "\n")
                end
              else
                tagdump(v,depth,lvl+1,unfold, fp)
              end
            else
              fp:write(indent(lvl) .. tostring(k) .. " = \n")
              dump(v, depth, lvl+1, unfold, fp)
            end
          else
            fp:write(indent(lvl+1) .. tostring(k) .. " = [[" .. tostring(v) .. "]]\n")
          end
        end
      end
      fp:write(indent(lvl) .. "}\n")
    end
  --end
  fp:flush()
end

function dump(obj, depth, lvl, unfold, fp)
  if not lvl then lvl = 1 end
  if not fp then fp = io.stdout end
  
  if type(obj) == "table" then
    if obj.tag then
      tagdump(obj,depth,lvl,unfold, fp)
    else
      if lvl == 1 then
        fp:write("{\n")
      end

      for k,v in pairs(obj) do
        if type(v) == "table" then
          if depth and depth == lvl then
            fp:write(indent(lvl) .. tostring(k) .. " table\n")
          else
            if v.tag and (not unfold) then
              tagdump(v,depth,lvl+1, unfold, fp)
            else
              if type(k) == "number" then
                fp:write(indent(lvl) .. string.format("[%d]", k).. " = {\n")
              else
                fp:write(indent(lvl) .. tostring(k) .. " = {\n")
              end
              dump(v,depth, lvl+1, unfold, fp)
            end
            fp:write(indent(lvl) .. "}\n")
          end
        else
          fp:write(indent(lvl) .. tostring(k) .. " = " .. tostring(v) .. "\n")
        end
      end

      if lvl == 1 then
        fp:write("}\n")
      end
    end
  else
      fp:write(indent(lvl) .. tostring(obj) .. "\n")
  end

  return obj
end

