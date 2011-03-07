function indent(lvl)
  return string.rep ("  ", lvl)
end

local rrr = {}

for i=1,8 do
  rrr[i] = string.format("[1;%dm", 30+i-1)
end


function tagdump(obj, id, depth, lvl, unfold, fp)
  -- [[
  --if not dump_subtree(obj) then
  local green = "[1;32m"
  local yellow = "[1;33m"
  local red = "[1;31m"
  local b = "[1;34m"
  local clear = "[0;m"

  if obj.tag == "token" then
    fp:write(indent(lvl) .. id .. ":" .. rrr[lvl%7+1] .. tostring(obj["tag"])
      .. clear .. " = "..b.."("..clear.."" .. table.concat({"id:", obj.id, ", line:", obj._line, ", value:>", obj.value, "<"})
      ..b..")"..clear.."\n")
  else
    if obj.repr then
      obj:repr(lvl, fp)
    else
      fp:write(indent(lvl) .. id .. ":" .. rrr[lvl%7+1] .. tostring(obj["tag"]) .. clear .. " = "..b.."{"..clear.."\n")
      for k,v in pairs(obj) do
        if k ~= "tag" and string.byte(k,1) ~= 95 then
          if type(v) == "table" then
            if v.tag and (not unfold) then
              tagdump(v,k,depth,lvl+1,unfold, fp)
            else
              fp:write(indent(lvl+1) .. tostring(k) .. " = {\n")
              _dump(v, depth, lvl+2, unfold, fp)
              fp:write(indent(lvl+1) .. "}\n")
            end
          else
            fp:write(indent(lvl+1) .. tostring(k) .. " = >" .. tostring(v) .. "<\n")
          end
        end
      end
      fp:write(indent(lvl) ..b.."}"..clear.."\n")
    end
    fp:flush()
  end
end

function _dump(obj, depth, lvl, unfold, fp)
  if type(obj) == "table" then
    if obj.tag then
      tagdump(obj,"root",depth,lvl,unfold, fp)
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
              tagdump(v,k,depth,lvl, unfold, fp)
            else
              if type(k) == "number" then
                fp:write(indent(lvl) .. string.format("%d", k).. " = {\n")
              else
                fp:write(indent(lvl) .. tostring(k) .. " = {\n")
              end
              _dump(v,depth, lvl+1, unfold, fp)
              fp:write(indent(lvl) .. "}\n")
            end
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

function dump(obj, depth, lvl, unfold, fp)
  if not lvl then lvl = 1 end
  if not fp then fp = io.stdout end 
  
  fp:write(">>>>>>>>>>>>>>>>>>>>>\n")
  _dump(obj, depth, lvl, unfold, fp)
  fp:write("<<<<<<<<<<<<<<<<<<<<<\n")
  fp:flush()
end
