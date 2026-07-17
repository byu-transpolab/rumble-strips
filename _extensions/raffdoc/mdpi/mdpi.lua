function Meta(meta)
  local function get_initials(given_str, family_str)
    local initials = {}
    if given_str then
      for word in string.gmatch(given_str, "%S+") do
        table.insert(initials, string.sub(word, 1, 1) .. ".")
      end
    end
    if family_str then
      for word in string.gmatch(family_str, "%S+") do
        table.insert(initials, string.sub(word, 1, 1) .. ".")
      end
    end
    return table.concat(initials, "")
  end

  local roles_map = {}
  local by_author = meta['by-author']
  if not by_author then return meta end

  for _, author in ipairs(by_author) do
    local given = ""
    local family = ""
    if author.name then
      if author.name.given then given = pandoc.utils.stringify(author.name.given) end
      if author.name.family then family = pandoc.utils.stringify(author.name.family) end
    end
    local initials = get_initials(given, family)
    if initials == "" and author.name and author.name.literal then
       initials = get_initials(pandoc.utils.stringify(author.name.literal), "")
    end
    
    if author.roles then
      for _, role_obj in ipairs(author.roles) do
        local r = ""
        if role_obj.role then
           r = pandoc.utils.stringify(role_obj.role)
        else
           r = pandoc.utils.stringify(role_obj)
        end
        r = r:gsub("^%l", string.upper)
        if not roles_map[r] then
          roles_map[r] = {}
        end
        table.insert(roles_map[r], initials)
      end
    else
        if author.role then
           for _, role_obj in ipairs(author.role) do
             local r = pandoc.utils.stringify(role_obj)
             r = r:gsub("^%l", string.upper)
             if not roles_map[r] then
               roles_map[r] = {}
             end
             table.insert(roles_map[r], initials)
           end
        end
    end
  end

  local contribs = {}
  local sorted_roles = {}
  for role, _ in pairs(roles_map) do
     table.insert(sorted_roles, role)
  end
  table.sort(sorted_roles)

  for _, role in ipairs(sorted_roles) do
    local initials_list = roles_map[role]
    local current_role_str = role .. ", "
    if #initials_list == 1 then
      current_role_str = current_role_str .. initials_list[1]
    else
      local last = table.remove(initials_list)
      current_role_str = current_role_str .. table.concat(initials_list, ", ") .. " and " .. last
    end
    table.insert(contribs, current_role_str)
  end

  if #contribs > 0 then
    local final_string = table.concat(contribs, "; ")
    final_string = final_string .. ". All authors have read and agreed to the published version of the manuscript."
    meta.authorcontributions = final_string
  end

  return meta
end