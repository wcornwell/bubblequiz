-- Lua filter: replace Unicode subscripts and math symbols with LaTeX equivalents
local text_subs = {
  ["₀"] = "\\textsubscript{0}",
  ["₁"] = "\\textsubscript{1}",
  ["₂"] = "\\textsubscript{2}",
  ["₃"] = "\\textsubscript{3}",
  ["α"] = "$\\alpha$",
  ["β"] = "$\\beta$",
  ["≈"] = "$\\approx$",
  ["≥"] = "$\\geq$",
  ["≤"] = "$\\leq$",
  ["×"] = "$\\times$",
}

-- Characters that need escaping inside \texttt{}
local latex_escape = {
  ["#"] = "\\#", ["$"] = "\\$", ["%"] = "\\%",
  ["&"] = "\\&", ["{"] = "\\{", ["}"] = "\\}",
  ["^"] = "\\textasciicircum{}", ["~"] = "\\textasciitilde{}",
  ["\\"] = "\\textbackslash{}",
}

function Str(el)
  local changed = false
  local s = el.text
  for k, v in pairs(text_subs) do
    local new = s:gsub(k, v)
    if new ~= s then s = new; changed = true end
  end
  if changed then return pandoc.RawInline("latex", s) end
end

function Code(el)
  local s = el.text
  -- Escape LaTeX special chars first
  for k, v in pairs(latex_escape) do
    s = s:gsub(k:gsub("[%(%)%.%%%+%-%*%?%[%^%$]", "%%%1"), v)
  end
  -- Then replace Unicode with LaTeX
  local changed = false
  for k, v in pairs(text_subs) do
    local new = s:gsub(k, v)
    if new ~= s then s = new; changed = true end
  end
  if changed then
    return pandoc.RawInline("latex", "\\texttt{" .. s .. "}")
  end
end
