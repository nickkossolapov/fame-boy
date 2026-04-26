-- pandoc-filters.lua
-- Pandoc Lua filter for the Fame Boy blog post
--
-- Usage (from repo root):
--   pandoc docs/blog.md -o docs/index.html --standalone --lua-filter=docs/pandoc-filters.lua --syntax-highlighting=kate

local GITHUB_BASE = "https://github.com/nickkossolapov/fame-boy/tree/main/"

--- Inject blog.css inline and set document metadata
function Meta(meta)
  local script_dir = PANDOC_SCRIPT_FILE and PANDOC_SCRIPT_FILE:match("(.*/)") or ""
  local css_file = script_dir .. "blog.css"

  local TITLE = "I built a Game Boy emulator in F#"
  local DESC = "Hundreds of hours, many late nights, and a working Game Boy emulator in F# with sound, running on desktop and web."
  local URL = "https://nickkossolapov.github.io/fame-boy/building-a-game-boy-emulator-in-fsharp/"
  local IMAGE = URL .. "images/pokemon.gif"

  local f = io.open(css_file, "r")
  if f then
    local css = f:read("*a")
    f:close()
    local style = pandoc.RawBlock("html", "<style>\n" .. css .. "\n</style>")
    local favicon = pandoc.RawBlock("html",
      '<link rel="icon" type="image/x-icon" href="/fame-boy/favicon.ico">\n' ..
      '<link rel="icon" type="image/png" sizes="32x32" href="/fame-boy/favicon-32x32.png">\n' ..
      '<link rel="icon" type="image/png" sizes="16x16" href="/fame-boy/favicon-16x16.png">')
    local og = pandoc.RawBlock("html",
      '<meta name="description" content="' .. DESC .. '">\n' ..
      '<meta property="og:title" content="' .. TITLE .. '">\n' ..
      '<meta property="og:description" content="' .. DESC .. '">\n' ..
      '<meta property="og:type" content="article">\n' ..
      '<meta property="og:url" content="' .. URL .. '">\n' ..
      '<meta property="og:image" content="' .. IMAGE .. '">\n' ..
      '<meta name="twitter:card" content="summary_large_image">')
    meta["header-includes"] = pandoc.MetaBlocks({ style, favicon, og })
  else
    io.stderr:write("Warning: could not open " .. css_file .. "\n")
  end

  meta.pagetitle = TITLE
  meta.lang = "en"
  return meta
end

--- Rewrite relative source links to GitHub and open external links in new tab
function Link(el)
  if el.target:match("^%.%./src/") then
    el.target = el.target:gsub("^%.%./src/", GITHUB_BASE .. "src/")
  elseif el.target:match("^%./src/") then
    el.target = el.target:gsub("^%./src/", GITHUB_BASE .. "src/")
  end

  -- Header anchors are created after Links are processed, so they won't reach here
  el.attributes["target"] = "_blank"
  el.attributes["rel"] = "noopener noreferrer"
  return el
end

function Header(el)
  local id = el.identifier

  if el.level == 1 then
    local open = pandoc.RawBlock("html", "<header>")
    local subtitle = pandoc.RawBlock("html",
      '<p class="subtitle">Nick Kossolapov &middot; April 2026</p>')
    local close = pandoc.RawBlock("html", "</header>")
    return { open, el, subtitle, close }
  end

  -- Wrap header text in an anchor link for all other headings
  local link = pandoc.Link(el.content, "#" .. id, "", { class = "header-anchor" })
  el.content = { link }
  return el
end
