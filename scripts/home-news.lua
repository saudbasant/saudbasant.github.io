function Div(div)
  if div.identifier == "home-news-listing" then
    local file = io.open("_generated/home-news.html", "r")

    if not file then
      return div
    end

    local html = file:read("*all")
    file:close()

    return pandoc.RawBlock("html", html)
  end
end
