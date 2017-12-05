#' ---
#' title: "The Cost of True Love (a.k.a. The Tidy — and expensive — Twelve Days of Christmas)"
#' author: "@hrbrmstr"
#' date: "2017-12-05"
#' output:
#'   html_document:
#'     keep_md: true
#'     theme: simplex
#'     highlight: monochrome
#' ---
#+ init, include=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE, dev="png",
                      fig.retina = 2, fig.width = 10, fig.height = 6)

#+ preamble

#' ## The Mission

#' I'm in the market for Christmas presents for my true love, @mrshrbrmstr, and
#' thought I'd look to an age-old shopping list for inspiration Just what would
#' it set me back if I decided to mimic the 12 Days of Christmas in this modern
#' day and age?
#'
#' Let's try to do the whole thing in R (of course!).
#'
#' We'll need to:
#'
#' - Grab the lyrics
#' - Parse the lyrics
#' - Get pricing data
#' - Compute some statistics
#' - Make some (hopefully) pretty charts
#'
#' We'll need some R packages to help us along the way:

#+ libs
library(rprojroot) # where am I?
library(stringi) # better string ops
library(rvest) # web data
library(httr) # web data
library(V8) # work with javascript
library(hrbrthemes) # pretty things
library(colourlovers) # i wanted a christmas palette
library(magick) # magic
library(decapitated) # devtools::install_github("hrbrmstr/decapitated")
library(fuzzyjoin) # data manip
library(tidyverse) # tidyvers

#' I'm also using `knitr`, `kableExtras` and `DT` but provide full (`::`) function
#' reverences vs pollute the namespace (we've got quite a bit in the namespace as
#' it stands).

#+ data

rt <- find_rstudio_root_file()

#' ## The stuff

#' Despite the fact that lyric sites steal content from other sites, they still
#' put up barriers to scrape said grifted content. So, we have to pick a site without
#' scraping restrictions, which can mean we get data but may have to massage it a bit.
#'
#' I found this site and it has what we need, but the content will need work.

decapitated::chrome_shot(
  "http://www.genekeyes.com/12-DAYS/12-days-of-Christmas.html#Lyrics",
  path=file.path(rt, "lyrics.png")
)

pg <- read_html("http://www.genekeyes.com/12-DAYS/12-days-of-Christmas.html#Lyrics")

#' We just want to target the table cell with the English lyrics

html_node(pg, xpath=".//td[contains(., 'On the ninth day of Christmas')]") %>%
  html_text() -> lyrics

#' It's a mess

lyrics

#' Let's clean it up

lyrics %>%
  stri_replace_first_fixed("1", "1\n") %>%            # fix the first '1'
  gsub("\\.([[:digit:]]{1,2})", "\\.\n\\1\n", .) %>%  # we want all digits on a unique line
  gsub("[,:]", "\n", .) %>%                           # things are smushed together and these are good line-break points if you look at the text
  stri_split_lines() %>%                              # break lines into a char vector
  flatten_chr() %>%                                   # "unlist" it safely
  stri_trim_both() %>%                                # clean up whitespace
  discard(`==`, "") -> lyrics                         # get rid of empty lines

#' Much better

lyrics

#' Now, target the days (lines with just a number)
#'
#' This is a commmon idiom: find the start & end positions of "records" to parse & extract

day_starts <- which(stri_detect_regex(lyrics, "^[[:digit:]]+$"))

length(day_starts) == 12 # if `TRUE` we're :thumbsup:

day_ends <- c(day_starts[-1]-1, length(lyrics))

#' Does it work?

map2(day_starts, day_ends, ~{
  day <- lyrics[.x:.y]
})

#' Cool. So, now we need to tidy this up a bit. We ultimately want a data frame with
#' the `day` number, the `amount` of the item given on the day and the `item` itself.
#'
#' We'll build a regex and a table from a set of english value names.

amounts <- c("a", "A", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve")
amt_trans <- set_names(c(1,1:12), amounts)
item_regex <- sprintf("(%s) ([[:graph:] ]+)$", paste0(amounts, collapse="|"))

#' Iterate over each segment containing a record

map2_df(day_starts, day_ends, ~{

  day <- lyrics[.x:.y] # this is just for shorthand use but it's the raw record

  day_number <- as.numeric(day[1]) # it's a char now so we want numeric

  stri_match_first_regex(day, item_regex) %>%                         # this makes a matrix of the extracted regex groups based on the regex we built earlier
    as_data_frame() %>%
    filter(!is.na(V1)) %>%                                            # quick hack to get rid of non-gift enumeration lines
    mutate(day = day_number, V3 = sub(".", "", V3, fixed=TRUE)) %>%   # get rid of trailing "."'s and add a day column
    select(day, amount=V2, item=V3) %>%
    mutate(amount = amt_trans[amount])                                # use the translation table we built to get a numeric amount

}) -> day_items

#' How'd we do?

DT::datatable(day_items)

#' For fun, we'll add categories for the items, which is straightforward with `case_when()`:

mutate(
  day_items,
  category = case_when(
    stri_detect_regex(item, "(bird|hen|geese|part|swan|dove)") ~ "Birds",
    stri_detect_regex(item, "(lord|drummer|ladies|piper|maid)") ~ "People",
    TRUE ~ "Things"
  )
) -> day_items

#' Said categories will also help us color things (a 3-color palette is easy, a decent
#' 12-color palette is not so easy, so we'll color by category when possible).

#' Still looking good?

DT::datatable(day_items)

#' Let's grab a decent palette

xmas <- colourlovers::clpalette(2545144)

#+ xmas_palette
plot(xmas)

#' Let's look first at the daily item count exchanged. I'm usually #notafan of
#' stacked bar charts but I think they work OK here.

group_by(day_items, item) %>%
  mutate(item_cum = cumsum(amount)) %>%
  ungroup() -> day_item_sums

colrs <- set_names(swatch(xmas)[[1]][c(1,3,5)], c("Birds", "People", "Things"))

#+ daily_item_count
mutate(day_item_sums, item = factor(item, levels=rev(unique(item)))) %>%
  ggplot(aes(x=day, y=item_cum, group=item, fill=category)) +
  geom_col(position="stack", color="#2b2b2b", size=0.25) +
  scale_x_comma(limits=c(0, 13), breaks=1:12) +
  scale_y_continuous(expand=c(0,0.5)) +
  scale_fill_manual(values=colrs) +
  labs(x="Day", y="# Items", title="Daily item count exchanged") +
  theme_ipsum_rc(grid="Y", axis="x")

#' How about the same by category?

#+ daily_item_count_by_cateogry
count(day_items, day, category, wt=amount) %>%
  arrange(day) %>%
  group_by(category) %>%
  mutate(cat_cum = cumsum(n)) %>%
  ungroup() %>%
  mutate(category = factor(category, levels=rev(c("Birds", "People", "Things")))) %>%
  ggplot(aes(x=day, y=cat_cum, group=category, fill=category)) +
  geom_col(position="stack", color="#2b2b2b", size=0.25) +
  scale_x_comma(limits=c(0, 13), breaks=1:12) +
  scale_y_continuous(expand=c(0,0.5)) +
  scale_fill_manual(values=colrs) +
  labs(x="Day", y="# Items", title="Daily item count exchanged (by category)") +
  theme_ipsum_rc(grid="Y", axis="x")

#' Here's the total #'s for each item at the end of the 12 days:

count(day_items, item, wt=amount) %>%
  arrange(desc(n)) %>%
  knitr::kable(align="rl", format="html") %>%
  kableExtra::kable_styling(full_width=FALSE)

#' ## The cost

#' PNC bank computes the annual costs of the 12 Days of Christmas each year and
#' publishes a fun (for a financial institution) [Christmas Price Index](https://www.pnc.com/en/about-pnc/topics/pnc-christmas-price-index.html).
decapitated::chrome_shot(
  "https://www.pnc.com/en/about-pnc/topics/pnc-christmas-price-index.html",
  path=file.path(rt, "png.png")
)

#' We can work with the raw data (they encourage using this site/data in educational contexts) whic
#' can be found in a javascript file that gets loaded with the page. Use Developer Tools
#' and refresh the page with Developer Tools up and examine the tabs to see where this comes from.

res <- GET("https://www.pnc.com/content/dam/pnc-com/code_assets/cpi/cpi-chart-data.js")

decapitated::chrome_shot(
  "https://www.pnc.com/content/dam/pnc-com/code_assets/cpi/cpi-chart-data.js",
  path=file.path(rt, "viewsrc.png")
)

#' But, we have a problem. It's javascript, not JSON or XML. Thankfully, it's
#' literally just data declarations and no DOM machinations, so we can use the `V8` package
#' to pass the javascript to the V8 engine for execution and then marshal the data back to R.

ctx <- v8()
ctx$eval(content(res, as="text", encoding="UTF-8"))

str(data_init <- ctx$get("dataInit"))

str(data_group <- ctx$get("dataGroup"))

#' We need to do a bit of work to make it usabe:

set_names(data_group$data, data_group$label) %>%
  bind_cols() %>%
  mutate(year = data_init$labels) %>%
  gather(item_display_name, cost, -year) %>%
  mutate(item = tolower(gsub("-", " ", item_display_name, fixed=TRUE))) -> annual_item_costs

DT::datatable(annual_item_costs)

#' We have another problem, tho. The gift names aren't perfect matches and we'll want to use the values
#' with our previous data work:

sort(unique(annual_item_costs$item))

sort(unique(day_items$item))

#' We'll worry about that in a bit.
#'
#' First, let's see the individual and prices going back to when PNC started tracking this:

#+ pnc_price_index, fig.width=11, fig.height=9
mutate(annual_item_costs, item_display_name = factor(item_display_name, unique(item_display_name))) %>%
  mutate(year = as.Date(sprintf("%s-12-25", year))) %>%
  mutate(
    category = case_when(
      stri_detect_regex(item, "(bird|hen|geese|part|swan|dove)") ~ "Birds",
      stri_detect_regex(item, "(lord|drummer|ladies|piper|maid)") ~ "People",
      TRUE ~ "Things"
    )
  ) %>%
  filter(item_display_name != "All Gifts") %>%
  ggplot(aes(year, cost, color=category, fill=category)) +
  geom_area(alpha=2/3) +
  geom_point(size=0.1) +
  scale_x_date(expand=c(0,0)) +
  scale_y_continuous(label=scales::dollar) +
  scale_color_manual(values=colrs, guide=FALSE) +
  scale_fill_manual(values=colrs, guide=FALSE) +
  facet_wrap(~item_display_name, scales="free") +
  labs(x=NULL, y="Item cost",
       title="The PNC Christmas Price Index Through the Years",
       subtitle="Adjusted for inflation; Note free-Y scales",
       caption="Source: https://www.pnc.com/en/about-pnc/topics/pnc-christmas-price-index.html") +
  theme_ipsum_rc(grid="XY", strip_text_face = "bold")

#' Now back to the problem. We want to find unit costs for items so we can compute some final
#' data for the last two visualizations. Thankfully, with a bit of string manipulation
#' and the `fuzzyjoin` package, this is super-straigthforward:

count(day_items, item, wt=amount) %>%
  mutate(item = tolower(gsub("-", "", item, fixed=TRUE))) %>%        # minor pre-cleanup work
  stringdist_left_join(filter(annual_item_costs, year == 2017)) %>%  # this does the heavy lifting
  mutate(unit_cost = cost/n) %>%
  select(item=item.x, unit_cost) -> per_unit

DT::datatable(per_unit)

mutate(day_items, item = tolower(gsub("-", "", item, fixed=TRUE))) %>%
  left_join(per_unit) %>%
  mutate(item_total = amount * unit_cost) -> day_item_cost

DT::datatable(day_item_cost)

#' Now, the daily spend on presents:

#+ daily_present_spend
count(day_item_cost, day, wt=item_total) %>%
  ggplot(aes(day, n)) +
  geom_segment(aes(xend=day, yend=0, color="bar"), size=5) +
  scale_x_comma(limits=c(0, 13), breaks=1:12) +
  scale_y_continuous(label=scales::dollar) +
  scale_color_manual(values=swatch(xmas)[[1]][c(1,3,5)], guide=FALSE)  +
  labs(x=NULL, y="$ Spent",
       title="Daily spend on presents",
       subtitle="Based on the PNC Christmas Price Index Through the Years",
       caption="Source: https://www.pnc.com/en/about-pnc/topics/pnc-christmas-price-index.html") +
  theme_ipsum_rc(grid="Y")

#+ cumulative_present_spend
count(day_item_cost, day, wt=item_total) %>%
  mutate(cumsum=cumsum(n)) %>%
  ggplot(aes(day, cumsum, color="spend", fill="spend")) +
  geom_area(alpha=2/3) +
  scale_x_comma(expand=c(0,0), limits=c(1, 12), breaks=1:12) +
  scale_y_continuous(label=scales::dollar, limits=c(0,40000)) +
  scale_color_manual(values=swatch(xmas)[[1]][c(1,3,5)], guide=FALSE)  +
  scale_fill_manual(values=swatch(xmas)[[1]][c(1,3,5)], guide=FALSE)  +
  labs(x=NULL, y="$ Spent",
       title="Cumulative spend on presents",
       subtitle="Based on the PNC Christmas Price Index Through the Years",
       caption="Source: https://www.pnc.com/en/about-pnc/topics/pnc-christmas-price-index.html") +
  theme_ipsum_rc(grid="XY")

#' And, here's the final cost for this year
count(day_item_cost, wt=item_total)

#' Needless to say, @mrshrbrmstr will be getting something else for Christmas this year.
#'
#' You can find the entire project folder [on GitHub](https://github.com/hrbrmstr/twelvedays)
