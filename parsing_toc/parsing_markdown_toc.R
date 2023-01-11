# TODO: incorporate parsing statements containing glue!

## for .Rmd - this can ignore the glue::glue text but won't incorporate the text
## that glue would insert
library(tidyverse)
tibble(text = read_lines("./parsing_toc/example.Rmd")) %>%
  # find your h3 tags to tie the output to
  mutate(h3_cnt = cumsum(str_detect(text, "^### ")),
         # find the titles
         h3 = str_extract(text, "(?<=### ).*") %>% replace_na(""),
         # find the captions
         # caption = str_extract(text, "(?<=caption = \\\").+(?=\\\")") %>% replace_na("")
         caption = 
           str_extract(text, "(?<=caption).+(?=\\\")") %>% 
           str_remove(pattern = "\\s*\\=\\s*(\\w+\\:{0,2}\\w*\\()*\\\"") %>% 
           replace_na("")
  ) %>%
  group_by(h3_cnt) %>%
  summarise(title = paste(h3, collapse = ""),
            caption = paste(caption, collapse = ""))

## using .md file
tibble(text = read_lines("./parsing_toc/example.md")) %>%
  ## preprocess: remove code blocks containing comments -
  ## otherwise they are indistinguishable from markdown headings!
  mutate(
    open_block = cumsum(str_detect(text, "^\\`{3}\\.+")),
    close_block = cumsum(str_detect(text, "^\\`{3}$"))
  ) %>%
  ## quick & dirty: remove lines that are code blocks and start with a '#'
  ## because these indicate comments rather than headings!
  filter(!(open_block>close_block & str_detect(text,"^#+"))) %>%
  mutate(
    h1 = cumsum(str_detect(text, "^# "))
  ) %>% 
  group_by(h1) %>%
  mutate(
    h2 = cumsum(str_detect(text, "^## "))
  ) %>%
  group_by(h1,h2) %>%  # this ensure counts are nested within h1 also
  mutate(
    h3 = cumsum(str_detect(text, "^### "))
  ) %>%
  ungroup %>%
  mutate(
    heading_number = glue::glue("{h1}.{h2}.{h3}"),
    heading_text = str_extract(text, "(?<=### ).*") %>% replace_na(""),
    # find the captions
    caption = 
     str_extract(text, "(?<=\\<caption\\>).+(?=\\<\\\\\\/caption\\>)") %>% 
     replace_na("")
  ) %>%
  distinct %>%
  group_by(h1,h2,h3) %>%
  summarise(heading = unique(heading_number),
            title = paste(heading_text, collapse = ""),
            caption = paste(caption, collapse = "")
            ) %>%
  ## keep only 'substantial' lines - containing both 3rd level heading and
  ## caption
  filter(title!="" & caption !="")
