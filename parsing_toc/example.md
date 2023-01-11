---
title: "Example of report with tables"
output: 
  html_document:
    keep_md: true
    number_sections: true
date: "2022-11-22"
---
  


# Top level header

## Subheading L2 without much content

### What

### is this

## Subheading

### L3 subheading without a table

Just some lorem ipsum

### Table 1: SW characters shorter than 100

Some lorem ipsum that doesn't need to be in a listing of tables.


```r
starwars %>% filter(height<100) %>%
datatable(caption = "SW characters shorter than 100 units, presumably centimeters.")
```

```{=html}
<div id="htmlwidget-4047855df6cb45855be9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4047855df6cb45855be9">{"x":{"filter":"none","vertical":false,"caption":"<caption>SW characters shorter than 100 units, presumably centimeters.<\/caption>","data":[["1","2","3","4","5","6","7"],["R2-D2","R5-D4","Yoda","Wicket Systri Warrick","Dud Bolt","Ratts Tyerell","R4-P17"],[96,97,66,88,94,79,96],[32,32,17,20,45,15,null],[null,null,"white","brown","none","none","none"],["white, blue","white, red","green","brown","blue, grey","grey, blue","silver, red"],["red","red","brown","brown","yellow","unknown","red, blue"],[33,null,896,8,null,null,null],["none","none","male","male","male","male","none"],["masculine","masculine","masculine","masculine","masculine","masculine","feminine"],["Naboo","Tatooine",null,"Endor","Vulpter","Aleen Minor",null],["Droid","Droid","Yoda's species","Ewok","Vulptereen","Aleena","Droid"],[["The Empire Strikes Back","Attack of the Clones","The Phantom Menace","Revenge of the Sith","Return of the Jedi","A New Hope","The Force Awakens"],["A New Hope"],["The Empire Strikes Back","Attack of the Clones","The Phantom Menace","Revenge of the Sith","Return of the Jedi"],["Return of the Jedi"],["The Phantom Menace"],["The Phantom Menace"],["Attack of the Clones","Revenge of the Sith"]],[[],[],[],[],[],[],[]],[[],[],[],[],[],[],[]]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>name<\/th>\n      <th>height<\/th>\n      <th>mass<\/th>\n      <th>hair_color<\/th>\n      <th>skin_color<\/th>\n      <th>eye_color<\/th>\n      <th>birth_year<\/th>\n      <th>sex<\/th>\n      <th>gender<\/th>\n      <th>homeworld<\/th>\n      <th>species<\/th>\n      <th>films<\/th>\n      <th>vehicles<\/th>\n      <th>starships<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

### Table 2: SW characters taller than 200

Some more lorem ipsum that doesn't need to be in a listing of tables.


```r
starwars %>% filter(height>200) %>%
  datatable(caption = "SW characters taller than 200 units, presumably centimeters.")
```

```{=html}
<div id="htmlwidget-56e4de51d835aa9db5e5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-56e4de51d835aa9db5e5">{"x":{"filter":"none","vertical":false,"caption":"<caption>SW characters taller than 200 units, presumably centimeters.<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10"],["Darth Vader","Chewbacca","Roos Tarpals","Rugor Nass","Yarael Poof","Lama Su","Taun We","Grievous","Tarfful","Tion Medon"],[202,228,224,206,264,229,213,216,234,206],[136,112,82,null,null,88,null,159,136,80],["none","brown","none","none","none","none","none","none","brown","none"],["white","unknown","grey","green","white","grey","grey","brown, white","brown","grey"],["yellow","blue","orange","orange","yellow","black","black","green, yellow","blue","black"],[41.9,200,null,null,null,null,null,null,null,null],["male","male","male","male","male","male","female","male","male","male"],["masculine","masculine","masculine","masculine","masculine","masculine","feminine","masculine","masculine","masculine"],["Tatooine","Kashyyyk","Naboo","Naboo","Quermia","Kamino","Kamino","Kalee","Kashyyyk","Utapau"],["Human","Wookiee","Gungan","Gungan","Quermian","Kaminoan","Kaminoan","Kaleesh","Wookiee","Pau'an"],[["The Empire Strikes Back","Revenge of the Sith","Return of the Jedi","A New Hope"],["The Empire Strikes Back","Revenge of the Sith","Return of the Jedi","A New Hope","The Force Awakens"],["The Phantom Menace"],["The Phantom Menace"],["The Phantom Menace"],["Attack of the Clones"],["Attack of the Clones"],["Revenge of the Sith"],["Revenge of the Sith"],["Revenge of the Sith"]],[[],["AT-ST"],[],[],[],[],[],["Tsmeu-6 personal wheel bike"],[],[]],[["TIE Advanced x1"],["Millennium Falcon","Imperial shuttle"],[],[],[],[],[],["Belbullab-22 starfighter"],[],[]]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>name<\/th>\n      <th>height<\/th>\n      <th>mass<\/th>\n      <th>hair_color<\/th>\n      <th>skin_color<\/th>\n      <th>eye_color<\/th>\n      <th>birth_year<\/th>\n      <th>sex<\/th>\n      <th>gender<\/th>\n      <th>homeworld<\/th>\n      <th>species<\/th>\n      <th>films<\/th>\n      <th>vehicles<\/th>\n      <th>starships<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

## Subheading 2

### Table 2 contains `glue`

Some more lorem ipsum that doesn't need to be in a listing of tables.


```r
starwars %>% filter(height>200) %>%
  datatable(caption = glue::glue("SW characters taller than 200 units, presumably centimeters."))
```

```{=html}
<div id="htmlwidget-8f0686a5b99ac73002b5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8f0686a5b99ac73002b5">{"x":{"filter":"none","vertical":false,"caption":"<caption>SW characters taller than 200 units, presumably centimeters.<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10"],["Darth Vader","Chewbacca","Roos Tarpals","Rugor Nass","Yarael Poof","Lama Su","Taun We","Grievous","Tarfful","Tion Medon"],[202,228,224,206,264,229,213,216,234,206],[136,112,82,null,null,88,null,159,136,80],["none","brown","none","none","none","none","none","none","brown","none"],["white","unknown","grey","green","white","grey","grey","brown, white","brown","grey"],["yellow","blue","orange","orange","yellow","black","black","green, yellow","blue","black"],[41.9,200,null,null,null,null,null,null,null,null],["male","male","male","male","male","male","female","male","male","male"],["masculine","masculine","masculine","masculine","masculine","masculine","feminine","masculine","masculine","masculine"],["Tatooine","Kashyyyk","Naboo","Naboo","Quermia","Kamino","Kamino","Kalee","Kashyyyk","Utapau"],["Human","Wookiee","Gungan","Gungan","Quermian","Kaminoan","Kaminoan","Kaleesh","Wookiee","Pau'an"],[["The Empire Strikes Back","Revenge of the Sith","Return of the Jedi","A New Hope"],["The Empire Strikes Back","Revenge of the Sith","Return of the Jedi","A New Hope","The Force Awakens"],["The Phantom Menace"],["The Phantom Menace"],["The Phantom Menace"],["Attack of the Clones"],["Attack of the Clones"],["Revenge of the Sith"],["Revenge of the Sith"],["Revenge of the Sith"]],[[],["AT-ST"],[],[],[],[],[],["Tsmeu-6 personal wheel bike"],[],[]],[["TIE Advanced x1"],["Millennium Falcon","Imperial shuttle"],[],[],[],[],[],["Belbullab-22 starfighter"],[],[]]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>name<\/th>\n      <th>height<\/th>\n      <th>mass<\/th>\n      <th>hair_color<\/th>\n      <th>skin_color<\/th>\n      <th>eye_color<\/th>\n      <th>birth_year<\/th>\n      <th>sex<\/th>\n      <th>gender<\/th>\n      <th>homeworld<\/th>\n      <th>species<\/th>\n      <th>films<\/th>\n      <th>vehicles<\/th>\n      <th>starships<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
