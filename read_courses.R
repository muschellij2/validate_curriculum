library(rvest)
library(dplyr)
doc = read_html(
	"fall_jhsph_courses.html")
tabs = html_table(doc, fill = TRUE)
tabs = lapply(tabs, function(x) {
	names(x) = unlist(x[1,])
	x = x[-1, ]
	x
})
tab = bind_rows(tabs)
tab = tab %>% 
	rename(class = `Class #`, 
		title = Title, 
		term = Term,
		location = Location,
		times = `Day-Times`, 
		instructor = `Instructor(s)`,
		status = `Status`)
tab = tab %>% 
	filter(!is.na(class)) %>% 
	mutate(title = sub("\n.*", "", title))