
library(readtext)

# Specify the path to the Word document and the search word
docx_path <-"www/taxa_explorer.docx"
search_word <- "Taxa Explorer"

doc <- readtext("www/word_documents/Taxa Explorer.docx")$text

# Split text into parts using new line character:
doc.parts <- strsplit(doc, "\n")[[1]]

search_paragraph <- grep(search_word, doc)[1]
description <- doc.parts[search_paragraph]

