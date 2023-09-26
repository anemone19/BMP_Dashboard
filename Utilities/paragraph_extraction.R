
library(readtext)

# Specify the path to the Word document and the search word
docx_path <-"www/word_documents/student_aboutme.docx"
search_word <- "Erica"

doc <- readtext("www/word_documents/student_aboutme.docx")$text

# Split text into parts using new line character:
doc.parts <- strsplit(doc, "\n")[[1]]

search_paragraph <- grep(search_word, doc.parts)[1]
description <- doc.parts[search_paragraph]

