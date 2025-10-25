# Set up ----
## Packages
library(httr)
library(xml2)
library(stringr)


# Q1: How many Cov-2 papers ----
## website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

## counts
counts <- xml2::xml_find_first(website, "//span[@class='value']")

## turn into text
counts <- as.character(counts)

## extracting data using regex
totalcount <- str_extract(counts, "[0-9,]+")

## remove commas to convert to numeric
totalcount <- gsub(",", "", totalcount)
totalcount <- as.numeric(totalcount)
print(totalcount)

# Q2: Article abstracts and authors ----
## text file
abstracts <- readLines('~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/PM566-Repository/Lab/Lab 7/Lab 7/data/abstract-sars-cov-2-set.txt', warn = FALSE)

## combine all text into one character
abstracts <- paste(abstracts, collapse = '\n')

## split text whenever 3 new lines occur in a row (two blank lines)
abstracts <- unlist(strsplit(abstracts, split = '\n\n\n'))

## replace any remaining "\n" symbols with spaces
abstracts <- gsub("\n", " ", abstracts)

## replace multiple spaces with single space
abstracts <- gsub(" +", " ", abstracts)

# Q3: Top 10 common institutions ----
institution <- str_extract_all(
  abstracts,
  "([[:alpha:]-]+\\s+University|University\\s+of\\s+[[:alpha:]-]+|[[:alpha:]-]+\\s+Institute\\s+of\\s+[[:alpha:]-]+)"
) 
institution <- unlist(institution)

table(institution)

head(sort(table(institution), decreasing = TRUE), 10)


# Q4: Tidy dataset ----
abstracts <- readLines('~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/PM566-Repository/Lab/Lab 7/Lab 7/data/abstract-sars-cov-2-set.txt', warn = FALSE)
abstracts <- paste(abstracts, collapse = '\n')
abstracts <- unlist(strsplit(abstracts, split = '\n\n\n'))

journal <- str_extract(abstracts, "^[0-9]+\\.\\s[^\n]+")

titles <- sapply(abstracts, function(x){
  unlist(strsplit(x, split = "\n\n"))[2]
}, USE.NAMES = FALSE)

authors <- sapply(abstracts, function(x){
  unlist(strsplit(x, split = "\n\n"))[3]
}, USE.NAMES = FALSE)

affiliations <- str_extract(abstracts, "Author information:[\\s\\S]*?(?=\\n\\n|$)")

papers <- data.frame(
  Journal = journal,
  Title = titles,
  Authors = authors,
  Affiliations = affiliations,
  stringsAsFactors = FALSE
)

kable(papers[1:5, ])
