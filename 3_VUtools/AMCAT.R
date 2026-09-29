
.libPaths("/home/oae694/R/", include.site = TRUE)

# This is how you would install AmCAT in your computer:
# remotes::install_github("ccs-amsterdam/amcat4r")

#### Loading the packages ####

library(amcat4r)
library(tidyverse)
library(janeaustenr)

#### Logging in the AmCAT server ####

API_KEY="ak.PNuUCSV992F8WG-NCEnsWOgp8dNIGyts577ooEWiDEo"

amcat_login(server="https://amcat4.labs.vu.nl/api",
            api_key = API_KEY)

#### Listing the indexes ####

list_indexes()$id
get_fields("comptext_sgc_289")$name

#### The following code creates the Austen's books variables ####

JA_BOOKS=austen_books()%>%
  filter(!text=="")%>%
  mutate(ROW=row_number())

## Adding the author

JA_BOOKS$author="Jane Austen"

## Renaming book as title

JA_BOOKS<-JA_BOOKS%>%
  rename(title=book)

## Checking the chapters

CHAPTERS=JA_BOOKS%>%
  filter(str_detect(text,"CHAPTER"))

BREAKS=c(1,CHAPTERS$ROW[2:nrow(CHAPTERS)],nrow(JA_BOOKS))

JA_BOOKS<-JA_BOOKS%>%
  mutate(chapter=cut(ROW,BREAKS,
                     labels=CHAPTERS$text,
                     include.lowest = TRUE))%>%
  select(-ROW)

## Adding the year

YEAR=JA_BOOKS%>%
  filter(str_detect(text,"\\([:digit:]{4}\\)"))%>%
  mutate(year=str_extract(text,"[:digit:]{4}"))%>%
  select(-text,-chapter,-author)
YEAR<-rbind(YEAR,c("Emma",1815),c("Pride & Prejudice",1813))
YEAR<-as.data.frame(YEAR)
row.names(YEAR)<-YEAR$title
JA_BOOKS$year=YEAR[as.character(JA_BOOKS$title),]$year

## Adding the ids

JA_BOOKS<-JA_BOOKS%>%
  group_by(title,chapter)%>%
  mutate(id=paste0(title,row_number()))

#### Uploading the data to AmCAT ####

upload_documents(index = "comptext_sgc_289",
                 documents=JA_BOOKS)


get_fields("comptext_sgc_289")$name



#### Exploring the Jane Austen's books using AmCAT ####

list_indexes()$id

query_documents(index="comptext_sgc_289",
                queries = "temper*",
                fields=c("year", "title"))%>%
  group_by(title,year)%>%
  count()%>%
  ggplot(aes(x=year,y=n,fill=title))+
  geom_col()