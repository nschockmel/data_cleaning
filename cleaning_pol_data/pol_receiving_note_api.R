library(tidyverse)

##########################################################################
# this script removes personal identifiable information from po-line 
# receiving notes and creates a csv file of new po-line id/note pairs 
# that can be used with the alma api to update the notes in batch 
##########################################################################

##########################################################################
# pt1: format and export data for use with retrieve po-line api
##########################################################################

path <- "API Datasets/Receiving Note Analysis/"

# load list from receiving team
note_check <- read_csv(paste0(path, "Interested Users Check.csv"))

# write po-lines to .txt output for checking with API
note_check %>%
  select(`PO Line Reference`) %>%
  write_tsv(paste0(path, "pol_ids.txt"), col_names = FALSE)

##########################################################################
# pt2: import api results edit and format data for api put calls
##########################################################################

# import api get results
note_update <- read_tsv(paste0(path, "api_rec_notes.txt"))

# remove lines with no receiving note in data retrieved from alma api
note_update <- note_update %>%
  filter(!is.na(`receiving_note`))

# find notes with netid - these appear to be prefixed by N=,H=, and notify
note_update <- note_update %>%
  mutate(needs_fix = if_else(str_detect(str_to_lower(`receiving_note`), "n=|h=|notify:"), "YES", "NO")) %>%
  filter(needs_fix == "YES")

# remove netids from receiving note 
note_update <- note_update %>%
  mutate(new_note = if_else(str_detect(str_to_lower(`receiving_note`), "h\\="), str_replace_all(`receiving_note`, "(h\\=)|(?<=h\\=)\\S+|(H\\=)|(?<=H\\=)\\S+", ""),
                            if_else(str_detect(str_to_lower(`receiving_note`), "n\\="), str_replace_all(`receiving_note`, "(n\\=)|(?<=n\\=)\\S+|(N\\=)|(?<=N\\=)\\S+", ""),
                                    if_else(str_detect(str_to_lower(`receiving_note`), "notify\\:"), str_replace_all(`receiving_note`, "(notify\\: )|(?<=NOTIFY\\: )\\S+|(notify\\: )|(?<=notify\\: )\\S+", ""),
                                            `receiving_note`))))

# clean up remnant characters
note_update$new_note <- str_squish(note_update$new_note)
note_update$new_note <- str_remove(note_update$new_note, "^\\;")
note_update$new_note <- str_remove(note_update$new_note, "\\;$")
note_update$new_note <- str_squish(note_update$new_note)
note_update$pol_line <- str_squish(note_update$po_line)

##########################################################################
# pt3: export the data as .txt file for use with alma update po-line api
##########################################################################

# select columns and export data
note_update <- note_update %>%
  select(po_line, new_note) %>%
  write_delim(paste0(path, "pols_update.txt"), col_names = FALSE, delim = "^")
