# utility function to use throughout

percent <- function(num, digits = 2, multiplier = 100, ...) {
  percentage <- formatC(num * multiplier, format = "f", digits = digits, ...)

  # appending "%" symbol at the end of 
  # calculate percentage value 
  paste0(percentage, "%") 
}

dec_places <- function(num, digits = 2, ...) {
  number <- format(round(num, digits), nsmall = digits)

  number
}
#############################################################################
# MongoDB
# this is a read only account
HOST = "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
DB = "ebd_mgmt"
# COLLECTION = "ebd_test" # testing
COLLECTION = "ebd" # production
source("ncba_config.r")
# other relevant collections include: blocks and ebd_taxonomy

URI = sprintf(
  paste0("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=",
    "atlas-3olgg1-shard-0&readPreference=primary&ssl=true"),
  ncba_db_user,
  ncba_db_pass,
  HOST,
  DB)

# connect to a specific collection (table)
m <- mongo(
  COLLECTION,
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_spp <- mongo(
  "ebd_taxonomy",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_users <- mongo(
  "ncba_reviewers",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_blocks <- mongo(
  "blocks",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

aggregate_ebd_data <- function(pipeline) {
  # Perform aggregation on ebd collection in MongoDB Atlas implementation
  #
  # Description:
  #   Returns records resulting from the passed aggregation pipeline
  #
  # Arguments:
  # pipeline -- valid JSON formatted aggregation pipeline

  mongodata <- m$aggregate(pipeline)
  return(mongodata)
}

#############################################################################
# Get User List
user_list <- function() {
  result <- m_users$find('{}','{}')
  return(result)
}
user_base <- user_list()

#############################################################################
# Get Species List
get_spp_list <- function(query = "{}", filter = "{}" ) {

  mongodata <- m_spp$find(query, filter)

  return(mongodata)
}

species_list <- sort(
  get_spp_list(
    query = '{"NC_STATUS":"definitive"}',
    filter = '{"PRIMARY_COM_NAME":1}'
  )$PRIMARY_COM_NAME, decreasing = FALSE
)
species_list <- c("", species_list) # add blank for select list

#############################################################################
# Breeding Categories and Colors

## BREEDING CODE LISTS
breeding_codes <- read.csv("breeding_codes.csv")
# breeding_codes <- breeding_codes %>%
#   mutate(
#     select_label = paste0(description, " (", code, ")")
#   )
code_levels_list <- factor(
  c(
    "", "No Change", "F", "H", "S", "S7", "M", "P", "T", "C", "N", "A", "B",
    "PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY"
  )
)
code_levels_boxplot <- c(
    "F", "H", "S", "S7", "M", "P", "T", "C", "N", "A", "B",
    "PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY"
  )

# breeding_category_names <- unique(breeding_codes$category_name)

get_breeding_category <- function(code) {
  result <- breeding_codes[breeding_codes$code == code,]$category
  return(result)
}

# breeding_categories <- breeding_category_names
# used to develop palette - for the map
breeding_categories <- c(
  "Confirmed",
  "Probable",
  "Possible",
  "Observed"
)

# used in boxplot
categorycolors <- c(
  "Observed" = "#f2f0f7",
  "Possible" = "#cbc9e2",
  "Probable" = "#9e9ac8",
  "Confirmed" = "#6a51a3"
)

# get_category <- function(BREEDING_CODE) {
#   return(
#     breeding_codes[breeding_codes$code == BREEDING_CODE,]$category_name
#   )
# }

# get_category_color <- function(BREEDING_CODE) {
#   return(categorycolors[get_category(BREEDING_CODE)])
# } 


#############################################################################
# Review Record Management

update_review_record <- function(guids, update_code) {
  print("update_review_record runs!")
  # filter <- paste0(
  #   '{"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : "', guid, '"}'
  # )
  # compile filter list
  guid_list <- '['
  for (o in guids) {
    guid_list <- paste0(guid_list, '"', o, '",')
  }
  guid_list <- substr(guid_list, 1, nchar(guid_list) - 1)
  guid_list <- paste0(guid_list, "]")
  filter <- paste0(
    '{"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : {"$in" :', guids, '}}'
  )

  array_filter <- paste0(
    '[{"elem.GLOBAL_UNIQUE_IDENTIFIER" : {"$in" : ', guids, '}}]')

  result <- m$update(
    query = filter,
    update = update_code,
    filters = array_filter,
    multiple = FALSE
  )

  return(result)
}

#############################################################################
# Observations

get_observations <- function(species) {
  # Perform aggregation on ebd collection in MongoDB Atlas implementation
  #
  # Description:
  #   Returns all records with breeding codes in the NCBA Portal/Project
  #
  # Arguments:
  # species -- common name of the species records to be retrieved

  pipeline <- paste0('[
    {
      "$match" : {
        "PROJECT_CODE" : "EBIRD_ATL_NC",
        "PRIORITY_BLOCK" : "1",
        "OBSERVATIONS.COMMON_NAME" : "', species, '"
      }
    },
    {
      "$unwind" : {
        "path" : "$OBSERVATIONS"
      }
    },
    {
      "$match" : {
        "OBSERVATIONS.BREEDING_CODE" : {"$ne": ""},
        "OBSERVATIONS.COMMON_NAME" : "', species, '"
      }
    },
    {
      "$project" : {
        "JULIAN_DAY" : "$NCBA_JULIAN_DAY",
        "BREEDING_CODE" : "$OBSERVATIONS.BREEDING_CODE",
        "SEI" : "$_id",
        "GUID" : "$OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER",
        "LATITUDE" : 1,
        "LONGITUDE" : 1,
        "LOCALITY" : 1,
        "OBSERVATION_DATE" : 1,
        "COUNTY" : 1,
        "NCBA_REVIEW" : 1,
        "_id" : 0
      }
    }
  ]')

  results <- aggregate_ebd_data(pipeline)
  if (nrow(results) > 0) {
    # ADD BREEDING CATEGORY
    results$BREEDING_CATEGORY <- breeding_codes$category_name[
      match(results$BREEDING_CODE, breeding_codes$code)
    ]

    # add ecoregion if county present
    results <- add_ecoregion_to_df(results)

    # ADD EBIRD LINK AND JOIN WITH SPECIES CODES
    results <- results %>%
      mutate(
        EBIRD_LINK = paste0("https://ebird.org/checklist/", SEI)
      )
    # add species code review data
    results <- add_species_codes_to_df(results, species)
    }


  return(results)
}

default_obs_project <- paste0('{
    "SEI" : "$SAMPLING_EVENT_IDENTIFIER",
    "GUID" : "$OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER",
    "OBSERVATION_DATE" : 1,
    "JULIAN_DAY" : "$NCBA_JULIAN_DAY",
    "TIME_OBSERVATIONS_STARTED" : 1,
    "COUNTY" : 1,
    "ID_NCBA_BLOCK" : 1,
    "OBSERVER_ID" : 1,
    "NCBA_OBSERVER" : 1,
    "PROTOCOL_TYPE" : 1,
    "DURATION_MINUTES" : 1,
    "EFFORT_DISTANCE_KM" : 1,
    "NUMBER_OBSERVERS" : 1,
    "COMMON_NAME" : "$OBSERVATIONS.COMMON_NAME",
    "SCIENTFIC_NAME" : "$OBSERVATIONS.SCIENTIFIC_NAME",
    "BREEDING_CODE" : "$OBSERVATIONS.BREEDING_CODE",
    "BREEDING_CATEGORY" : "$OBSERVATIONS.BREEDING_CATEGORY",
    "BEHAVIOR_CODE" : "$OBSERVATIONS.BEHAVIOR_CODE",
    "SPECIES_COMMENTS" : "$OBSERVATIONS.SPECIES_COMMENTS",
    "HAS_MEDIA" : "$OBSERVATIONS.HAS_MEDIA",
    "LOCALITY" : 1,
    "LATITUDE": 1,
    "LONGITUDE" : 1,
    "NCBA_REVIEW" : 1,
    "_id" : 0
  }'
)

# get_obs_record <- function(GUID, project = default_obs_project) {
#   # Retrieve observation data record
#   #
#   # Description:
#   #   Returns records resulting from the passed aggregation pipeline
#   #
#   # Arguments:
#   # GUID -- valid GLOBAL_UNIQUE_IDENTIFIER
#   # project -- valid JSON of fields to be returned

#   pipeline <- paste0(
#     '[
#       {"$match" : {"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : "', GUID, '"}},
#       {"$unwind" : {"path" : "$OBSERVATIONS"}},
#       {"$match": {"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : "', GUID, '"}},
#       {"$project": ', project, '}
#     ]'
#   )
#   results <- aggregate_ebd_data(pipeline)

#   # add ecoregion if county present
#   results <- add_ecoregion_to_df(results)

#   # ADD EBIRD LINK AND JOIN WITH SPECIES CODES
#   results <- results %>%
#     mutate(
#       EBIRD_LINK = paste0("https://ebird.org/checklist/", SEI)
#     )
#   # add species code review data
#   results <- add_species_codes_to_df(results, species)

#   return(results[1,])

# }
get_obs_records <- function(guids, species, project = default_obs_project) {
  # Retrieve observation data record
  #
  # Description:
  #   Returns records resulting from the passed aggregation pipeline
  #
  # Arguments:
  # GUID -- list of guids
  # project -- valid JSON of fields to be returned

  # compile filter list
  guid_list <- '['
  for (o in guids) {
    guid_list <- paste0(guid_list, '"', o, '",')
  }
  guid_list <- substr(guid_list, 1, nchar(guid_list) - 1)
  guid_list <- paste0(guid_list, "]")

  pipeline <- paste0(
    '[
      {"$match" : {"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : {
      "$in" : ', guid_list, '}}},
      {"$unwind" : {"path" : "$OBSERVATIONS"}},
      {"$match": {"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : {
      "$in" : ', guid_list, '}}},
      {"$project": ', project, '}
    ]'
  )

  results <- aggregate_ebd_data(pipeline)

  # add ecoregion if county present
  results <- add_ecoregion_to_df(results)

  # ADD EBIRD LINK AND JOIN WITH SPECIES CODES
  results <- results %>%
    mutate(
      EBIRD_LINK = paste0("https://ebird.org/checklist/", SEI)
    )
  # add species code review data
  results <- add_species_codes_to_df(results, species)
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # add REVIEW_STATUS field
  # R = reviewed (NCBA_REVIEW present)
  # U = unreviewed (NCBA_REVIEW not present)

  # add FLAG field - used to format table, map, boxplot points
  # all combos of the following:
  # outside safe dates
  # outside breeding range
  # flagged spp/code combo
  # unsuitable spp/code combo

  # REVIEW_STATUS = R should trump any other formatting

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  return(results)

}

#############################################################################
# BLOCKS

get_blocks <- function() {
  filter <- paste0(
    '{
      "ID_NCBA_BLOCK" : 1,
      "COUNTY" : 1,
      "ID_BLOCK_CODE" : 1,
      "PRIORITY" : 1,
      "ID_EBD_NAME" : 1,
      "ECOREGION" : 1
    }'
  )

  results <- m_blocks$find('{}', filter)

  return(results)
}

block_data <- get_blocks()
county_ecoregion <- block_data %>%
  distinct(COUNTY, ECOREGION) %>%
  filter(COUNTY != "") %>%
  mutate(COUNTY_TITLE = str_to_title(COUNTY))


add_ecoregion_to_df <- function(df) {

  if ("COUNTY" %in% names(df)) {
    df$ECOREGION <-
      county_ecoregion$ECOREGION[match(
        df$COUNTY, county_ecoregion$COUNTY_TITLE
      )]
  }
  return(df)
}

add_species_codes_to_df <- function(df, species) {
  selected_species_codes <- species_codes[
    species_codes$SPECIES == species,
  ]
  if (
    "ECOREGION" %in% names(df) &&
    "BREEDING_CODE" %in% names(df)
    ) {
      df <- df %>%
        merge(
          selected_species_codes,
          by = c("BREEDING_CODE", "ECOREGION")
        )
    }

  return (df)
}

## BBA REVEIW REASONS

bba_review_reasons <- read.csv("bba_review_reasons.csv")
bba_review_reason_list <- c("", bba_review_reasons$reason)


species_codes <- read.csv("ncba_species_codes.csv")

