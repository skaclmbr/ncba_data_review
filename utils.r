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
COLLECTION = "ebd_test" # testing
# COLLECTION = "ebd" # production
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

# Get User List
user_list <- function() {
  result <- m_users$find('{}','{}')
  return(result)
}
user_base <- user_list()

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

breeding_categories <- c(
  "Confirmed",
  "Probable",
  "Possible",
  "Observed"
)

categorycolors <- c(
  "Observed" = "#f2f0f7",
  "Possible" = "#cbc9e2",
  "Probable" = "#9e9ac8",
  "Confirmed" = "#6a51a3"
)

get_category <- function(breeding_code) {
  return(codecategory[breeding_code])
}

get_category_color <- function(breeding_code) {
  return(categorycolors[get_category(breeding_code)])
} 

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

update_review_record <- function(guid, update_code) {
  print("update_review_record runs!")
  filter <- paste0(
    '{"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : "', guid, '"}'
  )
  
  array_filter <- paste0('[{"elem.GLOBAL_UNIQUE_IDENTIFIER" : "', guid, '"}]')

  result <- m$update(
    query = filter,
    update = update_code,
    filters = array_filter,
    multiple = FALSE
  )

  return(result)
}


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
        "julian_day" : "$NCBA_JULIAN_DAY",
        "breeding_code" : "$OBSERVATIONS.BREEDING_CODE",
        "sei" : "$_id",
        "guid" : "$OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER",
        "LATITUDE" : 1,
        "LONGITUDE" : 1,
        "LOCALITY" : 1,
        "OBSERVATION_DATE" : 1,
        "_id" : 0
      }
    }
  ]')

  obs_records <- aggregate_ebd_data(pipeline)
  if (nrow(obs_records) > 0) {
    obs_records <- obs_records %>%
      mutate(
        breeding_category = get_category(obs_records$breeding_code)
      ) %>%
      mutate(
        ebird_link = paste0("https://ebird.org/checklist/", sei),
        breeding_code = factor(
          obs_records$breeding_code, levels = codelevels, ordered = TRUE
        )
      )
    }
  return(obs_records)
}

default_obs_project <- paste0('{
    "sei" : "$SAMPLING_EVENT_IDENTIFIER",
    "guid" : "$OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER",
    "OBSERVATION_DATE" : 1,
    "NCBA_JULIAN_DAY" : 1,
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
    "_id" : 0
  }'
)

get_obs_record <- function(guid, project = default_obs_project) {
  # Retrieve observation data record
  #
  # Description:
  #   Returns records resulting from the passed aggregation pipeline
  #
  # Arguments:
  # guid -- valid GLOBAL_UNIQUE_IDENTIFIER
  # project -- valid JSON of fields to be returned

  pipeline <- paste0(
    '[
      {"$match" : {"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : "', guid, '"}},
      {"$unwind" : {"path" : "$OBSERVATIONS"}},
      {"$match": {"OBSERVATIONS.GLOBAL_UNIQUE_IDENTIFIER" : "', guid, '"}},
      {"$project": ', project, '}
    ]'
  )
  results <- aggregate_ebd_data(pipeline)

  return(results[1,])

}

## BLOCKS

get_blocks <- function() {
  filter <- paste0(
    '{
      "ID_NCBA_BLOCK" : 1,
      "COUNTY" : 1,
      "ID_BLOCK_CODE" : 1,
      "PRIORITY" : 1,
      "ID_EBD_NAME" : 1,
      "ECOREGION" : 1,
    }'
  )

  results <- m_blocks$find('{}', filter)

  return(results)
}

# block_data <- get_blocks()
# county_ecoregion <- blocks %>%


# get_ecoregion <- function(county) {
#   return()
# }

## BBA REVEIW REASONS

bba_review_reasons <- read.csv("bba_review_reasons.csv")


## BREEDING CODE LISTS
breeding_codes <- read.csv("breeding_codes.csv")
breeding_codes <- breeding_codes %>%
  mutate(
    select_label = paste0(description, " (", code, ")")
  )

breeding_category_names <- unique(breeding_codes$category_name)

breeding_code_select_list <- split(
  breeding_codes$code,
  breeding_category_names
)
get_breeding_category <- function(code) {
  result <- breeding_codes[breeding_codes$code == code,]$category
  result
}