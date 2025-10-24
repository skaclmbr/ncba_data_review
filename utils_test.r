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
COLLECTION = "ebd"
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

# specificy breeding codes and preferred plotting order
# this vector will need updating if any new codes are introduced via "lump".
code_levels_list <- factor(c(
  "Obs",
  "NC",
  "O",
  "F",
  "H",
  "S",
  "S7",
  "M",
  "P",
  "T",
  "C",
  "N",
  "A",
  "B",
  "PE",
  "CN",
  "NB",
  "DD",
  "UN",
  "ON",
  "FL",
  "CF",
  "FY",
  "FS",
  "NE",
  "NY"
))

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

codecategory <- c(
  "F" = "Observed",
  "H" = "Possible",
  "S" = "Possible",
  "S7" = "Probable",
  "M" = "Probable",
  "T" = "Probable",
  "P" = "Probable",
  "C" = "Probable",
  "N" = "Probable",
  "A" = "Probable",
  "B" = "Probable",
  "PE" = "Confirmed",
  "CN" = "Confirmed",
  "NB" = "Confirmed",
  "DD" = "Confirmed",
  "UN" = "Confirmed",
  "ON" = "Confirmed",
  "FL" = "Confirmed",
  "CF" = "Confirmed",
  "FY" = "Confirmed",
  "FS" = "Confirmed",
  "NE" = "Confirmed",
  "NY" = "Confirmed",
  "O" = "Observed",
  "NC" = "Observed",
  "Obs" = "Observed"
  )

get_category <- function(breeding_code) {
  return(codecategory[breeding_code])
}

get_category_color <- function(breeding_code) {
  return(categorycolors[get_category(breeding_code)])
} 

retrieve_observations <- function(species) {
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
          obs_records$breeding_code, levels = code_levels_list, ordered = TRUE
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
retrieve_obs_data <- function(guid, project = default_obs_project) {
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