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

m_users <- mongo(
  "ncba_reviewers",
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


bba_review_reasons <- read.csv("bba_review_reasons.csv")

breeding_codes <- read.csv("breeding_codes.csv")
breeding_codes <- breeding_codes %>%
  mutate(
    select_label = paste0(description, " (", code, ")")
  )

breeding_code_select_list <- breeding_codes %>%
  split(breeding_codes$category_name) %>%
  lapply(function(x) split(x, x$select_label))