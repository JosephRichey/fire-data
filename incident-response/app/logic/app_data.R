box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "cfddb",
                 host = Sys.getenv("CFDDB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("CFDDB_PASSWORD"))

#' @export
Firefighter <- dbGetQuery(CON,
                          paste0("SELECT * FROM ", Sys.getenv("FIREFIGHTER_TABLE"),
                                 " WHERE firefighter_deactive_date IS NULL"))

#' @export
Apparatus <- dbGetQuery(CON,
                        paste0("SELECT * FROM ", Sys.getenv("APPARATUS_TABLE")))

#' @export
Incident <- dbGetQuery(CON,
                       paste0("SELECT * FROM ", Sys.getenv("INCIDENT_TABLE")))

#' @export
Firefighter_Incident <- dbGetQuery(CON,
                                paste0("SELECT * FROM ", Sys.getenv("FF_INC_TABLE")))


#' @export
Dispatch_Codes <- list(
  Medical = list(
    "Abdominal Pain/Problems",
    "Allergies / Envenomations",
    "Animal Bites / Attacks",
    "Assault / Sexual Assault / Stun Gun",
    "Back Pain",
    "Breathing Problems",
    "Burns / Explosions",
    "Carbon Monoxide / Inhalation / HAZMAT / CBRN",
    "Cardiac or Respiratory Arrest / Death",
    "Chest Pain",
    "Choking",
    "Convulsions / Seizures",
    "Diabetic Problems",
    "Drowning / Diving / SCUBA Accident",
    "Electrocution / Lightning",
    "Eye Problems / Injuries",
    "Falls",
    "Headache",
    "Heart Problems / A.I.C.D.",
    "Heat / Cold Exposure",
    "Hemorrhage / Lacerations",
    "Inaccessible Incident / Entrapments",
    "Overdose / Poisoning (Ingestion)",
    "Pregnancy / Childbirth / Miscarriage",
    "Psychiatric / Suicide Attempt",
    "Sick Person",
    "Stab / Gunshot / Penetrating Trauma",
    "Stroke (CVA) / Transient Ischemic Attack (TIA)",
    "Traffic / Transportation Incidents",
    "Traumatic Injuries",
    "Unconscious / Fainting (Near)",
    "Unknown Problem",
    "Inter-Facility Transfer / Palliative Care",
    "Automatic Crash Notification (A.C.N.)",
    "Pandemic / Epidemic / Outbreak (Surveillance or Triage)"
  ),
  Fire = list(
    "CO Alarm",
    "Fire Alarm",
    "Garage Fire",
    "Grass Fire",
    "HAZMAT",
    "Illegal Burn",
    "Motor Vehicle Accident (Injuries)",
    "Motor Vehicle Accident (No Injuries)",
    "Smoke Investigation",
    "Structure Fire",
    "Vehicle Fire"
  )
)

# Creating mapping vectors to get Ids
#' @export
apparatus_mapping <- stats::setNames(Apparatus$apparatus_id, Apparatus$apparatus_name)

#' @export
firefighter_mapping <- stats::setNames(Firefighter$firefighter_id, Firefighter$firefighter_full_name)