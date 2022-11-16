Overview
------------

This is the accompanying code repository for the paper Weigert et al. (2022) - ''Association of vaccine-induced or hybrid immunity with COVID-19-related mortality during the Omicron wave - a retrospective observational study in elderly Bavarians''.

The code contains the R code for all relevant analyses in the paper, including the supplementary material.

Folder structure:

- Code: Code for the main analyses and the sensitivity analyses, both building on functions in `Functions.R`
- Graphics: Figures contained in the manuscript and the supplementary material. The code for Figure 1 is not part of this repository.
- Models: Estimated model objects

<br/><br/>


Data dictionary
---------------

The data we used for our analyses are structured as follows:

Original variables:
- Meldedatum: date when the infection was recorded
- Erkrankungsbeginn: date when first symptoms occurred
- AlterKat: age category in years (0-5 / 6-11 / 12-15 / 16-19 / 20-34 / 35-59 / 60-64 / 65-69 / 70-74 / 75-79 / 80-84 / 85-89 / 90+)
- Geschlecht: sex of the infected individual (weiblich = female, männlich = male, divers = diverse, nicht ermittelbar = unascertainable, nicht erhoben = not recorded)
- impfung_datum: date of last vaccination
- impfstatus: level of immunity (ungeimpft = unvaccinated, unvollstaendig grundimmunisiert = incomplete primary immunization, grundimmunisiert = complete primary immunization, geboostert = boosted immunization, unplausible oder unzureichende Angaben = implausible record, keine Angabe = unknown)
- reinfektion: reinfection (Ja = yes, Nein = no)
- VerstorbenDatum: date of death
- VerstorbenStatus: death of the infected individual (Ja = yes, Nein = no, nicht ermittelbar = unascertainable, nicht erhoben = not recorded)
- VerstorbenGrund: cause of death (an der gemeldeten Krankheit = COVID-19 (direct), aufgrund anderer Ursache = other cause (indirect), nicht ermittelbar = unascertainable (unknown), nicht erhoben = not recorded (unknown))

Newly created variables:
- Erkrankungsdatum_surv: earliest date of documented infection (first occuring date of Erkrankungsbeginn and Meldedatum)
- Erkrankungsdatum_num: numerical version of Erkrankungsdatum_surv (starting with 2022-01-01 as time origin)
- Impfstatus_surv: level of immunity with seven categories (ungeimpft = unvaccinated, unvollstaendig grundimmunisiert = incomplete primary immunization, grundimmunisiert = complete primary immunization earlier than six months before infection, grundimmunisiert_within_6months = complete primary immunization later than six months before infection, geboostert = boosted immunization earlier than three months before infection, geboostert_within_3months = boosted immunization later than three months before infection, keine Angabe = unknown or implausible records)
- Alter_surv_num: numerical age variable (center of five-year categories)
- time: days after earliest date of documented infection
- VerstorbenStatus_surv: VerstorbenStatus after 60 days

