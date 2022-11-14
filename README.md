Overview
------------

This is the accompanying code repository for the paper Weigert et al. (2022) - ''Association of vaccine-induced or hybrid immunity with COVID-19-related mortality during the Omicron wave - a retrospective observational study in elderly Bavarians''.

The code contains the R code for all relevant analyses in the paper, including the supplementary material.

Folder structure:

- Code: Code for the main analyses and the sensitivity analyses, both building on functions in `Functions.R`
- Graphics: Figures contained in the manuscript and the supplementary material. The code for Figure 1 is not part of this repository.
- Models: Estimated model objects


Data Dictionary
---------------

Original variables:
- Meldedatum – date when the infection was recorded
- Erkrankungsbeginn – date when first symptoms occurred
- AlterKat – age category in years (0-5 / 6-11 / 12-15 / 16-19 / 20-34 / 35-59 / 60-64 / 65-69 / 70-74 / 75-79 / 80-84 / 85-89 / 90+)
- Geschlecht – sex of the infected individual (weiblich = female, männlich = male, divers = diverse, nicht ermittelbar = unascertainable, nicht erhoben = not recorded)
- HospitalisierungStatus – hospitalization of the infected individual (Ja = yes, Nein = No, nicht ermittelbar = unascertainable, nicht erhoben = not recorded)
- ExHosp_StayFrom – date of hospitalization
- impfung_datum – date of last vaccination
- impfstatus – level of immunity (ungeimpft = unvaccinated, unvollstaendig grundimmunisiert = incomplete primary immunization, grundimmunisiert = complete primary immunization, geboostert = boosted immunization, unplausible oder unzureichende Angaben = implausible record, keine Angabe = unknown)
- reinfektion - reinfection (Ja = yes, Nein = no)
- VerstorbenDatum - date of death
- VerstorbenStatus - (Ja = yes, Nein = no, nicht ermittelbar = unascertainable, nicht erhoben = not recorded)
- VerstorbenGrund – cause of death (an der gemeldeten Krankheit = COVID-19, aufgrund anderer Ursache = other cause, nicht ermittelbar = unascertainable, nicht erhoben = not recorded)

Derived variables:

