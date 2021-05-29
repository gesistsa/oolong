# oolong 0.4.0

* Add `export_oolong` and `deploy_oolong` for online deployment [thanks Marius Sältzer, Daniel Brady (and his friend Louis), Johannes Gruber and Felicia Loecherbach for testing this feature; thanks SAGE Ocean for the concept grant to support the development of this feature]
* Support models from `seededlda` [thanks Marius Sältzer]
* Support Naive Bayes models from `quanteda.textmodels` [thanks Marius Sältzer]
* Support generation of word set intrusion test (Ying et al. forthcoming)
* Support generation of topic intrusion test only
* Add new wrappers: `wi`, `ti`, `witi`, `wsi`, and `gs`
* Add `userid` as an suggested parameter
* Total revamp of the object of all oolong tests; add more meta data. Add `update_oolong` for updating object created by older versions of oolong
* Update the print method of all oolong tests; it is now based on `cli`
* Various bug fixes; all Shiny components are now automatically tested

# oolong 0.3.11

* Support BTM [thanks Marius Sältzer]
* Update Shiny UI (with jump button)
* Various bug fixes

# oolong 0.3.4

* Initial CRAN version.
