# CitizenScienceLUX (THIS README IS INACCURATE AND A WORK IN PROGRESS)

## Project goal

Analyze iNaturalist data observed in Luxembourg and evaluate biases, gaps, and opportunities

## Project structure

The project is organized into five main sections, numbered according to their position in the analytical workflow and their apparition in the article. Each section acts as a parent script, sourcing smaller scripts dedicated to specific technical operations, data processing steps, or sub-questions.

### 0. Initialization
`Initialization.R` –  Initializes the environment and loads packages
- `config.R` – One should always use a local configuration file so each user uses their own paths for data without the need to change any line of code in any other file. We provide a template.
– `Libraries.R` – Loading all the libraries required; I put it in a single scipt to make it easier to keep it clean and so it is easier to use renv later


### 1. Data
`1_Data.R` – Data acquisition, import, combination, and preparation.

- `CombineObs.R`
- `get_obs.R`
- `CitizenScienceLUX.R`
- `Origin.R`

### 2. Taxonomic
`2_Taxonomic.R` – Taxonomic standardization, identification processing, and richness analyses.

- `Identifications.R`
- `TaxoClusteringSpe.R`
- `CorrectedRichness.R`
- `Accum.R`
- `top500ID.R`

### 3. Spatial
`3_Spatial.R` – Geographical data preparation and spatial analyses.

- `Communes.R`
- `CountryEEZCombine.R`
- `CLC18_5cat.R`
- `LandCover.R`
- `LuxembourgBorders.R`
- `ProtectedAreas.R`
- `RoadData.R`
- `Territory.R`
- - `Accessibility.R`

### 4. User
`4_User.R` – User-level data processing and characterization.

- `UserData.R`
- `UserChar.R`
- `First_observation_user.R`
- `get_inat_obs_user_JW.R`
- `get_all_obs_user_json_JW.R`
- - `Activity.R`

## Additional scripts (SORT)

Scripts for specific figures, exploratory analyses, testing, and supplementary operations that are not necessarily part of the main workflow.

- **Figures and visualization:** `Figure_CommuneStats4x4.R`, `Figure_ObsByWeek.R`, `Figure_iNat_VS_national.R`, `Fig_platform_grade_iconic.R`, `Figure_platform_grade_DiCeccotaxa.R`, `InteractiveMaps.R`, `TreeViz.R`
- **Exploration and testing:** `Exploration_with_Matteo.R`, `Testing.R`, `Testing2.R`, `japg_102916_analysis.R`, `Unused4Paper.R`, `Pres2204.R`
- **Utilities:** `utils.R`, `fun2.R`, `fun3.R`, `fun4.R`

## Non-script files

- `.Rprofile` – Session initialization.
- `.gitignore` – Git exclusions.
- `config.R.example` – Example for a local configuration file.
- `CitizenScienceLUX.Rproj` – RStudio project
- `LICENSE` – License.
- Other `.csv`, `.gpkg`, `.RDS`, `.png`, `.pdf`, and `.html` files – Data, intermediate products, and generated outputs.
