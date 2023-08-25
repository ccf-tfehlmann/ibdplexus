medication-in-SPARC
================

A Study of a Prospective Adult Research Cohort with Inflammatory Bowel
Disease (SPARC IBD) is a longitudinal study following adult IBD patients
as they receive care at 17 different sites across the United States. To
learn more about SPARC IBD and it’s development please see \[The
Development and Initial Findings of A Study of a Prospective Adult
Research Cohort with Inflammatory Bowel Disease (SPARC IBD)\]
(<https://doi.org/10.1093/ibd/izab071>).

Data from patient reported surveys (eCRFs), IBD Smartform and electronic
medication records (EMR) are integrated into IBD Plexus. The ibdplexus
package was created to synthesize this data into research ready formats.
This vignette focuses on the functions in the ibdplexus package that
handle the medication information in SPARC IBD.

Patient reported surveys, sent out at enrollment and quarterly, ask
patients about their current IBD medication use specifically. Electronic
medical records, when available, contain the prescriptions for each
patient from that institution. They may not be prescribed specifically
for IBD. The Smartform, updated at the time of office visit by the
provider, asks for exposure to specific IBD medications.

This vignette mainly focuses on the medication data from eCRF and EMR.

# Filtering for Medications of Interest

`sparc_med_filter` filters through the eCRF and EMR data for medications
of interest that are listed in the data.frame `med_grp`.

`med grp` contains the medication name of interest, the type of
medication and a standardized name. The MEDICATION_NAME column includes
generic and brand names. the new_med_name column is the standardized
name for each MEDICATION_NAME. Currently, all biosimilars are mapped to
the generic name.

A future iteration will parse out biosimilars into their own group.

``` r

library(ibdplexus, quietly = T)
library(tidyverse, quietly = T)

# Here is the table of the medication types in med_grp
knitr::kable(med_grp %>% distinct(med_type)) 
```

| med_type         |
|:-----------------|
| Biologic         |
| Antibiotics      |
| Immunomodulators |
| Aminosalicylates |
| Corticosteroids  |
| Antidiarrheals   |
| Other            |
| Probiotic        |

``` r

# Here is the first 5 rows of the med_grp data.frame

knitr::kable(head(med_grp, 5)) 
```

| MEDICATION_NAME           | med_type         | new_med_name |
|:--------------------------|:-----------------|:-------------|
| Adalimumab                | Biologic         | Adalimumab   |
| Amoxicillin               | Antibiotics      | Amoxicillin  |
| Amoxicillin / Clavulanate | Antibiotics      | Amoxicillin  |
| Azathioprine              | Immunomodulators | Azathioprine |
| Balsalazide               | Aminosalicylates | Balsalazide  |

``` r

# Here is how Infliximab and its' bio-similars are mapped 

knitr::kable(med_grp %>% filter(new_med_name == "Infliximab"))
```

| MEDICATION_NAME             | med_type | new_med_name |
|:----------------------------|:---------|:-------------|
| Infliximab (Remsima)        | Biologic | Infliximab   |
| Infliximab (Unspecified)    | Biologic | Infliximab   |
| Remicade                    | Biologic | Infliximab   |
| Remsima                     | Biologic | Infliximab   |
| Infliximab                  | Biologic | Infliximab   |
| Infliximab (Remicade)       | Biologic | Infliximab   |
| Infliximab-abda (Renflexis) | Biologic | Infliximab   |
| Infliximab-axxq (Avsola)    | Biologic | Infliximab   |
| Infliximab-dyyb (Inflectra) | Biologic | Infliximab   |
| Renflexis                   | Biologic | Infliximab   |
| Avsola                      | Biologic | Infliximab   |
| Inflectra                   | Biologic | Infliximab   |
| Infliximab-abda             | Biologic | Infliximab   |
| Infliximab-axxq             | Biologic | Infliximab   |
| Infliximab-dyyb             | Biologic | Infliximab   |

To see the entire med_grp data.frame click
<a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/79168cbffa523ab8da142d250133e70f625e00c7/data/med_grp.rda">`here`</a>

`sparc_med_filter` requires the following SPARC domains to be loaded:

-   prescriptions

-   observations

-   demographics

-   encounter

One must also supply the med_groups of interest which corresponds to the
med_type in the med_grp data.frame.

`sparc med filter` subsets the prescriptions table to just the
medication types specified in med_groups. The leading question on
medications is located in the observations table.

In the baseline survey, a patient is asked if they are currently taking
any medications for their IBD. If they answer “No” they are flagged as
having no medications at enrollment. It is important to note that
antibiotics and probiotics are asked about separately in the patient
surveys.

In the quarterly follow-up surveys, a patient is asked if they have had
any changes in their IBD medication. If a patient indicates they have
not had any medication changes, the data from the previous survey is
pulled forward.

`sparc med filter` uses grep to search through the MEDICATION,
OTHER_MEDICATION and SRC_DRUG_CODE_CONCEPT_NAME columns in the
prescription table. If the medication of interest is found, it subsets
the data and maps the medication to the standardized name
(new_med_name).

``` r

# Load EMR and eCRF data necessary for sparc_med_filter 

data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = c("prescriptions", "observations", "demographics", "encounter"), data_type = "Both")

# Filter SPARC IBD data for biologics & immunomodulators 

meds_of_interest <- sparc_med_filter(
  data$prescriptions,
  data$observations,
  data$demographics,
  data$encounter,
  med_groups = c("Biologic","Immunomodulators")
)
```

# Finding the First Instance of a Medication

`sparc_med_starts` looks at a patients treatment journey utilizing EMR
and eCRF data. It is designed to find the first instance of a
medication. It is only appropriate for long term medications that we do
not expect to re-start. We assume biologics, aminosalicylates and
immunomodulators fit this description. It returns a data.frame with the
first medication start date for each drug, and an end date if available.

It calls `sparc_med_filter` to filter for the medications of interest.
It is recommended to only use those medications that a patient is on
long term and unlikely to restart.

Medication start and stop dates are chosen independently from both eCRF
and EMR sources. Medications with a start or stop date before 1980 are
dropped.

For EMR data, if a medication start date is missing, the visit encounter
start date is used. These records are flagged in the column
VISIT_ENCOUNTER_MED_START.

If a patient has medication information for the same drug from eCRF and
EMR, the eCRF data is preferred and used to generate MED_START_DATE and
MED_END_DATE. If only EMR data is available for that medication, then
EMR data is used.

Any overlap between medications is reported along with the number of
days the medications overlap.

If no end date is given for a prescription, the duration of the overlap
is calculated assuming an ongoing prescription. The effective end date
is set using a database wide cutoff based on the the date of the latest
encounter any patient had (as returned by the function `extract_latest`.

`sparc_med_starts` creates a table with the following columns:

<table>
<colgroup>
<col style="width: 16%" />
<col style="width: 83%" />
</colgroup>
<tbody>
<tr class="odd">
<td>Column</td>
<td>Description</td>
</tr>
<tr class="even">
<td>DEIDENTIFIED_MASTER_PATIENT_ID</td>
<td>Unique patient id</td>
</tr>
<tr class="odd">
<td>MEDICATION</td>
<td>Corresponds to new_med_name in the med_grp data.frame</td>
</tr>
<tr class="even">
<td>MED_START_DATE_ECRF</td>
<td>Medication Start Date from the eCRF data source</td>
</tr>
<tr class="odd">
<td>MED_END_DATE_ECRF</td>
<td>Medication End Date from the eCRF data source</td>
</tr>
<tr class="even">
<td>CURRENT_MEDICATION_ECRF</td>
<td>YES if the medication is current, NO if the medication has been
stopped</td>
</tr>
<tr class="odd">
<td>NUMBER_OF_DOSE_CHANGES_ECRF</td>
<td>Counts the number of times a patient reported a change in dose. This
field is free text and the r script only parses out numbers.</td>
</tr>
<tr class="even">
<td>NUMBER_OF_FREQ_CHANGES_ECRF</td>
<td>Counts the number of times a patient reported a change in medication
frequency.</td>
</tr>
<tr class="odd">
<td>REASON_STOPPED_ECRF</td>
<td>Lists the reason stopped if available. A medication may be current,
but have a reason stopped populated because of a dose and/or frequency
change.</td>
</tr>
<tr class="even">
<td>VISIT_ENCOUNTER_MED_START</td>
<td>For EMR data, flags if a medication start date is missing and the
visit encounter start date was used instead</td>
</tr>
<tr class="odd">
<td>MED_START_DATE_EMR</td>
<td>Medication Start Date from the EMR data source</td>
</tr>
<tr class="even">
<td>MED_END_DATE_EMR</td>
<td>Medication End Date from the EMR data source</td>
</tr>
<tr class="odd">
<td>LOADING_DOSE_EMR</td>
<td><p>1 if a patients prescription pattern for specific biologics
represents the loading pattern of that medication.</p>
<table>
<colgroup>
<col style="width: 20%" />
<col style="width: 79%" />
</colgroup>
<thead>
<tr class="header">
<th>Biologic</th>
<th>Criteria</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Infliximab</td>
<td>Prescriptions at 0,2,6 and then every 8 weeks</td>
</tr>
<tr class="even">
<td>Adalimumab</td>
<td>Every 2 weeks with the first dose as 160mg, then 80 mg, then 40mg
going forward</td>
</tr>
<tr class="odd">
<td>Certolizumab Pegol</td>
<td>Prescriptions at 0,2,4 and then every 4 weeks</td>
</tr>
<tr class="even">
<td>VedolizInumab</td>
<td>Prescriptions at 0,2,6 and then every 8 weeks</td>
</tr>
<tr class="odd">
<td>Ustekinumab</td>
<td>Initial dose as an IV and then subcutaneous injection every 8
weeks</td>
</tr>
</tbody>
</table></td>
</tr>
<tr class="even">
<td>PRESCRIPTION_NUMBER_EMR</td>
<td>Number of rows for this medication in the prescription emr data</td>
</tr>
<tr class="odd">
<td>MEDICATION_REFILLS_EMR</td>
<td>Number of refills from most recent prescription in the EMR data</td>
</tr>
<tr class="even">
<td>ECRF_PRESCRIPTION_DATA</td>
<td>1 if data for this medication is available in the eCRF data
source</td>
</tr>
<tr class="odd">
<td>MED_START_DATE</td>
<td>If ECRF_PRESCRIPTION_DATA = 1, then from MED_START_DATE_ECRF, else
from MED_START_DATE_EMR.</td>
</tr>
<tr class="even">
<td>MED_END_DATE</td>
<td>If ECRF_PRESCRIPTION_DATA = 1, then from MED_END_DATE_ECRF, else
from MED_END_DATE_EMR.</td>
</tr>
<tr class="odd">
<td>MOA</td>
<td>The mechanism of action for each medication. Includes, antiTNF,
Aminosalicylates, Immunomodulators, Interleukin-12 and -23 Antagonist,
JAKi, Integrin Receptor Antagonists, S1P</td>
</tr>
<tr class="even">
<td>MEDICATION_NUMBER</td>
<td>counts the number of different medications in a patients’ journey.
Only medications in the selected med_groups are considered. The
medication with with earliest start date will have MEDICATION_NUMBER =
1.</td>
</tr>
<tr class="odd">
<td>OVERLAPPING_MOA</td>
<td>If multiple medications overlap, the overlapping MOA is listed
here</td>
</tr>
<tr class="even">
<td>OVERLAPPING_DAYS</td>
<td>The number of days the medication overlaps with another MOA</td>
</tr>
<tr class="odd">
<td>DATE_OF_CONSENT</td>
<td>The date of enrollment into SPARC for the patient</td>
</tr>
<tr class="even">
<td>DATE_OF_CONSENT_WITHDRAWN</td>
<td>The date of withdrawal from SPARC, if applicable</td>
</tr>
<tr class="odd">
<td>STARTED_AFTER_ENROLLMENT</td>
<td>is 1 if the medication start date is after the date of consent.</td>
</tr>
<tr class="even">
<td>NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT</td>
<td>1 if a patient indicates “No” to “Are you currently on any IBD
medications?” at their baseline survey.</td>
</tr>
<tr class="odd">
<td>BIOLOGIC</td>
<td>is 1 if the record is for a biologic</td>
</tr>
<tr class="even">
<td>FIRST_BIOLOGIC</td>
<td>is 1 if a medication record is the first biologic a patient
receives.</td>
</tr>
<tr class="odd">
<td>FIRST_BIOLOGIC_NUMBER</td>
<td> If a record is the first biologic a patient receives,
FIRST_BIOLOGIC_NUMBER is equal to the MEDICATION_NUMBER.</td>
</tr>
<tr class="even">
<td>BIONAIVE</td>
<td>is 1 if patient has no prior reported biologic use</td>
</tr>
</tbody>
</table>

``` r


# Find Medication Start Dates for Biologics and Immunomodulators. 
# If export = TRUE then an excel spreadsheet is generated. 

medication_starts <- sparc_med_starts(
  data$prescriptions,
  data$observations,
  data$demographics,
  data$encounter,
  med_groups = c("Biologic","Immunomodulators"),
  export = TRUE
)
```

# Medication at a Specific Time-point

This medication journey can then be used to find the medications a
patient is on at a specific time point using the `sparc_medication`
function. This time point is referred to as an “index” date. The
function looks to see if the index date is within the medication start
and stop dates as generated by the `sparc_med_starts` function.

`sparc_medication` includes pre-programmed index dates for enrollment,
latest, endoscopy, omics and biosample collection. A data frame with the
DEIDENTIFIED_MASTER_PATIENT_ID and index_date can also be passed into
the function. If “Enrollment” is specified as the index, then only the
patient reported data (MED_START_DATE_ECRF & MED_END_DATE_ECRF) is used
to determine what medication a patient is on as this data is very robust
at this time point. If two medications of the same MOA overlap, the
start of the 2nd medication is used as the end date of the previous
medication.

`sparc_medication` also requires a list of data.frames as generated by
`load_data` which must include demographics, diagnosis, encounter,
procedures, observations, biosample, omics_patient_mapping, and
prescriptions.

Currently, this function does not support steroid or antibiotic use.
Functions to parse out this information are in development.

A data.frame and excel file is generated with the following columns:

|                                         |                                                                                                                                   |
|-----------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| **Column**                              | **Description**                                                                                                                   |
| DEIDENTIFIED_MASTER_PATIENT_ID          | Unique patient id                                                                                                                 |
| DATE_OF_CONSENT                         | The date of enrollment into SPARC for the patient                                                                                 |
| DATE_OF_CONSENT_WITHDRAWN               | The date of withdrawal from SPARC, if applicable                                                                                  |
| BIRTH_YEAR                              | Year of patients birth                                                                                                            |
| SEX                                     | Male, Female or Unknown                                                                                                           |
| DIAGNOSIS                               | Crohn’s Disease, Ulcerative Colitis, or IBD Unclassified                                                                          |
| DIAGNOSIS_DATE                          | Date of IBD Diagnosis                                                                                                             |
| INDEX_DATE                              | Date of interest as specified in “index_info”                                                                                     |
| NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT | 1 if a patient indicates “No” to “Are you currently on any IBD medications?” at their baseline survey.                            |
| MEDICATION_AT_INDEX                     | A concatenated list of medications the patient is on based on if the index date falls between the medication start and stop date. |
| BIONAIVE                                | is 1 if patient has no prior reported biologic use                                                                                |
| MED_START_DATE_ECRF_XXX                 | The start date in the ECRF for medication XXX                                                                                     |
| MED_END_DATE_ECRF_XXX                   | The end date in the ECRF for medication XXX                                                                                       |
| MED_START_DATE_EMR_XXX                  | The start date in the EMR for medication XXX                                                                                      |
| MED_END_DATE_EMR_XXX                    | The end date in the EMR for medication XXX                                                                                        |
| CURRENT_MEDICATION_ECRF_XXX             | The current medication flag (yes/no) from eCRF for medication XXX                                                                 |

``` r


# Load Data needed for sparc_medication function 

data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = c ("demographics", "diagnosis", "encounter", "procedures", "observations", "biosample", "omics_patient_mapping","prescriptions"), data_type = "Both")

# Find Biologics and Immunomodulators a patient is on at enrollment. 

med_at_enrollment <- sparc_medication(
  data = data,
  index_info = "Enrollment",
  med_groups = c("Biologic", "Immunomodulators"),
  filename = "SPARC_MEDICATION_AT_ENROLLMENT.xlsx"
)

# Find Biologics and Immunomodulators a patient is on at endoscopy

med_at_enrollment <- sparc_medication(
  data = data,
  index_info = "Endoscopy",
  med_groups = c("Biologic", "Immunomodulators"),
  filename = "SPARC_MEDICATION_AT_ENDOSCOPY.xlsx"
)
```
