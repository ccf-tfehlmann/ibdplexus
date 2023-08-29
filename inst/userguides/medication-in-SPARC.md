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
medication and a standardized name. The MEDICATION\_NAME column includes
generic and brand names. the new\_med\_name column is the standardized
name for each MEDICATION\_NAME.

Biosimilars are now in their own groups.

    library(ibdplexus, quietly = T)
    library(tidyr, quietly = T)
    library(dplyr, quietly = T)
    library(lubridate, quietly = T)

    # Here is the table of the medication types in med_grp
    knitr::kable(med_grp %>% distinct(med_type))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">med_type</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">biologic</td>
</tr>
<tr class="even">
<td style="text-align: left;">antibiotics</td>
</tr>
<tr class="odd">
<td style="text-align: left;">immunomodulators</td>
</tr>
<tr class="even">
<td style="text-align: left;">aminosalicylates</td>
</tr>
<tr class="odd">
<td style="text-align: left;">corticosteroids</td>
</tr>
<tr class="even">
<td style="text-align: left;">antidiarrheals</td>
</tr>
<tr class="odd">
<td style="text-align: left;">other</td>
</tr>
<tr class="even">
<td style="text-align: left;">targeted synthetic small molecules</td>
</tr>
<tr class="odd">
<td style="text-align: left;">probiotic</td>
</tr>
</tbody>
</table>


    # Here is the first 5 rows of the med_grp data.frame

    knitr::kable(head(med_grp, 5))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">MEDICATION_NAME</th>
<th style="text-align: left;">med_type</th>
<th style="text-align: left;">new_med_name</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Cyltezo</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Adalimumab (Cyltezo)</td>
</tr>
<tr class="even">
<td style="text-align: left;">adalimumab-adbm</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Adalimumab (Cyltezo)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Adalimumab-bwwd</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Adalimumab (HADLIMA)</td>
</tr>
<tr class="even">
<td style="text-align: left;">HADLIMA</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Adalimumab (HADLIMA)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Adalimumab</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Adalimumab (Humira)</td>
</tr>
</tbody>
</table>


    # Here is how Infliximab and its' bio-similars are mapped

    knitr::kable(med_grp %>% filter(grepl("Infliximab", new_med_name, ignore.case = T)))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">MEDICATION_NAME</th>
<th style="text-align: left;">med_type</th>
<th style="text-align: left;">new_med_name</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Infliximab-axxq (Avsola)</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Avsola)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Avsola</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Avsola)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Infliximab-axxq</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Avsola)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Infliximab-dyyb (Inflectra)</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Inflectra)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Inflectra</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Inflectra)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Infliximab-dyyb</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Inflectra)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Remicade</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Remicade)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Infliximab</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Remicade)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Infliximab (Remicade)</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Remicade)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Infliximab (Remsima)</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Remsima)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Remsima</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Remsima)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Infliximab-abda (Renflexis)</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Renflexis)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Renflexis</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Renflexis)</td>
</tr>
<tr class="even">
<td style="text-align: left;">Infliximab-abda</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Renflexis)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Infliximab (Unspecified)</td>
<td style="text-align: left;">biologic</td>
<td style="text-align: left;">Infliximab (Unspecified)</td>
</tr>
</tbody>
</table>

To see the entire med\_grp data.frame click
<a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/79168cbffa523ab8da142d250133e70f625e00c7/data/med_grp.rda">`here`</a>

`sparc_med_filter` requires the following SPARC domains to be loaded:

-   prescriptions

-   observations

-   demographics

-   encounter

One must also supply the med\_groups of interest which corresponds to
the med\_type in the med\_grp data.frame.

`sparc med filter` subsets the prescriptions table to just the
medication types specified in med\_groups. The leading question on
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
OTHER\_MEDICATION and SRC\_DRUG\_CODE\_CONCEPT\_NAME columns in the
prescription table. If the medication of interest is found, it subsets
the data and maps the medication to the standardized name
(new\_med\_name).

    # Load EMR and eCRF data necessary for sparc_med_filter

    data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = c("prescriptions", "observations", "demographics", "encounter"), data_type = "Both")

    # Filter SPARC IBD data for biologics & immunomodulators

    meds_of_interest <- sparc_med_filter(
      data$prescriptions,
      data$observations,
      data$demographics,
      data$encounter,
      med_groups = c("Biologic", "Immunomodulators")
    )

# Finding the First Instance of a Medication

`sparc_med_journey` looks at a patients treatment journey utilizing EMR
and eCRF data. It is designed to find the first instance of a
medication. It is only appropriate for long term medications that we do
not expect to re-start. We assume biologics, aminosalicylates,
immunomodulators and targeted synthetic small molecules fit this
description. It returns a data.frame with the first medication start
date for each drug, and an end date if available.

It calls `sparc_med_filter` to filter for the medications of interest.
It is recommended to only use those medications that a patient is on
long term and unlikely to restart.

Medication start and stop dates are chosen independently from both eCRF
and EMR sources. Medications with a start or stop date before 1900 are
dropped.

For EMR data, if a medication start date is missing, the visit encounter
start date is used. These records are flagged in the column
VISIT\_ENCOUNTER\_MED\_START.

If a patient has medication information for the same drug from eCRF and
EMR, the earliest MED\_START\_DATE is used and the latest
MED\_END\_DATE. The columns MED\_START\_SOURCE and MED\_END\_SOURCE tell
the data source of the respective date.

Any overlap between medications is reported along with the number of
days the medications overlap.

If no end date is given for a prescription, the duration of the overlap
is calculated assuming an ongoing prescription. The effective end date
is set using a database wide cutoff based on the the date of the latest
encounter any patient had (as returned by the function `extract_latest`.

There are flags for if a patient was on a steroid at the same time as
the listed medication.

`sparc_med_journey` creates a table with the following columns:

<table>
<colgroup>
<col style="width: 14%" />
<col style="width: 85%" />
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
<td>DECREASE_IN_FREQUENCY</td>
<td>Flags if there is a decrease in frequency of a medication.</td>
</tr>
<tr class="odd">
<td>REASON_STOPPED</td>
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
<td>MED_DISCONT_START_DATE_EMR</td>
<td>Medication discontinue date from the EMR data source</td>
</tr>
<tr class="even">
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
<tr class="odd">
<td>PRESCRIPTION_NUMBER_EMR</td>
<td>Number of rows for this medication in the prescription emr data</td>
</tr>
<tr class="even">
<td>MEDICATION_REFILLS_EMR</td>
<td>Number of refills from most recent prescription in the EMR data</td>
</tr>
<tr class="odd">
<td>ECRF_PRESCRIPTION_DATA</td>
<td>1 if data for this medication is available in the eCRF data
source</td>
</tr>
<tr class="even">
<td>MED_START_DATE</td>
<td>The earliest date of MED_START_DATE_ECRF and MED_END_DATE_EMR</td>
</tr>
<tr class="odd">
<td>MED_END_DATE</td>
<td>The latest date of MED_END_DATE_ECRF and MED_END_DATE_EMR</td>
</tr>
<tr class="even">
<td>MED_START_SOURCE</td>
<td>The data source where the MED_START_DATE is coming from. If EMR
&amp; eCRF have the same date, then it is “Both”</td>
</tr>
<tr class="odd">
<td>MED_END_SOURCE</td>
<td>The data source where the MED_END_DATE is coming from. If EMR &amp;
eCRF have the same date, then it is “Both”</td>
</tr>
<tr class="even">
<td>MOA</td>
<td>The mechanism of action for each medication. Includes, antiTNF,
Aminosalicylates, Immunomodulators, Interleukin-12 and -23 Antagonist,
JAKi, Integrin Receptor Antagonists, S1P</td>
</tr>
<tr class="odd">
<td>MEDICATION_NUMBER</td>
<td>counts the number of different medications in a patients’ journey.
Only medications in the selected med_groups are considered. The
medication with with earliest start date will have MEDICATION_NUMBER =
1.</td>
</tr>
<tr class="even">
<td>OVERLAPPING_MOA</td>
<td>If multiple medications overlap, the overlapping MOA is listed
here</td>
</tr>
<tr class="odd">
<td>OVERLAPPING_DAYS</td>
<td>The number of days the medication overlaps with another MOA</td>
</tr>
<tr class="even">
<td>DATE_OF_CONSENT</td>
<td>The date of enrollment into SPARC for the patient</td>
</tr>
<tr class="odd">
<td>DATE_OF_CONSENT_WITHDRAWN</td>
<td>The date of withdrawal from SPARC, if applicable</td>
</tr>
<tr class="even">
<td>STARTED_AFTER_ENROLLMENT</td>
<td>is 1 if the medication start date is after the date of consent.</td>
</tr>
<tr class="odd">
<td>NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT</td>
<td>1 if a patient indicates “No” to “Are you currently on any IBD
medications?” at their baseline survey.</td>
</tr>
<tr class="even">
<td>ADVANCED_MED</td>
<td>is 1 if the record is for an advanced medication including
Adalimumab, Certolizumab Pegol, Golimumab, Infliximab, Natalizumab,
Other Biologic, Ustekinumab, Vedolizumab, Tofacitinib, Upadacitinib,
Ozanimod, or Risankizumab. Biosimilars are included.</td>
</tr>
<tr class="odd">
<td>FIRST_ADVANCED_MED</td>
<td>is 1 if a medication record is the first advanced medication a
patient receives.</td>
</tr>
<tr class="even">
<td>FIRST_ADVANCED_MED_NUMBER</td>
<td> If a record is the first advanced medication a patient receives,
FIRST_BIOLOGIC_NUMBER is equal to the MEDICATION_NUMBER.</td>
</tr>
<tr class="odd">
<td>BIONAIVE</td>
<td>is 1 if patient has no prior reported advanced medication use</td>
</tr>
<tr class="even">
<td>PREDNISONE</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="odd">
<td>BUDESONIDE</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="even">
<td>STEROIDS_ORAL_ECRF</td>
<td>Flag 1 if a patient has a report of Steroids Oral on the eCRF that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="odd">
<td>METHYLPREDNISOLONE</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="even">
<td>PREDNISOLONE</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="odd">
<td>STEROID_FOAM</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="even">
<td>STEROID_ENEMA</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="odd">
<td>STEROID_SUPPOSITORY</td>
<td>Flag 1 if a patient has a prescription or report of use that
overlaps with the MED_START_DATE and MED_END_DATE.</td>
</tr>
<tr class="even">
<td>ANY_STEROID</td>
<td>1 if any of the steroid flags are 1.</td>
</tr>
<tr class="odd">
<td>RECTAL_STEROID</td>
<td>1 if steroid foam, steroid enema or steroid suppository is 1</td>
</tr>
<tr class="even">
<td>ORAL_IV_STEROID</td>
<td>1 if prednisone, budesonide, steroids_oral_ecrf, methylprednisolone,
or prednisolone are 1</td>
</tr>
</tbody>
</table>

    # Find Medication Start Dates for Biologics and Immunomodulators.
    # If export = TRUE then an excel spreadsheet is generated.

    medication_starts <- sparc_med_journey(
      data$prescriptions,
      data$observations,
      data$demographics,
      data$encounter,
      med_groups = c("Biologic", "Immunomodulators"),
      export = TRUE
    )

# Medication at a Specific Time-point

This medication journey can then be used to find the medications a
patient is on at a specific time point using the `sparc_medication`
function. This time point is referred to as an “index” date. The
function looks to see if the index date is within the medication start
and stop dates as generated by the `sparc_med_journey` function.

`sparc_medication` includes pre-programmed index dates for enrollment,
latest, endoscopy, omics and biosample collection. A data frame with the
DEIDENTIFIED\_MASTER\_PATIENT\_ID and index\_date can also be passed
into the function. If two medications of the same MOA overlap, the start
of the 2nd medication is used as the end date of the previous
medication.

`sparc_medication` also requires a list of data.frames as generated by
`load_data` which must include demographics, diagnosis, encounter,
procedures, observations, biosample, omics\_patient\_mapping, and
prescriptions.

Currently, this function does not support antibiotic use. Functions to
parse out this information are in development.

A data.frame and excel file is generated with the following columns:

<table>
<colgroup>
<col style="width: 24%" />
<col style="width: 75%" />
</colgroup>
<tbody>
<tr class="odd">
<td><strong>Column</strong></td>
<td><strong>Description</strong></td>
</tr>
<tr class="even">
<td>DEIDENTIFIED_MASTER_PATIENT_ID</td>
<td>Unique patient id</td>
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
<td>BIRTH_YEAR</td>
<td>Year of patients birth</td>
</tr>
<tr class="even">
<td>SEX</td>
<td>Male, Female or Unknown</td>
</tr>
<tr class="odd">
<td>DIAGNOSIS</td>
<td>Crohn’s Disease, Ulcerative Colitis, or IBD Unclassified</td>
</tr>
<tr class="even">
<td>DIAGNOSIS_DATE</td>
<td>Date of IBD Diagnosis</td>
</tr>
<tr class="odd">
<td>INDEX_DATE</td>
<td>Date of interest as specified in “index_info”</td>
</tr>
<tr class="even">
<td>NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT</td>
<td>1 if a patient indicates “No” to “Are you currently on any IBD
medications?” at their baseline survey.</td>
</tr>
<tr class="odd">
<td>MEDICATION_AT_INDEX</td>
<td>A concatenated list of medications the patient is on based on if the
index date falls between the medication start and stop date.</td>
</tr>
<tr class="even">
<td>BIONAIVE</td>
<td>is 1 if patient has no prior reported biologic use</td>
</tr>
<tr class="odd">
<td>MED_START_DATE_ECRF_XXX</td>
<td>The start date in the ECRF for medication XXX</td>
</tr>
<tr class="even">
<td>MED_END_DATE_ECRF_XXX</td>
<td>The end date in the ECRF for medication XXX</td>
</tr>
<tr class="odd">
<td>MED_START_DATE_EMR_XXX</td>
<td>The start date in the EMR for medication XXX</td>
</tr>
<tr class="even">
<td>MED_END_DATE_EMR_XXX</td>
<td>The end date in the EMR for medication XXX</td>
</tr>
<tr class="odd">
<td>CURRENT_MEDICATION_ECRF_XXX</td>
<td>The current medication flag (yes/no) from eCRF for medication
XXX</td>
</tr>
<tr class="even">
<td>PREDNISONE</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="odd">
<td>BUDESONIDE</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="even">
<td>STEROIDS_ORAL_ECRF</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="odd">
<td>METHYLPREDNISOLONE</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="even">
<td>PREDNISOLONE</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="odd">
<td>STEROID_FOAM</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="even">
<td>STEROID_ENEMA</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="odd">
<td>STEROID_SUPPOSITORY</td>
<td>Flag if patient on listed steroid at index date.</td>
</tr>
<tr class="even">
<td>ANY_STEROID</td>
<td>Flag if patient on any of the steroids at index date.</td>
</tr>
<tr class="odd">
<td>RECTAL_STEROID</td>
<td>Flag is patient on steroid foam, steroid enema, or steroid
suppository at index date.</td>
</tr>
<tr class="even">
<td>ORAL_IV_STEROID</td>
<td>Flag if patient on oral or IV steroid at the index date.</td>
</tr>
</tbody>
</table>

    # Load Data needed for sparc_medication function

    data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = c("demographics", "diagnosis", "encounter", "procedures", "observations", "biosample", "omics_patient_mapping", "prescriptions"), data_type = "Both")

    # Find Biologics and Immunomodulators a patient is on at enrollment.

    med_at_enrollment <- sparc_medication(
      data = data,
      index_info = "Enrollment",
      med_groups = c("Biologic", "Immunomodulators"),
      filename = "SPARC_MEDICATION_AT_ENROLLMENT.xlsx"
    )

    # Find Biologics and Immunomodulators a patient is on at endoscopy

    med_at_endoscopy<- sparc_medication(
      data = data,
      index_info = "Endoscopy",
      med_groups = c("Biologic", "Immunomodulators"),
      filename = "SPARC_MEDICATION_AT_ENDOSCOPY.xlsx"
    )
