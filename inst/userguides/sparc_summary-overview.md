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
This vignette focuses on the sparc\_summary function in the ibdplexus
package.

## sparc\_summary() components

The sparc\_summary() function generates a table with the information
collected on patient surveys, Smartform and from EMR around a specific
index date for each patient. It collapses the raw data from multiple
domains in the DDM into one research ready data set.

The index date can be a data frame with the
DEIDENTIFIED\_MASTER\_PATIENT\_ID and a field called “index\_date” which
is any date of interest. The function also has built in dates of
interest including time of enrollment, enodscopy, biosample collection,
omics date, and the most recent Smartform or patient survey date.

The function allows for a ‘index range’ in days, which specifies how far
out from the index date to gather the relevant data. The default is 30
days.

It requires a list of data frames to be loaded using the load\_data()
function. This list of data frames must include demographics, diagnosis,
encounter, procedures, observations, biosample, omics\_patient\_mapping,
prescriptions and labs.

## Resulting Table

The function produces a table with the following columns. The
sparc\_scores() function produces a subset of this table but with only
the disease activity scores and some other Smartform data, noted in the
following table.

-   ORDER - the column number in the table

-   COLUMN\_NAME - the column in the sparc\_summary() or sparc\_scores()
    table

-   COLUMN\_LOCATION - which table the column can be found in

-   COLUMN\_TYPE - describes if the information in this column is
    patient-level or within X days of the index date (“indexed column”)

-   DEFINITION - an explanation of the column name

-   DERVIVED\_VARIABLE - if this variable is derived from multiple
    fields in the raw data

-   DDM\_TABLE - what table the column is found in the ddm

-   DDM\_DATA\_SOURCE - the data source of the field

-   OTHER\_DDM\_FILTERS - any other filters that need to be performed to
    generate this column.

-   DDM\_VARIABLE - the variable that is reflected by this column

-   NOTES - any notes regarding the column

<table>
<colgroup>
<col style="width: 0%" />
<col style="width: 6%" />
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 10%" />
<col style="width: 1%" />
<col style="width: 1%" />
<col style="width: 1%" />
<col style="width: 31%" />
<col style="width: 4%" />
<col style="width: 37%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">ORDER</th>
<th style="text-align: left;">COLUMN_NAME</th>
<th style="text-align: left;">COLUMN_LOCATION</th>
<th style="text-align: left;">COLUMN_TYPE</th>
<th style="text-align: left;">DEFINITION</th>
<th style="text-align: left;">DERVIVED_VARIABLE</th>
<th style="text-align: left;">DDM_TABLE</th>
<th style="text-align: left;">DDM_DATA_SOURCE</th>
<th style="text-align: left;">OTHER_DDM_FILTERS</th>
<th style="text-align: left;">DDM_VARIABLE</th>
<th style="text-align: left;">NOTES</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">1</td>
<td style="text-align: left;">DEIDENTIFIED_MASTER_PATIENT_ID</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Unique id for each person</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DEIDENTIFIED_MASTER_PATIENT_ID</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">2</td>
<td style="text-align: left;">INDEX_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">A date of interest specified by the
user.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">3</td>
<td style="text-align: left;">DATE_OF_CONSENT</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Date of enrollment into SPARC.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATE_OF_CONSENT</td>
<td style="text-align: left;">If a patient withdraws and then re-enrolls
in SPARC, the first date of consent is used.</td>
</tr>
<tr class="even">
<td style="text-align: right;">4</td>
<td style="text-align: left;">DATE_OF_CONSENT_WITHDRAWN</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Date of Withdrawal From SPARC</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATE_OF_CONSENT_WITHDRAWN</td>
<td style="text-align: left;">If a patient withdraws and then re-enrolls
in SPARC, this will be blank and DATE_OF_CONSENT_WITHDRAWN_X will be
populated.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">5</td>
<td style="text-align: left;">DATE_OF_CONSENT_X</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Date of Consent for X enrollment</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATE_OF_CONSENT</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">6</td>
<td style="text-align: left;">DATE_OF_CONSENT_WITHDRAWN_X</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Date of Consent withdrawn for X
enrollment</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATE_OF_CONSENT_WITHDRAWN</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">7</td>
<td style="text-align: left;">BIRTH_YEAR</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Year the patient was born.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIRTH_YEAR</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">8</td>
<td style="text-align: left;">SEX</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Sex of patient from EMR and eCRF data
sources</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">EMR/ECRF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">GENDER</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">9</td>
<td style="text-align: left;">RACE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">from eCRF and EMR data sources</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">EMR/ECRF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">RACE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">10</td>
<td style="text-align: left;">ETHNICITY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">from eCRF and EMR data sources</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Demographics</td>
<td style="text-align: left;">EMR/ECRF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ETHNICITY</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">11</td>
<td style="text-align: left;">DIAGNOSIS</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Chose diagnosis reported closest to index
date from SF first, then ECRF. Can use ECRF_QORUS if consented to both
studies.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC &gt; ECRF_SPARC</td>
<td style="text-align: left;">DIAG_CONCEPT_NAME equals Crohn’s Disease,
Ulcerative Colitis or IBD Unclassified. For SF only,</td>
<td style="text-align: left;">DIAG_CONCEPT_NAME</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">12</td>
<td style="text-align: left;">DIAGNOSIS_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Year of IBD Diagnosis</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">DIAG_CONCEPT_NAME equals Crohn’s Disease,
Ulcerative Colitis, IBD Unclassified or Inflammatory Bowel Disease.</td>
<td style="text-align: left;">DIAGNOSIS_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">13</td>
<td style="text-align: left;">SAMPLE_NUMBER</td>
<td style="text-align: left;">Biosample Summary Table</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">The number of the sample taken by sample
type.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Only in Summary at Biosample Table</td>
</tr>
<tr class="even">
<td style="text-align: right;">14</td>
<td style="text-align: left;">DATA_NUMBER</td>
<td style="text-align: left;">Omics Summary Table</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">The number of the sample taken by sample
type.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Only in Summary at Omics Table.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">15</td>
<td style="text-align: left;">EMR_AVAILABLE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">EMR</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">16</td>
<td style="text-align: left;">SMARTFORM_AVAILABLE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">17</td>
<td style="text-align: left;">IBD_MEDICATION_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">18</td>
<td style="text-align: left;">IBD_DIAGNOSIS_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">19</td>
<td style="text-align: left;">IBD_SYMPTOMS_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">20</td>
<td style="text-align: left;">IBD_PROCEDURE_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">21</td>
<td style="text-align: left;">IBD_HOSPITALIZATION_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">22</td>
<td style="text-align: left;">IBD_HOSPITALIZATION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">23</td>
<td style="text-align: left;">SCHEDULED_IBD_HOSPITALIZATION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">24</td>
<td style="text-align: left;">OFFICE_VISIT</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">25</td>
<td
style="text-align: left;">IBD_HOSPITALIZATION_VIA_EMERGENCY_ROOM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">26</td>
<td style="text-align: left;">EMERGENCY_VISIT_NO_HOSPITALIZATION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">27</td>
<td style="text-align: left;">TELEMEDICINE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">28</td>
<td style="text-align: left;">HOSPITALIZATION_VIA_OFFICE_VISIT</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">29</td>
<td style="text-align: left;">IBD_PRE_VISIT_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">30</td>
<td style="text-align: left;">IBD_LABS_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">31</td>
<td style="text-align: left;">IBD_VACCINATION_SURVEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Flag if any of this data type is collected
throughout enrollment into SPARC</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Encounter</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">1 if data type is available</td>
</tr>
<tr class="even">
<td style="text-align: right;">32</td>
<td style="text-align: left;">BLOOD_PLASMA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this biosample
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">33</td>
<td
style="text-align: left;">BLOOD_RNA_IN_PAXGENE_TUBES_TO_BE_ISOLATED</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this biosample
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">34</td>
<td style="text-align: left;">TISSUE_DNA_EXTRACTION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this biosample
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">35</td>
<td style="text-align: left;">TISSUE_FORMALIN_JAR</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this biosample
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">36</td>
<td style="text-align: left;">TISSUE_LN2</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this biosample
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">37</td>
<td style="text-align: left;">TISSUE_RNALATER</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this biosample
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">38</td>
<td style="text-align: left;">TOTAL_BLOOD_PLASMA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Number is based on date sample is
collected, not the volume or aliquots of sample available.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">39</td>
<td
style="text-align: left;">TOTAL_BLOOD_RNA_IN_PAXGENE_TUBES_TO_BE_ISOLATED</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Number is based on date sample is
collected, not the volume or aliquots of sample available.</td>
</tr>
<tr class="even">
<td style="text-align: right;">40</td>
<td style="text-align: left;">TOTAL_TISSUE_LN2</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Number is based on date sample is
collected, not the volume or aliquots of sample available.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">41</td>
<td style="text-align: left;">TOTAL_TISSUE_DNA_EXTRACTION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Number is based on date sample is
collected, not the volume or aliquots of sample available.</td>
</tr>
<tr class="even">
<td style="text-align: right;">42</td>
<td style="text-align: left;">TOTAL_TISSUE_RNALATER</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Number is based on date sample is
collected, not the volume or aliquots of sample available.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">43</td>
<td style="text-align: left;">TOTAL_TISSUE_FORMALIN_JAR</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Biosample</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">BIOSAMPLE_CONCEPT_NAME</td>
<td style="text-align: left;">Date.Sample.Collected</td>
<td style="text-align: left;">Number is based on date sample is
collected, not the volume or aliquots of sample available.</td>
</tr>
<tr class="even">
<td style="text-align: right;">44</td>
<td style="text-align: left;">GENOTYPING_GLOBAL_SCREENING_ARRAY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">45</td>
<td style="text-align: left;">IMMUNOSEQ_TCRB_ASSAY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">46</td>
<td style="text-align: left;">ITS2_SEQUENCING</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">47</td>
<td style="text-align: left;">PROTEOMIC_BIOMARKER_PANELS_OLINK</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">48</td>
<td style="text-align: left;">RNASEQ</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">49</td>
<td style="text-align: left;">VIRAL_METAGENOMICS_SEQUENCING_VIROME</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">50</td>
<td style="text-align: left;">WHOLE_EXOME_SEQUENCING</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">51</td>
<td style="text-align: left;">WHOLE_SHOTGUN_SEQUENCING_WGS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">All dates available for this data
type</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">52</td>
<td
style="text-align: left;">TOTAL_PROTEOMIC_BIOMARKER_PANELS_OLINK</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Number is based on date sample is
collected.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">53</td>
<td style="text-align: left;">TOTAL_RNASEQ</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Number is based on date sample is
collected.</td>
</tr>
<tr class="even">
<td style="text-align: right;">54</td>
<td style="text-align: left;">TOTAL_IMMUNOSEQ_TCRB_ASSAY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Number is based on date sample is
collected.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">55</td>
<td style="text-align: left;">TOTAL_ITS2_SEQUENCING</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Number is based on date sample is
collected.</td>
</tr>
<tr class="even">
<td style="text-align: right;">56</td>
<td
style="text-align: left;">TOTAL_VIRAL_METAGENOMICS_SEQUENCING_VIROME</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Number is based on date sample is
collected.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">57</td>
<td style="text-align: left;">TOTAL_WHOLE_SHOTGUN_SEQUENCING_WGS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">Total Number of Sample Type Available</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Omics</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ASSAY NAME</td>
<td style="text-align: left;">SAMPLE_COLLECTED_DATE</td>
<td style="text-align: left;">Number is based on date sample is
collected.</td>
</tr>
<tr class="even">
<td style="text-align: right;">58</td>
<td style="text-align: left;">CROHN_S_DISEASE_PHENOTYPE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.
OBS_TEST_CONCEPT_NAME equals “Crohn’s Disease Phenotype”.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Inflammatory, non-penetrating,
non-stricturing phenotype can be pulled backward. All phenotypes can be
pulled forward to the next reported phenotype.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">59</td>
<td style="text-align: left;">PHENOTYPE__ANAL_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">60</td>
<td style="text-align: left;">PHENOTYPE__DUODENAL_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">61</td>
<td style="text-align: left;">PHENOTYPE__ESOPHAGEAL_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">62</td>
<td style="text-align: left;">PHENOTYPE__GASTRIC_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">63</td>
<td style="text-align: left;">PHENOTYPE__ILEAL_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">64</td>
<td style="text-align: left;">PHENOTYPE__JEJUNAL_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">65</td>
<td style="text-align: left;">PHENOTYPE__LEFT_COLONIC_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">66</td>
<td style="text-align: left;">PHENOTYPE__RECTAL_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">67</td>
<td style="text-align: left;">PHENOTYPE__RIGHT_COLONIC_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">68</td>
<td
style="text-align: left;">PHENOTYPE__TRANSVERSE_COLONIC_STRICTURE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">69</td>
<td style="text-align: left;">PHENOTYPE__ENTEROCUTANEOUS_FISTULA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">70</td>
<td style="text-align: left;">PHENOTYPE__ENTEROENTERIC_FISTULA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">71</td>
<td style="text-align: left;">PHENOTYPE__ENTEROVESICAL_FISTULA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">72</td>
<td style="text-align: left;">PHENOTYPE__INTRA_ABDOMINAL_ABCESS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">73</td>
<td style="text-align: left;">PHENOTYPE__OTHER_FISTULA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">74</td>
<td style="text-align: left;">CD_PHENO_JOURNEY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">patient level</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Phenotype journey can only get
progressively more severe.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">75</td>
<td
style="text-align: left;">IBD_MANIFESTATIONS__ABDOMINAL_ABSCESS,_FISTULA,_OR_OTHER_PENETRATING_COMPLICATION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">76</td>
<td style="text-align: left;">ANAL_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">77</td>
<td style="text-align: left;">DUODENAL_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">78</td>
<td style="text-align: left;">ESOPHAGEAL_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">79</td>
<td style="text-align: left;">GASTRIC_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">80</td>
<td style="text-align: left;">ILEAL_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">81</td>
<td style="text-align: left;">JEJUNAL_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">82</td>
<td style="text-align: left;">LEFT_COLONIC_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">83</td>
<td style="text-align: left;">RECTAL_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">84</td>
<td style="text-align: left;">RIGHT_COLONIC_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">85</td>
<td style="text-align: left;">TRANSVERSE_COLONIC_PHENOTYPE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Involved sites since disease onset for
Crohn’s Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">86</td>
<td style="text-align: left;">ANAL_CANAL_ULCER</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">87</td>
<td style="text-align: left;">LARGE_SKIN_TAGS</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">88</td>
<td style="text-align: left;">PERIANAL_FISTULA</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">89</td>
<td style="text-align: left;">ANAL_CANAL_STRICTURE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">90</td>
<td style="text-align: left;">ANAL_FISSURE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">91</td>
<td style="text-align: left;">PERIANAL_ABCESS</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">perianal modifier determined from
Smartform variables</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">92</td>
<td style="text-align: left;">RECTOVAGINAL_FISTULA</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">93</td>
<td style="text-align: left;">PERIANAL_FISTULA__COMPLEX_FISTULA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Crohn’s Disease patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">History of Perianal Disease for Crohn’s
Disease</td>
</tr>
<tr class="even">
<td style="text-align: right;">94</td>
<td style="text-align: left;">DISEASE_LOCATION</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Montreal Classification for Crohn’s
Disease patients determined from Smartform variables</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Ileal disease if ILEAL_PHENOTYPE = YES and
colonic disease if LEFT COLONIC PHENOTYPE<code>="Yes" or</code>RECTAL
PHENOTYPE<code>= "Yes" or</code>RIGHT COLONIC
PHENOTYPE<code>= "Yes" or</code>TRANSVERSE COLONIC
PHENOTYPE<code>="Yes". Ileocolonic if both colonic and ileal are yes.                                                                                                                                                                                                         | |    95|UPPERGI                                                                           |Summary Table; Scores at Index Table |indexed variable |Montreal Classification for Crohn's Disease patients determined from Smartform variables                                               |Y                 |[Calculated]  |NA                    |NA                                                                                                                                                                                                                                                                                                                                                                                                      |NA                                                      |Yes if</code>DUODENAL
PHENOTYPE<code>= "Yes" or</code>ESOPHAGEAL
PHENOTYPE<code>= "Yes" or</code>GASTRIC
PHENOTYPE<code>= "Yes" or</code>JEJUNAL
PHENOTYPE<code>= "Yes". Unknown if all fields are unknown and No if all fields are No.                                                                                                                                                                                                                                                                       | |    96|PERIANAL                                                                          |Summary Table; Scores at Index Table |indexed variable |Montreal Classification for Crohn's Disease patients determined from Smartform variables                                               |Y                 |[Calculated]  |NA                    |NA                                                                                                                                                                                                                                                                                                                                                                                                      |NA                                                      |Yes if</code>PHENOTYPE
- ANAL STRICTURE<code>= "Yes" or</code>ANAL
PHENOTYPE<code>= "Yes" or</code>ANAL CANAL
STRICTURE<code>= "Yes" or</code>ANAL CANAL
ULCER<code>= "Yes" or</code>ANAL FISSURE<code>= "Yes" or</code>PERIANAL
ABCESS<code>= "Yes" or</code>PERIANAL FISTULA - COMPLEX
FISTULA<code>= "Yes" or</code>PERIANAL
FISTULA<code>= "Yes" or</code>LARGE SKIN
TAGS<code>= "Yes" or</code>RECTOVAGINAL FISTULA` = “Yes” , No if all
fields are No</td>
</tr>
<tr class="odd">
<td style="text-align: right;">97</td>
<td
style="text-align: left;">EXTENT_OF_MACROSCOPIC_ULCERATIVE_COLITIS</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Ulcerative Colitis patients only.
OBS_TEST_CONCEPT_NAME equals “Extent of Macroscopic Ulcerative
Colitis”</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Ulcerative proctitis (rectum only) can be
pulled backward. All phenotypes can be pulled forward to the next
reported phenotype.</td>
</tr>
<tr class="even">
<td style="text-align: right;">98</td>
<td
style="text-align: left;">HISTORY_OF_HOSPITALIZATION_FOR_SEVERE_ULCERATIVE_COLITIS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For Ulcerative Colitis patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">99</td>
<td
style="text-align: left;">EXTENT_OF_MACROSCOPIC_IBD_UNCLASSIFIED</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For IBD unclassified patients only.
OBS_TEST_CONCEPT_NAME equals “Extent of Macroscopic IBD
Unclassified”</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Ulcerative proctitis (rectum only) can be
pulled backward. All phenotypes can be pulled forward to the next
reported phenotype.</td>
</tr>
<tr class="even">
<td style="text-align: right;">100</td>
<td
style="text-align: left;">HISTORY_OF_HOSPITALIZATION_FOR_SEVERE_IBD_UNCLASSIFIED</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">For IBD Unclassified patients only.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">101</td>
<td style="text-align: left;">NUMBER_OF_IBD_SURGERIES</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME = IBD Surgeries</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL</td>
<td style="text-align: left;">If 0 surgeries reported greater than the
index range after the index date, then pulled back.</td>
</tr>
<tr class="even">
<td style="text-align: right;">102</td>
<td style="text-align: left;">YEAR_OF_FIRST_IBD_SURGERY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME = IBD Surgeries</td>
<td style="text-align: left;">PROC_START_DATE</td>
<td style="text-align: left;">Year the same as or before the index
date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">103</td>
<td style="text-align: left;">YEAR_OF_MOST_RECENT_IBD_SURGERY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME = IBD Surgeries</td>
<td style="text-align: left;">PROC_END_DATE</td>
<td style="text-align: left;">Year the same as or before the index
date</td>
</tr>
<tr class="even">
<td style="text-align: right;">104</td>
<td style="text-align: left;">ESOPHAGEAL_SURGERY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">105</td>
<td style="text-align: left;">GASTRODUODENAL_SURGERY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">106</td>
<td style="text-align: left;">SMALL_BOWEL_RESECTION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">107</td>
<td style="text-align: left;">SMALL_BOWEL_RESECTION__DUODENUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">108</td>
<td style="text-align: left;">SMALL_BOWEL_RESECTION__ILEUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">109</td>
<td style="text-align: left;">SMALL_BOWEL_RESECTION__JEJUNUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">110</td>
<td style="text-align: left;">COLON_RESECTION</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">111</td>
<td style="text-align: left;">COLON_RESECTION__CECUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">112</td>
<td style="text-align: left;">COLON_RESECTION__RECTUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">113</td>
<td style="text-align: left;">COLON_RESECTION__SIGMOID</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">114</td>
<td style="text-align: left;">COMPLETE_COLECTOMY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME = Complete
Colectomy</td>
<td style="text-align: left;">PROC_STATUS_CONCEPT_CODE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">115</td>
<td style="text-align: left;">YEAR_OF_COMPLETE_COLECTOMY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME = Complete
Colectomy</td>
<td style="text-align: left;">PROC_START_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">116</td>
<td style="text-align: left;">INDICATION_FOR_TOTAL_COLECTOMY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME = Complete
Colectomy</td>
<td style="text-align: left;">INDICATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">117</td>
<td style="text-align: left;">ILEOSTOMY/COLOSTOMY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL and
PROC_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">118</td>
<td style="text-align: left;">STRICTUROPLASTY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL and
PROC_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">119</td>
<td style="text-align: left;">J_POUCH</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">PHYSICIAN_NOTES_PROC_AVAIL and
PROC_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">120</td>
<td style="text-align: left;">PHYSIOLOGICAL_SHORT_GUT_SYNDROME</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">121</td>
<td style="text-align: left;">TOBACCO_USE_WITHIN_LAST_7_DAYS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">122</td>
<td style="text-align: left;">TOBACCO_USE_WITHIN_LAST_3_MONTHS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">123</td>
<td style="text-align: left;">TOBACCO_USE__PREFERRED_TOBACCO_TYPE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">124</td>
<td style="text-align: left;">CURRENT_SMOKER</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From patient survey closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">125</td>
<td
style="text-align: left;">ORAL_NARCOTIC_INTAKE_WITHIN_LAST_30_DAYS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">126</td>
<td
style="text-align: left;">ORAL_NARCOTIC_INTAKE_WITHIN_LAST_7_DAYS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">127</td>
<td style="text-align: left;">APHTHOUS_ULCER</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="even">
<td style="text-align: right;">128</td>
<td style="text-align: left;">ERYTHEMA_NODOSUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="odd">
<td style="text-align: right;">129</td>
<td style="text-align: left;">IBD__ASSOCIATED_ARTHROPATHY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="even">
<td style="text-align: right;">130</td>
<td style="text-align: left;">IRITIS/UVEITIS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="odd">
<td style="text-align: right;">131</td>
<td style="text-align: left;">PRIMARY_SCLEROSING_CHOLANGITIS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="even">
<td style="text-align: right;">132</td>
<td style="text-align: left;">PYODERMA_GANGRENOSUM</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="odd">
<td style="text-align: right;">133</td>
<td style="text-align: left;">THROMBOTIC_COMPLICATIONS</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Extraintestinal Manifestation</td>
</tr>
<tr class="even">
<td style="text-align: right;">134</td>
<td style="text-align: left;">INTESTINAL_DYSPLASIA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Cancer History</td>
</tr>
<tr class="odd">
<td style="text-align: right;">135</td>
<td
style="text-align: left;">MALIGNANT_NEOPLASM_OF_COLON,_UNSPECIFIED</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Cancer History</td>
</tr>
<tr class="even">
<td style="text-align: right;">136</td>
<td
style="text-align: left;">PERSONAL_HISTORY_OF_CERVICAL_DYSPLASIA</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Cancer History</td>
</tr>
<tr class="odd">
<td style="text-align: right;">137</td>
<td style="text-align: left;">SKIN_CANCER</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Diagnosis</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DIAG_STATUS_CONCEPT_NAME</td>
<td style="text-align: left;">Cancer History</td>
</tr>
<tr class="even">
<td style="text-align: right;">138</td>
<td style="text-align: left;">ADALIMUMAB</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">139</td>
<td style="text-align: left;">CERTOLIZUMAB_PEGOL</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">140</td>
<td style="text-align: left;">CORTICOSTEROIDS</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">141</td>
<td style="text-align: left;">GOLIMUMAB</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">142</td>
<td style="text-align: left;">INFLIXIMAB_UNSPECIFIED</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">143</td>
<td style="text-align: left;">MESALAMINE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">144</td>
<td style="text-align: left;">METHOTREXATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">145</td>
<td style="text-align: left;">NATALIZUMAB</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">146</td>
<td style="text-align: left;">OTHER_BIOLOGIC</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">147</td>
<td style="text-align: left;">SULFASALAZINE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">148</td>
<td style="text-align: left;">THIOPURINE_UNSPECIFIED</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">149</td>
<td style="text-align: left;">TOFACITINIB</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">150</td>
<td style="text-align: left;">USTEKINUMAB</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="odd">
<td style="text-align: right;">151</td>
<td style="text-align: left;">VEDOLIZUMAB</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Prescriptions</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MEDICATION_ADMINISTRATED</td>
<td style="text-align: left;">Never can be pulled back &amp; Ever pulled
forward</td>
</tr>
<tr class="even">
<td style="text-align: right;">152</td>
<td
style="text-align: left;">CURRENT_STEROID_TREATMENT_10_MG/DAY_OR_HIGHER_FOR_PAST_60_DAYS_OR_MORE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">From Smartform closest to index date
within specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">153</td>
<td style="text-align: left;">FECAL_URGENCY</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Stool urgency in the past 7 days from the
patient survey closest to the index date within the specified index
range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">154</td>
<td style="text-align: left;">BMI</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Calculated from EMR data; BMI reported
closest to index date after outliers removed.</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">155</td>
<td style="text-align: left;">BMI_DATE</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of BMI</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">156</td>
<td
style="text-align: left;">LAB_RESULTS_FECAL_CALPROTECTIN_10_600_UG/G</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Results from fecal calprotectin test with
range of 10 - 600 ug/g. Value closest to the index date within the
specified index range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Labs</td>
<td style="text-align: left;">BIOSTORAGE_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">LAB_RESULTS or TEST_RESULT_NUMERIC</td>
<td style="text-align: left;">Lab information: <a
href="https://buhlmannlabs.com/buhlmann-fcal-elisa/"
class="uri">https://buhlmannlabs.com/buhlmann-fcal-elisa/</a></td>
</tr>
<tr class="odd">
<td style="text-align: right;">157</td>
<td
style="text-align: left;">TEST_UNIT_FECAL_CALPROTECTIN_10_600_UG/G</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Test unit from fecal calprotectin test
with range of 10 - 600 ug/g. Value closest to the index date within the
specified index range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Labs</td>
<td style="text-align: left;">BIOSTORAGE_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TEST_UNIT</td>
<td style="text-align: left;">Lab information: <a
href="https://buhlmannlabs.com/buhlmann-fcal-elisa/"
class="uri">https://buhlmannlabs.com/buhlmann-fcal-elisa/</a></td>
</tr>
<tr class="even">
<td style="text-align: right;">158</td>
<td
style="text-align: left;">LAB_RESULTS_FECAL_CALPROTECTIN_30_1800_UG/G</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Results from fecal calprotectin test with
range of 30 - 1800 ug/g. Value closest to the index date within the
specified index range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Labs</td>
<td style="text-align: left;">BIOSTORAGE_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">LAB_RESULTS or TEST_RESULT_NUMERIC</td>
<td style="text-align: left;">Lab information: <a
href="https://buhlmannlabs.com/buhlmann-fcal-elisa/"
class="uri">https://buhlmannlabs.com/buhlmann-fcal-elisa/</a></td>
</tr>
<tr class="odd">
<td style="text-align: right;">159</td>
<td
style="text-align: left;">TEST_UNIT_FECAL_CALPROTECTIN_30_1800_UG/G</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Test unit from fecal calprotectin test
with range of 30 - 1800 ug/g. Value closest to the index date within the
specified index range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Labs</td>
<td style="text-align: left;">BIOSTORAGE_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">TEST_UNIT</td>
<td style="text-align: left;">Lab information: <a
href="https://buhlmannlabs.com/buhlmann-fcal-elisa/"
class="uri">https://buhlmannlabs.com/buhlmann-fcal-elisa/</a></td>
</tr>
<tr class="even">
<td style="text-align: right;">160</td>
<td style="text-align: left;">ABDOMINAL_PAIN_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">FOR SF, OBS_TEST_CONCEPT_NAME equals
Abdominal Pain-Pain Scale; For ECRF, OBS_TEST_CONCEPT_NAME equals
Abdominal Pain</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Use to calculate A for sCDAI, PRO2 &amp;
PRO3 equation. None = 0; Mild = 1, Moderate = 2, Severe = 3</td>
</tr>
<tr class="odd">
<td style="text-align: right;">161</td>
<td style="text-align: left;">DAILY_BM</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">FOR SF, OBS_TEST_CONCEPT_NAME equals
Current Average Number of Daily Bowel Movements or Current Maximum
Number of Daily Bowel Movements. For ECRF, OBS_TEST_CONCEPT_NAME equals
Current Average Number of Daily Bowel Movements or Current Average
Number of Daily Liquid Bowel Movements. For ECRF, if
OBS_TEST_RESULT_DATE before August 1, 2018 use Current Average Number of
Daily Bowel Movements.</td>
<td style="text-align: left;">TEST_RESULT_NUMERIC;
DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">This is B for the sCDAI equation. This is
N/A for anyone with an ostomy. 20+ was treated as 20 in the sCDAI
equation.</td>
</tr>
<tr class="even">
<td style="text-align: right;">162</td>
<td style="text-align: left;">DAILY_BM_VERSION</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Refers to the question used to determine
the daily.bm variable in the sCDAI equation.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = Current Average Number of Daily Bowel
Movements 2 = Current Average Number of Daily Liquid Bowel Movements 3 =
Current Maximum Number of Daily Bowel Movements</td>
</tr>
<tr class="odd">
<td style="text-align: right;">163</td>
<td style="text-align: left;">GENERAL_WELL_BEING_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">FOR SF, OBS_TEST_CONCEPT_NAME equals
Constitutional - General Well-Being; For ECRF, OBS_TEST_CONCEPT_NAME
equals General Well-Being</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">Use to calculate G for sCDAI &amp; PRO3
equations. Generally well=0 Slightly under par=1 Poor=2 Very poor=3
Terrible=4</td>
</tr>
<tr class="even">
<td style="text-align: right;">164</td>
<td style="text-align: left;">LIQUID_BM</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">OBS_TEST_CONCEPT_NAME equals Current
Average Number of Daily Liquid Bowel Movements.</td>
<td style="text-align: left;">TEST_RESULT_NUMERIC;
DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">This is B for the PRO2 &amp; PRO3
equations. This is N/A for anyone with an ostomy. 20+ was treated as 20
in the PRO2 &amp; PRO3 equations.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">165</td>
<td style="text-align: left;">PRO2_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Crohn’s Disease Only. PRO2 = (B * 2) + (A
* 5)</td>
</tr>
<tr class="even">
<td style="text-align: right;">166</td>
<td style="text-align: left;">PRO2_CATEGORY</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The disease activity category for the
PRO2_SCORE</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Crohn’s Disease Only. - Remission: &lt; 8
- Mild: 8 - 13 - Moderate: 14 - 35 - Severe: &gt; 35</td>
</tr>
<tr class="odd">
<td style="text-align: right;">167</td>
<td style="text-align: left;">PRO2_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of PRO2 Score</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OBS_TEST_RESULT_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">168</td>
<td style="text-align: left;">PRO2_SOURCE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The data source for the PRO2_SCORE</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATA_SOURCE</td>
<td style="text-align: left;">Crohn’s Disease Only.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">169</td>
<td style="text-align: left;">PRO3_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Crohn’s Disease Only. PRO3 = (B * 2) + (A
* 5) + (G * 7)</td>
</tr>
<tr class="even">
<td style="text-align: right;">170</td>
<td style="text-align: left;">PRO3_CATEGORY</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The disease activity category for the
PRO3_SCORE</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Crohn’s Disease Only. - Remission: &lt; 13
- Mild: 13 - 21 - Moderate: 22 - 53 - Severe: &gt; 53</td>
</tr>
<tr class="odd">
<td style="text-align: right;">171</td>
<td style="text-align: left;">PRO3_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of PRO3 Score</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OBS_TEST_RESULT_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">172</td>
<td style="text-align: left;">PRO3_SOURCE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The data source for the PRO3_SCORE</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATA_SOURCE</td>
<td style="text-align: left;">Crohn’s Disease Only.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">173</td>
<td style="text-align: left;">SCDAI_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Crohn’s Disease Only.</td>
</tr>
<tr class="even">
<td style="text-align: right;">174</td>
<td style="text-align: left;">SCDAI_CATEGORY</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The disease activity category for the
SCDAI_SCORE</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Crohn’s Disease Only. - Remission: &lt;
150 - Mild: 150 - 219 - Moderate: 220 - 450 - Severe: &gt; 450</td>
</tr>
<tr class="odd">
<td style="text-align: right;">175</td>
<td style="text-align: left;">SCDAI_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of sCDAI Score</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OBS_TEST_RESULT_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">176</td>
<td style="text-align: left;">SCDAI_SOURCE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The data source for the SCDAI_SCORE</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATA_SOURCE</td>
<td style="text-align: left;">Crohn’s Disease Only.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">177</td>
<td style="text-align: left;">RECTAL_BLEEDING_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC;</td>
<td style="text-align: left;">FOR SF, OBS_TEST_CONCEPT_NAME Blood in
Stool - Recent Change in Rectal Bleeding Amount</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">If from both sources at baseline separate
by semicolon. This is R for the 6 pt Mayo &amp; 9 pt Mayo Scores. None;
No blood seen=0 Visible blood in stool less than half the time; Blood
less than 50% of the time=1 Visible blood in stool half the time or
more; Blood 50% or more of the time=2 Passing blood alone; ECRF: Blood
Passed Alone = Yes=3</td>
</tr>
<tr class="even">
<td style="text-align: right;">178</td>
<td style="text-align: left;">STOOL_FREQ_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC;</td>
<td style="text-align: left;">FOR SF, OBS_TEST_CONCEPT_NAME equals
Recent Change in Daily Stool Frequency. For ECRF, OBS_TEST_CONCEPT_NAME
equals Recent Change in Daily Stool Frequency</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">If from both sources at baseline separate
by semicolon. This is S for the 6 pt Mayo &amp; 9pt Mayo Scores Normal=0
1-2 stools/day more than normal=1 3-4 stools/day more than normal=2
&gt;4 stools/day more than normal; 5 or more stools per day more than
normal=3</td>
</tr>
<tr class="odd">
<td style="text-align: right;">179</td>
<td style="text-align: left;">GLOBAL_ASSESSMENT_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Value closest to index date within
specified index range</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">OBS_TEST_CONCEPT_NAME equals Physician’s
Global Assessment of Current Disease Status</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">This is T for the Mayo 9 Score.
Quiescent=0 Mild=1 Moderate=2 Severe=3</td>
</tr>
<tr class="even">
<td style="text-align: right;">180</td>
<td style="text-align: left;">MAYO_6_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Ulcerative Colitis Only. Only use SF or
ECRF to calculate; Do not combine from multiple sources to calculate. If
one variable is missing from equation, leave blank for that source.
MAYO_6_SCORE=S+R</td>
</tr>
<tr class="odd">
<td style="text-align: right;">181</td>
<td style="text-align: left;">MAYO_9_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Ulcerative Colitis Only. Only use SF or
ECRF to calculate; Do not combine from multiple sources to calculate. If
one variable is missing from equation, leave blank for that source.
MAYO_9_SCORE=S+R+T</td>
</tr>
<tr class="even">
<td style="text-align: right;">182</td>
<td style="text-align: left;">MAYO_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of score within index range of index
date</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OBS_TEST_RESULT_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">183</td>
<td style="text-align: left;">MAYO6_CATEGORY</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The disease activity category for the
MAYO_6_SCORE</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Ulcerative Colitis Only. - Remission: 0-1
- Mild: 2-3 - Moderate: 4-5 - Severe: 6</td>
</tr>
<tr class="even">
<td style="text-align: right;">184</td>
<td style="text-align: left;">MAYO_SOURCE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The data source for the MAYO6 &amp; MAYO9
score.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC; ECRF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATA_SOURCE</td>
<td style="text-align: left;">Ulcerative Colitis Only.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">185</td>
<td style="text-align: left;">PGA</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Physician global assessment closest to
index date within specified index range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">OBS_TEST_CONCEPT_CODE equals “EPIC#16411”
or “SMART_Q9__C”.</td>
<td style="text-align: left;">DESCRIPTIVE_SYMP_TEST_RESULTS</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">186</td>
<td style="text-align: left;">PGA_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of PGA Score</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OBS_TEST_RESULT_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">187</td>
<td style="text-align: left;">PGA_SOURCE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The data source for the PGA</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Observations</td>
<td style="text-align: left;">SF_SPARC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">DATA_SOURCE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">188</td>
<td style="text-align: left;">DISEASE_ACTIVITY_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The disease activity category for each
patient closest to the index date within the index range (XX)</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">The SCDAI_CATEGORY or CD patients, the
MAYO6_CATEGORY for UC patients, or the PGA for IBDU patients or when no
sCDAI or 6pt Mayo score available</td>
</tr>
<tr class="odd">
<td style="text-align: right;">189</td>
<td style="text-align: left;">SES_SUBSCORE_ILEUM</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for Crohn’s
Disease Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">190</td>
<td style="text-align: left;">SES_SUBSCORE_LEFT_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for Crohn’s
Disease Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">191</td>
<td style="text-align: left;">SES_SUBSCORE_RECTUM</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for Crohn’s
Disease Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">192</td>
<td style="text-align: left;">SES_SUBSCORE_RIGHT_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for Crohn’s
Disease Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">193</td>
<td style="text-align: left;">SES_SUBSCORE_TRANSVERSE_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for Crohn’s
Disease Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">194</td>
<td style="text-align: left;">SES_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The closest endoscopy score for CD
patients to the index date within the specified index range.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">SES_CD_SUBSCORE</td>
<td style="text-align: left;">Sum of SES_CD_SUBSCORE from each location
from same colonoscopy. If segment is “Not reached” then treated as 0.
For Crohn’s Disease patients only.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">195</td>
<td style="text-align: left;">RECTUM</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for
Ulcerative Colitis Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">196</td>
<td style="text-align: left;">SIGMOID_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for
Ulcerative Colitis Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">197</td>
<td style="text-align: left;">RIGHT_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for
Ulcerative Colitis Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">198</td>
<td style="text-align: left;">DESCENDING_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for
Ulcerative Colitis Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">199</td>
<td style="text-align: left;">TRANSVERSE_COLON</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Component of Endoscopy Score for
Ulcerative Colitis Patients</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">LOCATION</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: right;">200</td>
<td style="text-align: left;">MAX_EXTENT_ACTIVE_DISEASE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The closest endoscopy score for UC
patients to the index date within the specified index range.</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">MAX_EXTENT_ACTIVE_DISEASE</td>
<td style="text-align: left;">Units = cm</td>
</tr>
<tr class="odd">
<td style="text-align: right;">201</td>
<td style="text-align: left;">MAYO_ENDOSCOPY_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The closest endoscopy score for UC
patients to the index date within the specified index range.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">MAYO_ENDOSCOPIC_SUBSCORE</td>
<td style="text-align: left;">Maximum MAYO_ENDOSCOPIC_SUBSCORE from each
location from same colonoscopy. For Ulcerative Colitis patients
only.</td>
</tr>
<tr class="even">
<td style="text-align: right;">202</td>
<td style="text-align: left;">MODIFIED_MAYO_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The closest endoscopy score for UC
patients to the index date within the specified index range.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">MAYO_ENDOSCOPIC_SUBSCORE</td>
<td style="text-align: left;">Sum of MAYO_ENDOSCOPIC_SUBSCORE from each
location from same colonoscopy. For Ulcerative Colitis patients
only.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">203</td>
<td style="text-align: left;">EXTENDED_MODIFIED_MAYO_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The closest endoscopy score for UC
patients to the index date within the specified index range.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">EMS = MODIFIED_MAYO_SCORE /
(MAX_EXTENT_ACTIVE_DISEASE/10)</td>
</tr>
<tr class="even">
<td style="text-align: right;">204</td>
<td style="text-align: left;">MODIFIED_MAYO_ENDOSCOPIC_SCORE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">The closest endoscopy score for UC
patients to the index date within the specified index range.</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">[Calculated]</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">MMES = EMS / Number of Segments with
Active Disease</td>
</tr>
<tr class="odd">
<td style="text-align: right;">205</td>
<td style="text-align: left;">ENDO_CATEGORY</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Disease severity categorization from MES
or SES score from endoscopy closest to index date within specified index
range</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">For CD Patients using SES SCORE: -
Remission: 0-2 - Mild: 3-6 - Moderate: 7-15 - Severe: &gt; 15 For UC
Patients using Mayo Endoscopy Score (MES): - Remission: 0 - Mild: 1 -
Moderate: 2 - Severe: 3</td>
</tr>
<tr class="even">
<td style="text-align: right;">206</td>
<td style="text-align: left;">ENDO_DATE</td>
<td style="text-align: left;">Summary Table; Scores at Index Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Date of Endoscopy</td>
<td style="text-align: left;">N</td>
<td style="text-align: left;">Procedures</td>
<td style="text-align: left;">ECRF_SPARC</td>
<td style="text-align: left;">PROC_CONCEPT_NAME equals
Colonoscopy/Sigmoidoscopy</td>
<td style="text-align: left;">PROC_START_DATE</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: right;">207</td>
<td style="text-align: left;">ENDOSCOPY_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has endoscopy within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has an endoscopy with a MES or
SES Score within XX days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">208</td>
<td style="text-align: left;">BLOOD_PLASMA_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has biosample within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has a biosample available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">209</td>
<td
style="text-align: left;">BLOOD_RNA_IN_PAXGENE_TUBES_TO_BE_ISOLATED__XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has biosample within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has a biosample available
within xx days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">210</td>
<td style="text-align: left;">TISSUE_LN2_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has biosample within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has a biosample available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">211</td>
<td style="text-align: left;">TISSUE_DNA_EXTRACTION_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has biosample within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has a biosample available
within xx days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">212</td>
<td style="text-align: left;">TISSUE_RNALATER_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has biosample within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has a biosample available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">213</td>
<td style="text-align: left;">TISSUE_FORMALIN_JAR_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has biosample within xx
days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has a biosample available
within xx days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">214</td>
<td style="text-align: left;">GENOTYPING_GLOBAL_SCREENING_ARRAY__XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">215</td>
<td style="text-align: left;">WHOLE_EXOME_SEQUENCING_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">216</td>
<td style="text-align: left;">IMMUNOSEQ_TCRB_ASSAY_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">217</td>
<td style="text-align: left;">RNASEQ_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">218</td>
<td style="text-align: left;">WHOLE_SHOTGUN_SEQUENCING_WGS__XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">219</td>
<td style="text-align: left;">ITS2_SEQUENCING_XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="even">
<td style="text-align: right;">220</td>
<td
style="text-align: left;">VIRAL_METAGENOMICS_SEQUENCING_VIROME__XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
<tr class="odd">
<td style="text-align: right;">221</td>
<td style="text-align: left;">PROTEOMIC_BIOMARKER_PANELS_OLINK__XX</td>
<td style="text-align: left;">Summary Table</td>
<td style="text-align: left;">indexed variable</td>
<td style="text-align: left;">Flag if a patient has omics data generated
within xx days of index date</td>
<td style="text-align: left;">Y</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1 = patient has omics data available
within xx days of index date</td>
</tr>
</tbody>
</table>
