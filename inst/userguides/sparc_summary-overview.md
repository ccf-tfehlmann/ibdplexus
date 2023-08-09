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
<thead>
<tr>
<th style="text-align:right;">
ORDER
</th>
<th style="text-align:left;">
COLUMN\_NAME
</th>
<th style="text-align:left;">
COLUMN\_LOCATION
</th>
<th style="text-align:left;">
DEFINITION
</th>
<th style="text-align:left;">
DERVIVED\_VARIABLE
</th>
<th style="text-align:left;">
DDM\_TABLE
</th>
<th style="text-align:left;">
DDM\_DATA\_SOURCE
</th>
<th style="text-align:left;">
OTHER\_DDM\_FILTERS
</th>
<th style="text-align:left;">
DDM\_VARIABLE
</th>
<th style="text-align:left;">
NOTES
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
DEIDENTIFIED\_MASTER\_PATIENT\_ID
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Unique id for each person
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DEIDENTIFIED\_MASTER\_PATIENT\_ID
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
INDEX\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
A date of interest specified by the user.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of enrollment into SPARC.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT
</td>
<td style="text-align:left;">
If a patient withdraws and then re-enrolls in SPARC, the first date of
consent is used.
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT\_WITHDRAWN
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of Withdrawal From SPARC
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT\_WITHDRAWN
</td>
<td style="text-align:left;">
If a patient withdraws and then re-enrolls in SPARC, this will be blank
and DATE\_OF\_CONSENT\_WITHDRAWN\_X will be populated.
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT\_X
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of Consent for X enrollment
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT\_WITHDRAWN\_X
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of Consent withdrawn for X enrollment
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATE\_OF\_CONSENT\_WITHDRAWN
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
BIRTH\_YEAR
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Year the patient was born.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIRTH\_YEAR
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
SEX
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Sex of patient from EMR and eCRF data sources
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
EMR/ECRF
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
GENDER
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
RACE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
from eCRF and EMR data sources
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
EMR/ECRF
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
RACE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
ETHNICITY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
from eCRF and EMR data sources
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Demographics
</td>
<td style="text-align:left;">
EMR/ECRF
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ETHNICITY
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
DIAGNOSIS
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Chose diagnosis reported closest to index date from SF first, then ECRF.
Can use ECRF\_QORUS if consented to both studies.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC &gt; ECRF\_SPARC
</td>
<td style="text-align:left;">
DIAG\_CONCEPT\_NAME equals Crohn’s Disease, Ulcerative Colitis or IBD
Unclassified. For SF only,
</td>
<td style="text-align:left;">
DIAG\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
DIAGNOSIS\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Year of IBD Diagnosis
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
DIAG\_CONCEPT\_NAME equals Crohn’s Disease, Ulcerative Colitis, IBD
Unclassified or Inflammatory Bowel Disease.
</td>
<td style="text-align:left;">
DIAGNOSIS\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
SAMPLE\_NUMBER
</td>
<td style="text-align:left;">
Biosample Summary Table
</td>
<td style="text-align:left;">
The number of the sample taken by sample type.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Only in Summary at Biosample Table
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
DATA\_NUMBER
</td>
<td style="text-align:left;">
Omics Summary Table
</td>
<td style="text-align:left;">
The number of the sample taken by sample type.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Only in Summary at Omics Table.
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
EMR\_AVAILABLE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
EMR
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
SMARTFORM\_AVAILABLE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
IBD\_MEDICATION\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
IBD\_DIAGNOSIS\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
IBD\_SYMPTOMS\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
IBD\_PROCEDURE\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
21
</td>
<td style="text-align:left;">
IBD\_HOSPITALIZATION\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
IBD\_HOSPITALIZATION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
SCHEDULED\_IBD\_HOSPITALIZATION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
OFFICE\_VISIT
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
IBD\_HOSPITALIZATION\_VIA\_EMERGENCY\_ROOM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
EMERGENCY\_VISIT\_NO\_HOSPITALIZATION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
TELEMEDICINE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
HOSPITALIZATION\_VIA\_OFFICE\_VISIT
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
IBD\_PRE\_VISIT\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
IBD\_LABS\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
IBD\_VACCINATION\_SURVEY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if any of this data type is collected throughout enrollment into
SPARC
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Encounter
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TYPE\_OF\_ENCOUNTER
</td>
<td style="text-align:left;">
1 if data type is available
</td>
</tr>
<tr>
<td style="text-align:right;">
32
</td>
<td style="text-align:left;">
BLOOD\_PLASMA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this biosample type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
BLOOD\_RNA\_IN\_PAXGENE\_TUBES\_TO\_BE\_ISOLATED
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this biosample type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
TISSUE\_DNA\_EXTRACTION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this biosample type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
TISSUE\_FORMALIN\_JAR
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this biosample type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
TISSUE\_LN2
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this biosample type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
TISSUE\_RNALATER
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this biosample type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
TOTAL\_BLOOD\_PLASMA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Number is based on date sample is collected, not the volume or aliquots
of sample available.
</td>
</tr>
<tr>
<td style="text-align:right;">
39
</td>
<td style="text-align:left;">
TOTAL\_BLOOD\_RNA\_IN\_PAXGENE\_TUBES\_TO\_BE\_ISOLATED
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Number is based on date sample is collected, not the volume or aliquots
of sample available.
</td>
</tr>
<tr>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
TOTAL\_TISSUE\_LN2
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Number is based on date sample is collected, not the volume or aliquots
of sample available.
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
TOTAL\_TISSUE\_DNA\_EXTRACTION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Number is based on date sample is collected, not the volume or aliquots
of sample available.
</td>
</tr>
<tr>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
TOTAL\_TISSUE\_RNALATER
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Number is based on date sample is collected, not the volume or aliquots
of sample available.
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
TOTAL\_TISSUE\_FORMALIN\_JAR
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Biosample
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
BIOSAMPLE\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Date.Sample.Collected
</td>
<td style="text-align:left;">
Number is based on date sample is collected, not the volume or aliquots
of sample available.
</td>
</tr>
<tr>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
GENOTYPING\_GLOBAL\_SCREENING\_ARRAY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
IMMUNOSEQ\_TCRB\_ASSAY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
46
</td>
<td style="text-align:left;">
ITS2\_SEQUENCING
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
47
</td>
<td style="text-align:left;">
PROTEOMIC\_BIOMARKER\_PANELS\_OLINK
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
RNASEQ
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
VIRAL\_METAGENOMICS\_SEQUENCING\_VIROME
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
WHOLE\_EXOME\_SEQUENCING
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
51
</td>
<td style="text-align:left;">
WHOLE\_SHOTGUN\_SEQUENCING\_WGS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
All dates available for this data type
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
52
</td>
<td style="text-align:left;">
TOTAL\_PROTEOMIC\_BIOMARKER\_PANELS\_OLINK
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Number is based on date sample is collected.
</td>
</tr>
<tr>
<td style="text-align:right;">
53
</td>
<td style="text-align:left;">
TOTAL\_RNASEQ
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Number is based on date sample is collected.
</td>
</tr>
<tr>
<td style="text-align:right;">
54
</td>
<td style="text-align:left;">
TOTAL\_IMMUNOSEQ\_TCRB\_ASSAY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Number is based on date sample is collected.
</td>
</tr>
<tr>
<td style="text-align:right;">
55
</td>
<td style="text-align:left;">
TOTAL\_ITS2\_SEQUENCING
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Number is based on date sample is collected.
</td>
</tr>
<tr>
<td style="text-align:right;">
56
</td>
<td style="text-align:left;">
TOTAL\_VIRAL\_METAGENOMICS\_SEQUENCING\_VIROME
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Number is based on date sample is collected.
</td>
</tr>
<tr>
<td style="text-align:right;">
57
</td>
<td style="text-align:left;">
TOTAL\_WHOLE\_SHOTGUN\_SEQUENCING\_WGS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Total Number of Sample Type Available
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Omics
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
ASSAY NAME
</td>
<td style="text-align:left;">
SAMPLE\_COLLECTED\_DATE
</td>
<td style="text-align:left;">
Number is based on date sample is collected.
</td>
</tr>
<tr>
<td style="text-align:right;">
58
</td>
<td style="text-align:left;">
CROHN\_S\_DISEASE\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only. OBS\_TEST\_CONCEPT\_NAME equals
“Crohn’s Disease Phenotype”.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Inflammatory, non-penetrating, non-stricturing phenotype can be pulled
backward. All phenotypes can be pulled forward to the next reported
phenotype.
</td>
</tr>
<tr>
<td style="text-align:right;">
59
</td>
<td style="text-align:left;">
PHENOTYPE\_\_ANAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:left;">
PHENOTYPE\_\_DUODENAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
61
</td>
<td style="text-align:left;">
PHENOTYPE\_\_ESOPHAGEAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
62
</td>
<td style="text-align:left;">
PHENOTYPE\_\_GASTRIC\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
63
</td>
<td style="text-align:left;">
PHENOTYPE\_\_ILEAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
64
</td>
<td style="text-align:left;">
PHENOTYPE\_\_JEJUNAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
65
</td>
<td style="text-align:left;">
PHENOTYPE\_\_LEFT\_COLONIC\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
66
</td>
<td style="text-align:left;">
PHENOTYPE\_\_RECTAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
67
</td>
<td style="text-align:left;">
PHENOTYPE\_\_RIGHT\_COLONIC\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
68
</td>
<td style="text-align:left;">
PHENOTYPE\_\_TRANSVERSE\_COLONIC\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
69
</td>
<td style="text-align:left;">
PHENOTYPE\_\_ENTEROCUTANEOUS\_FISTULA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
70
</td>
<td style="text-align:left;">
PHENOTYPE\_\_ENTEROENTERIC\_FISTULA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
71
</td>
<td style="text-align:left;">
PHENOTYPE\_\_ENTEROVESICAL\_FISTULA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
PHENOTYPE\_\_INTRA\_ABDOMINAL\_ABCESS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
73
</td>
<td style="text-align:left;">
PHENOTYPE\_\_OTHER\_FISTULA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
74
</td>
<td style="text-align:left;">
IBD\_MANIFESTATIONS\_\_ABDOMINAL\_ABSCESS,\_FISTULA,\_OR\_OTHER\_PENETRATING\_COMPLICATION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
75
</td>
<td style="text-align:left;">
ANAL\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
76
</td>
<td style="text-align:left;">
DUODENAL\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
ESOPHAGEAL\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
GASTRIC\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
79
</td>
<td style="text-align:left;">
ILEAL\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
JEJUNAL\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
81
</td>
<td style="text-align:left;">
LEFT\_COLONIC\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
82
</td>
<td style="text-align:left;">
RECTAL\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
83
</td>
<td style="text-align:left;">
RIGHT\_COLONIC\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
TRANSVERSE\_COLONIC\_PHENOTYPE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Involved sites since disease onset for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
85
</td>
<td style="text-align:left;">
ANAL\_CANAL\_ULCER
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
LARGE\_SKIN\_TAGS
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
87
</td>
<td style="text-align:left;">
PERIANAL\_FISTULA
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
88
</td>
<td style="text-align:left;">
ANAL\_CANAL\_STRICTURE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
89
</td>
<td style="text-align:left;">
ANAL\_FISSURE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
PERIANAL\_ABCESS
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
perianal modifier determined from Smartform variables
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
91
</td>
<td style="text-align:left;">
RECTOVAGINAL\_FISTULA
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
92
</td>
<td style="text-align:left;">
PERIANAL\_FISTULA\_\_COMPLEX\_FISTULA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Crohn’s Disease patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
History of Perianal Disease for Crohn’s Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
93
</td>
<td style="text-align:left;">
DISEASE\_LOCATION
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Montreal Classification for Crohn’s Disease patients determined from
Smartform variables
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Ileal disease if ILEAL\_PHENOTYPE = YES and colonic disease if LEFT
COLONIC PHENOTYPE`=&quot;Yes&quot; or`RECTAL
PHENOTYPE`= &quot;Yes&quot; or`RIGHT COLONIC
PHENOTYPE`= &quot;Yes&quot; or`TRANSVERSE COLONIC
PHENOTYPE`=&quot;Yes&quot;. Ileocolonic if both colonic and ileal are yes. </td>   </tr>   <tr>    <td style="text-align:right;"> 94 </td>    <td style="text-align:left;"> UPPERGI </td>    <td style="text-align:left;"> Summary Table; Scores at Index Table </td>    <td style="text-align:left;"> Montreal Classification for Crohn's Disease patients determined from Smartform variables </td>    <td style="text-align:left;"> Y </td>    <td style="text-align:left;"> [Calculated] </td>    <td style="text-align:left;"> NA </td>    <td style="text-align:left;"> NA </td>    <td style="text-align:left;"> NA </td>    <td style="text-align:left;"> Yes if`DUODENAL
PHENOTYPE`= &quot;Yes&quot; or`ESOPHAGEAL
PHENOTYPE`= &quot;Yes&quot; or`GASTRIC
PHENOTYPE`= &quot;Yes&quot; or`JEJUNAL
PHENOTYPE`= &quot;Yes&quot;. Unknown if all fields are unknown and No if all fields are No. </td>   </tr>   <tr>    <td style="text-align:right;"> 95 </td>    <td style="text-align:left;"> PERIANAL </td>    <td style="text-align:left;"> Summary Table; Scores at Index Table </td>    <td style="text-align:left;"> Montreal Classification for Crohn's Disease patients determined from Smartform variables </td>    <td style="text-align:left;"> Y </td>    <td style="text-align:left;"> [Calculated] </td>    <td style="text-align:left;"> NA </td>    <td style="text-align:left;"> NA </td>    <td style="text-align:left;"> NA </td>    <td style="text-align:left;"> Yes if`PHENOTYPE -
ANAL STRICTURE`= &quot;Yes&quot; or`ANAL
PHENOTYPE`= &quot;Yes&quot; or`ANAL CANAL
STRICTURE`= &quot;Yes&quot; or`ANAL CANAL
ULCER`= &quot;Yes&quot; or`ANAL FISSURE`= &quot;Yes&quot; or`PERIANAL
ABCESS`= &quot;Yes&quot; or`PERIANAL FISTULA - COMPLEX
FISTULA`= &quot;Yes&quot; or`PERIANAL FISTULA`= &quot;Yes&quot; or`LARGE
SKIN TAGS`= &quot;Yes&quot; or`RECTOVAGINAL FISTULA\` = "Yes" , No if
all fields are No
</td>
</tr>
<tr>
<td style="text-align:right;">
96
</td>
<td style="text-align:left;">
EXTENT\_OF\_MACROSCOPIC\_ULCERATIVE\_COLITIS
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Ulcerative Colitis patients only. OBS\_TEST\_CONCEPT\_NAME equals
“Extent of Macroscopic Ulcerative Colitis”
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Ulcerative proctitis (rectum only) can be pulled backward. All
phenotypes can be pulled forward to the next reported phenotype.
</td>
</tr>
<tr>
<td style="text-align:right;">
97
</td>
<td style="text-align:left;">
HISTORY\_OF\_HOSPITALIZATION\_FOR\_SEVERE\_ULCERATIVE\_COLITIS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For Ulcerative Colitis patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
98
</td>
<td style="text-align:left;">
EXTENT\_OF\_MACROSCOPIC\_IBD\_UNCLASSIFIED
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For IBD unclassified patients only. OBS\_TEST\_CONCEPT\_NAME equals
“Extent of Macroscopic IBD Unclassified”
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Ulcerative proctitis (rectum only) can be pulled backward. All
phenotypes can be pulled forward to the next reported phenotype.
</td>
</tr>
<tr>
<td style="text-align:right;">
99
</td>
<td style="text-align:left;">
HISTORY\_OF\_HOSPITALIZATION\_FOR\_SEVERE\_IBD\_UNCLASSIFIED
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
For IBD Unclassified patients only.
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
100
</td>
<td style="text-align:left;">
NUMBER\_OF\_IBD\_SURGERIES
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME = IBD Surgeries
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL
</td>
<td style="text-align:left;">
If 0 surgeries reported greater than the index range after the index
date, then pulled back.
</td>
</tr>
<tr>
<td style="text-align:right;">
101
</td>
<td style="text-align:left;">
YEAR\_OF\_FIRST\_IBD\_SURGERY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME = IBD Surgeries
</td>
<td style="text-align:left;">
PROC\_START\_DATE
</td>
<td style="text-align:left;">
Year the same as or before the index date
</td>
</tr>
<tr>
<td style="text-align:right;">
102
</td>
<td style="text-align:left;">
YEAR\_OF\_MOST\_RECENT\_IBD\_SURGERY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME = IBD Surgeries
</td>
<td style="text-align:left;">
PROC\_END\_DATE
</td>
<td style="text-align:left;">
Year the same as or before the index date
</td>
</tr>
<tr>
<td style="text-align:right;">
103
</td>
<td style="text-align:left;">
ESOPHAGEAL\_SURGERY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
GASTRODUODENAL\_SURGERY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
SMALL\_BOWEL\_RESECTION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
SMALL\_BOWEL\_RESECTION\_\_DUODENUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
107
</td>
<td style="text-align:left;">
SMALL\_BOWEL\_RESECTION\_\_ILEUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
SMALL\_BOWEL\_RESECTION\_\_JEJUNUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
109
</td>
<td style="text-align:left;">
COLON\_RESECTION
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
COLON\_RESECTION\_\_CECUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
111
</td>
<td style="text-align:left;">
COLON\_RESECTION\_\_RECTUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
112
</td>
<td style="text-align:left;">
COLON\_RESECTION\_\_SIGMOID
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
113
</td>
<td style="text-align:left;">
COMPLETE\_COLECTOMY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME = Complete Colectomy
</td>
<td style="text-align:left;">
PROC\_STATUS\_CONCEPT\_CODE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
114
</td>
<td style="text-align:left;">
YEAR\_OF\_COMPLETE\_COLECTOMY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME = Complete Colectomy
</td>
<td style="text-align:left;">
PROC\_START\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
115
</td>
<td style="text-align:left;">
INDICATION\_FOR\_TOTAL\_COLECTOMY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME = Complete Colectomy
</td>
<td style="text-align:left;">
INDICATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
116
</td>
<td style="text-align:left;">
ILEOSTOMY/COLOSTOMY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL and PROC\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
STRICTUROPLASTY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL and PROC\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
118
</td>
<td style="text-align:left;">
J\_POUCH
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
PHYSICIAN\_NOTES\_PROC\_AVAIL and PROC\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
119
</td>
<td style="text-align:left;">
PHYSIOLOGICAL\_SHORT\_GUT\_SYNDROME
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:left;">
TOBACCO\_USE\_WITHIN\_LAST\_7\_DAYS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
TOBACCO\_USE\_WITHIN\_LAST\_3\_MONTHS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
TOBACCO\_USE\_\_PREFERRED\_TOBACCO\_TYPE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
123
</td>
<td style="text-align:left;">
CURRENT\_SMOKER
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From patient survey closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
124
</td>
<td style="text-align:left;">
ORAL\_NARCOTIC\_INTAKE\_WITHIN\_LAST\_30\_DAYS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
ORAL\_NARCOTIC\_INTAKE\_WITHIN\_LAST\_7\_DAYS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
APHTHOUS\_ULCER
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
ERYTHEMA\_NODOSUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
128
</td>
<td style="text-align:left;">
IBD\_\_ASSOCIATED\_ARTHROPATHY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
129
</td>
<td style="text-align:left;">
IRITIS/UVEITIS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
130
</td>
<td style="text-align:left;">
PRIMARY\_SCLEROSING\_CHOLANGITIS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
131
</td>
<td style="text-align:left;">
PYODERMA\_GANGRENOSUM
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
132
</td>
<td style="text-align:left;">
THROMBOTIC\_COMPLICATIONS
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Extraintestinal Manifestation
</td>
</tr>
<tr>
<td style="text-align:right;">
133
</td>
<td style="text-align:left;">
INTESTINAL\_DYSPLASIA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Cancer History
</td>
</tr>
<tr>
<td style="text-align:right;">
134
</td>
<td style="text-align:left;">
MALIGNANT\_NEOPLASM\_OF\_COLON,\_UNSPECIFIED
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Cancer History
</td>
</tr>
<tr>
<td style="text-align:right;">
135
</td>
<td style="text-align:left;">
PERSONAL\_HISTORY\_OF\_CERVICAL\_DYSPLASIA
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Cancer History
</td>
</tr>
<tr>
<td style="text-align:right;">
136
</td>
<td style="text-align:left;">
SKIN\_CANCER
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Diagnosis
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DIAG\_STATUS\_CONCEPT\_NAME
</td>
<td style="text-align:left;">
Cancer History
</td>
</tr>
<tr>
<td style="text-align:right;">
137
</td>
<td style="text-align:left;">
ADALIMUMAB
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
138
</td>
<td style="text-align:left;">
CERTOLIZUMAB\_PEGOL
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
139
</td>
<td style="text-align:left;">
CORTICOSTEROIDS
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
140
</td>
<td style="text-align:left;">
GOLIMUMAB
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
141
</td>
<td style="text-align:left;">
INFLIXIMAB\_UNSPECIFIED
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
142
</td>
<td style="text-align:left;">
MESALAMINE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
143
</td>
<td style="text-align:left;">
METHOTREXATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
144
</td>
<td style="text-align:left;">
NATALIZUMAB
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
145
</td>
<td style="text-align:left;">
OTHER\_BIOLOGIC
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
146
</td>
<td style="text-align:left;">
SULFASALAZINE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
147
</td>
<td style="text-align:left;">
THIOPURINE\_UNSPECIFIED
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
148
</td>
<td style="text-align:left;">
TOFACITINIB
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
149
</td>
<td style="text-align:left;">
USTEKINUMAB
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
VEDOLIZUMAB
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Prescriptions
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MEDICATION\_ADMINISTRATED
</td>
<td style="text-align:left;">
Never can be pulled back & Ever pulled forward
</td>
</tr>
<tr>
<td style="text-align:right;">
151
</td>
<td style="text-align:left;">
CURRENT\_STEROID\_TREATMENT\_10\_MG/DAY\_OR\_HIGHER\_FOR\_PAST\_60\_DAYS\_OR\_MORE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
From Smartform closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
152
</td>
<td style="text-align:left;">
FECAL\_URGENCY
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Stool urgency in the past 7 days from the patient survey closest to the
index date within the specified index range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
153
</td>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Calculated from EMR data; BMI reported closest to index date after
outliers removed.
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
154
</td>
<td style="text-align:left;">
BMI\_DATE
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Date of BMI
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
155
</td>
<td style="text-align:left;">
LAB\_RESULTS\_FECAL\_CALPROTECTIN\_10\_600\_UG/G
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Results from fecal calprotectin test with range of 10 - 600 ug/g. Value
closest to the index date within the specified index range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Labs
</td>
<td style="text-align:left;">
BIOSTORAGE\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
LAB\_RESULTS or TEST\_RESULT\_NUMERIC
</td>
<td style="text-align:left;">
Lab information: <https://buhlmannlabs.com/buhlmann-fcal-elisa/>
</td>
</tr>
<tr>
<td style="text-align:right;">
156
</td>
<td style="text-align:left;">
TEST\_UNIT\_FECAL\_CALPROTECTIN\_10\_600\_UG/G
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Test unit from fecal calprotectin test with range of 10 - 600 ug/g.
Value closest to the index date within the specified index range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Labs
</td>
<td style="text-align:left;">
BIOSTORAGE\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TEST\_UNIT
</td>
<td style="text-align:left;">
Lab information: <https://buhlmannlabs.com/buhlmann-fcal-elisa/>
</td>
</tr>
<tr>
<td style="text-align:right;">
157
</td>
<td style="text-align:left;">
LAB\_RESULTS\_FECAL\_CALPROTECTIN\_30\_1800\_UG/G
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Results from fecal calprotectin test with range of 30 - 1800 ug/g. Value
closest to the index date within the specified index range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Labs
</td>
<td style="text-align:left;">
BIOSTORAGE\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
LAB\_RESULTS or TEST\_RESULT\_NUMERIC
</td>
<td style="text-align:left;">
Lab information: <https://buhlmannlabs.com/buhlmann-fcal-elisa/>
</td>
</tr>
<tr>
<td style="text-align:right;">
158
</td>
<td style="text-align:left;">
TEST\_UNIT\_FECAL\_CALPROTECTIN\_30\_1800\_UG/G
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Test unit from fecal calprotectin test with range of 30 - 1800 ug/g.
Value closest to the index date within the specified index range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Labs
</td>
<td style="text-align:left;">
BIOSTORAGE\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TEST\_UNIT
</td>
<td style="text-align:left;">
Lab information: <https://buhlmannlabs.com/buhlmann-fcal-elisa/>
</td>
</tr>
<tr>
<td style="text-align:right;">
159
</td>
<td style="text-align:left;">
ABDOMINAL\_PAIN\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
FOR SF, OBS\_TEST\_CONCEPT\_NAME equals Abdominal Pain-Pain Scale; For
ECRF, OBS\_TEST\_CONCEPT\_NAME equals Abdominal Pain
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Use to calculate A for sCDAI, PRO2 & PRO3 equation. None = 0; Mild = 1,
Moderate = 2, Severe = 3
</td>
</tr>
<tr>
<td style="text-align:right;">
160
</td>
<td style="text-align:left;">
DAILY\_BM
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
FOR SF, OBS\_TEST\_CONCEPT\_NAME equals Current Average Number of Daily
Bowel Movements or Current Maximum Number of Daily Bowel Movements. For
ECRF, OBS\_TEST\_CONCEPT\_NAME equals Current Average Number of Daily
Bowel Movements or Current Average Number of Daily Liquid Bowel
Movements. For ECRF, if OBS\_TEST\_RESULT\_DATE before August 1, 2018
use Current Average Number of Daily Bowel Movements.
</td>
<td style="text-align:left;">
TEST\_RESULT\_NUMERIC; DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
This is B for the sCDAI equation. This is N/A for anyone with an ostomy.
20+ was treated as 20 in the sCDAI equation.
</td>
</tr>
<tr>
<td style="text-align:right;">
161
</td>
<td style="text-align:left;">
DAILY\_BM\_VERSION
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Refers to the question used to determine the daily.bm variable in the
sCDAI equation.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = Current Average Number of Daily Bowel Movements 2 = Current Average
Number of Daily Liquid Bowel Movements 3 = Current Maximum Number of
Daily Bowel Movements
</td>
</tr>
<tr>
<td style="text-align:right;">
162
</td>
<td style="text-align:left;">
GENERAL\_WELL\_BEING\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
FOR SF, OBS\_TEST\_CONCEPT\_NAME equals Constitutional - General
Well-Being; For ECRF, OBS\_TEST\_CONCEPT\_NAME equals General Well-Being
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
Use to calculate G for sCDAI & PRO3 equations. Generally well=0 Slightly
under par=1 Poor=2 Very poor=3 Terrible=4
</td>
</tr>
<tr>
<td style="text-align:right;">
163
</td>
<td style="text-align:left;">
LIQUID\_BM
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
OBS\_TEST\_CONCEPT\_NAME equals Current Average Number of Daily Liquid
Bowel Movements.
</td>
<td style="text-align:left;">
TEST\_RESULT\_NUMERIC; DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
This is B for the PRO2 & PRO3 equations. This is N/A for anyone with an
ostomy. 20+ was treated as 20 in the PRO2 & PRO3 equations.
</td>
</tr>
<tr>
<td style="text-align:right;">
164
</td>
<td style="text-align:left;">
PRO2\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Crohn’s Disease Only. PRO2 = (B \* 2) + (A \* 5)
</td>
</tr>
<tr>
<td style="text-align:right;">
165
</td>
<td style="text-align:left;">
PRO2\_CATEGORY
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The disease activity category for the PRO2\_SCORE
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Crohn’s Disease Only. - Remission: &lt; 8 - Mild: 8 - 13 - Moderate:
14 - 35 - Severe: &gt; 35
</td>
</tr>
<tr>
<td style="text-align:right;">
166
</td>
<td style="text-align:left;">
PRO2\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of PRO2 Score
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
OBS\_TEST\_RESULT\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
167
</td>
<td style="text-align:left;">
PRO2\_SOURCE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The data source for the PRO2\_SCORE
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATA\_SOURCE
</td>
<td style="text-align:left;">
Crohn’s Disease Only.
</td>
</tr>
<tr>
<td style="text-align:right;">
168
</td>
<td style="text-align:left;">
PRO3\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Crohn’s Disease Only. PRO3 = (B \* 2) + (A \* 5) + (G \* 7)
</td>
</tr>
<tr>
<td style="text-align:right;">
169
</td>
<td style="text-align:left;">
PRO3\_CATEGORY
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The disease activity category for the PRO3\_SCORE
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Crohn’s Disease Only. - Remission: &lt; 13 - Mild: 13 - 21 - Moderate:
22 - 53 - Severe: &gt; 53
</td>
</tr>
<tr>
<td style="text-align:right;">
170
</td>
<td style="text-align:left;">
PRO3\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of PRO3 Score
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
OBS\_TEST\_RESULT\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
171
</td>
<td style="text-align:left;">
PRO3\_SOURCE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The data source for the PRO3\_SCORE
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATA\_SOURCE
</td>
<td style="text-align:left;">
Crohn’s Disease Only.
</td>
</tr>
<tr>
<td style="text-align:right;">
172
</td>
<td style="text-align:left;">
SCDAI\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Crohn’s Disease Only.
</td>
</tr>
<tr>
<td style="text-align:right;">
173
</td>
<td style="text-align:left;">
SCDAI\_CATEGORY
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The disease activity category for the SCDAI\_SCORE
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Crohn’s Disease Only. - Remission: &lt; 150 - Mild: 150 - 219 -
Moderate: 220 - 450 - Severe: &gt; 450
</td>
</tr>
<tr>
<td style="text-align:right;">
174
</td>
<td style="text-align:left;">
SCDAI\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of sCDAI Score
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
OBS\_TEST\_RESULT\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
175
</td>
<td style="text-align:left;">
SCDAI\_SOURCE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The data source for the SCDAI\_SCORE
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATA\_SOURCE
</td>
<td style="text-align:left;">
Crohn’s Disease Only.
</td>
</tr>
<tr>
<td style="text-align:right;">
176
</td>
<td style="text-align:left;">
RECTAL\_BLEEDING\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC;
</td>
<td style="text-align:left;">
FOR SF, OBS\_TEST\_CONCEPT\_NAME Blood in Stool - Recent Change in
Rectal Bleeding Amount
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
If from both sources at baseline separate by semicolon. This is R for
the 6 pt Mayo & 9 pt Mayo Scores. None; No blood seen=0 Visible blood in
stool less than half the time; Blood less than 50% of the time=1 Visible
blood in stool half the time or more; Blood 50% or more of the time=2
Passing blood alone; ECRF: Blood Passed Alone = Yes=3
</td>
</tr>
<tr>
<td style="text-align:right;">
177
</td>
<td style="text-align:left;">
STOOL\_FREQ\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC;
</td>
<td style="text-align:left;">
FOR SF, OBS\_TEST\_CONCEPT\_NAME equals Recent Change in Daily Stool
Frequency. For ECRF, OBS\_TEST\_CONCEPT\_NAME equals Recent Change in
Daily Stool Frequency
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
If from both sources at baseline separate by semicolon. This is S for
the 6 pt Mayo & 9pt Mayo Scores Normal=0 1-2 stools/day more than
normal=1 3-4 stools/day more than normal=2 &gt;4 stools/day more than
normal; 5 or more stools per day more than normal=3
</td>
</tr>
<tr>
<td style="text-align:right;">
178
</td>
<td style="text-align:left;">
GLOBAL\_ASSESSMENT\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Value closest to index date within specified index range
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
OBS\_TEST\_CONCEPT\_NAME equals Physician’s Global Assessment of Current
Disease Status
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
This is T for the Mayo 9 Score. Quiescent=0 Mild=1 Moderate=2 Severe=3
</td>
</tr>
<tr>
<td style="text-align:right;">
179
</td>
<td style="text-align:left;">
MAYO\_6\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Ulcerative Colitis Only. Only use SF or ECRF to calculate; Do not
combine from multiple sources to calculate. If one variable is missing
from equation, leave blank for that source. MAYO\_6\_SCORE=S+R
</td>
</tr>
<tr>
<td style="text-align:right;">
180
</td>
<td style="text-align:left;">
MAYO\_9\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Ulcerative Colitis Only. Only use SF or ECRF to calculate; Do not
combine from multiple sources to calculate. If one variable is missing
from equation, leave blank for that source. MAYO\_9\_SCORE=S+R+T
</td>
</tr>
<tr>
<td style="text-align:right;">
181
</td>
<td style="text-align:left;">
MAYO\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of score within index range of index date
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
OBS\_TEST\_RESULT\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
182
</td>
<td style="text-align:left;">
MAYO6\_CATEGORY
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The disease activity category for the MAYO\_6\_SCORE
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Ulcerative Colitis Only. - Remission: 0-1 - Mild: 2-3 - Moderate: 4-5 -
Severe: 6
</td>
</tr>
<tr>
<td style="text-align:right;">
183
</td>
<td style="text-align:left;">
MAYO\_SOURCE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The data source for the MAYO6 & MAYO9 score.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC; ECRF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATA\_SOURCE
</td>
<td style="text-align:left;">
Ulcerative Colitis Only.
</td>
</tr>
<tr>
<td style="text-align:right;">
184
</td>
<td style="text-align:left;">
PGA
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Physician global assessment closest to index date within specified index
range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
OBS\_TEST\_CONCEPT\_CODE equals "EPIC#16411" or "SMART\_Q9\_\_C".
</td>
<td style="text-align:left;">
DESCRIPTIVE\_SYMP\_TEST\_RESULTS
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
185
</td>
<td style="text-align:left;">
PGA\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of PGA Score
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
OBS\_TEST\_RESULT\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
186
</td>
<td style="text-align:left;">
PGA\_SOURCE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The data source for the PGA
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:left;">
SF\_SPARC
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DATA\_SOURCE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
187
</td>
<td style="text-align:left;">
DISEASE\_ACTIVITY\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
The disease activity category for each patient closest to the index date
within the index range (XX)
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The SCDAI\_CATEGORY or CD patients, the MAYO6\_CATEGORY for UC patients,
or the PGA for IBDU patients or when no sCDAI or 6pt Mayo score
available
</td>
</tr>
<tr>
<td style="text-align:right;">
188
</td>
<td style="text-align:left;">
SES\_SUBSCORE\_ILEUM
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Crohn’s Disease Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
189
</td>
<td style="text-align:left;">
SES\_SUBSCORE\_LEFT\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Crohn’s Disease Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
190
</td>
<td style="text-align:left;">
SES\_SUBSCORE\_RECTUM
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Crohn’s Disease Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
191
</td>
<td style="text-align:left;">
SES\_SUBSCORE\_RIGHT\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Crohn’s Disease Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
192
</td>
<td style="text-align:left;">
SES\_SUBSCORE\_TRANSVERSE\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Crohn’s Disease Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
193
</td>
<td style="text-align:left;">
SES\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The closest endoscopy score for CD patients to the index date within the
specified index range.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
SES\_CD\_SUBSCORE
</td>
<td style="text-align:left;">
Sum of SES\_CD\_SUBSCORE from each location from same colonoscopy. If
segment is “Not reached” then treated as 0. For Crohn’s Disease patients
only.
</td>
</tr>
<tr>
<td style="text-align:right;">
194
</td>
<td style="text-align:left;">
RECTUM
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Ulcerative Colitis Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
195
</td>
<td style="text-align:left;">
SIGMOID\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Ulcerative Colitis Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
196
</td>
<td style="text-align:left;">
RIGHT\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Ulcerative Colitis Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
197
</td>
<td style="text-align:left;">
DESCENDING\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Ulcerative Colitis Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
198
</td>
<td style="text-align:left;">
TRANSVERSE\_COLON
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Component of Endoscopy Score for Ulcerative Colitis Patients
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
LOCATION
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
199
</td>
<td style="text-align:left;">
MAX\_EXTENT\_ACTIVE\_DISEASE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The closest endoscopy score for UC patients to the index date within the
specified index range.
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
MAX\_EXTENT\_ACTIVE\_DISEASE
</td>
<td style="text-align:left;">
Units = cm
</td>
</tr>
<tr>
<td style="text-align:right;">
200
</td>
<td style="text-align:left;">
MAYO\_ENDOSCOPY\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The closest endoscopy score for UC patients to the index date within the
specified index range.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
MAYO\_ENDOSCOPIC\_SUBSCORE
</td>
<td style="text-align:left;">
Maximum MAYO\_ENDOSCOPIC\_SUBSCORE from each location from same
colonoscopy. For Ulcerative Colitis patients only.
</td>
</tr>
<tr>
<td style="text-align:right;">
201
</td>
<td style="text-align:left;">
MODIFIED\_MAYO\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The closest endoscopy score for UC patients to the index date within the
specified index range.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
MAYO\_ENDOSCOPIC\_SUBSCORE
</td>
<td style="text-align:left;">
Sum of MAYO\_ENDOSCOPIC\_SUBSCORE from each location from same
colonoscopy. For Ulcerative Colitis patients only.
</td>
</tr>
<tr>
<td style="text-align:right;">
202
</td>
<td style="text-align:left;">
EXTENDED\_MODIFIED\_MAYO\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The closest endoscopy score for UC patients to the index date within the
specified index range.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
EMS = MODIFIED\_MAYO\_SCORE / (MAX\_EXTENT\_ACTIVE\_DISEASE/10)
</td>
</tr>
<tr>
<td style="text-align:right;">
203
</td>
<td style="text-align:left;">
MODIFIED\_MAYO\_ENDOSCOPIC\_SCORE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
The closest endoscopy score for UC patients to the index date within the
specified index range.
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
\[Calculated\]
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
MMES = EMS / Number of Segments with Active Disease
</td>
</tr>
<tr>
<td style="text-align:right;">
204
</td>
<td style="text-align:left;">
ENDO\_CATEGORY
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Disease severity categorization from MES or SES score from endoscopy
closest to index date within specified index range
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
For CD Patients using SES SCORE: - Remission: 0-2 - Mild: 3-6 -
Moderate: 7-15  
- Severe: &gt; 15 For UC Patients using Mayo Endoscopy Score (MES): -
Remission: 0 - Mild: 1 - Moderate: 2  
- Severe: 3
</td>
</tr>
<tr>
<td style="text-align:right;">
205
</td>
<td style="text-align:left;">
ENDO\_DATE
</td>
<td style="text-align:left;">
Summary Table; Scores at Index Table
</td>
<td style="text-align:left;">
Date of Endoscopy
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
Procedures
</td>
<td style="text-align:left;">
ECRF\_SPARC
</td>
<td style="text-align:left;">
PROC\_CONCEPT\_NAME equals Colonoscopy/Sigmoidoscopy
</td>
<td style="text-align:left;">
PROC\_START\_DATE
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
206
</td>
<td style="text-align:left;">
ENDOSCOPY\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has endoscopy within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has an endoscopy with a MES or SES Score within XX days of
index date
</td>
</tr>
<tr>
<td style="text-align:right;">
207
</td>
<td style="text-align:left;">
BLOOD\_PLASMA\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has biosample within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has a biosample available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
208
</td>
<td style="text-align:left;">
BLOOD\_RNA\_IN\_PAXGENE\_TUBES\_TO\_BE\_ISOLATED\_\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has biosample within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has a biosample available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
209
</td>
<td style="text-align:left;">
TISSUE\_LN2\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has biosample within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has a biosample available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
210
</td>
<td style="text-align:left;">
TISSUE\_DNA\_EXTRACTION\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has biosample within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has a biosample available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
211
</td>
<td style="text-align:left;">
TISSUE\_RNALATER\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has biosample within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has a biosample available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
212
</td>
<td style="text-align:left;">
TISSUE\_FORMALIN\_JAR\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has biosample within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has a biosample available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
213
</td>
<td style="text-align:left;">
GENOTYPING\_GLOBAL\_SCREENING\_ARRAY\_\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
214
</td>
<td style="text-align:left;">
WHOLE\_EXOME\_SEQUENCING\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
215
</td>
<td style="text-align:left;">
IMMUNOSEQ\_TCRB\_ASSAY\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
216
</td>
<td style="text-align:left;">
RNASEQ\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
217
</td>
<td style="text-align:left;">
WHOLE\_SHOTGUN\_SEQUENCING\_WGS\_\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
218
</td>
<td style="text-align:left;">
ITS2\_SEQUENCING\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
219
</td>
<td style="text-align:left;">
VIRAL\_METAGENOMICS\_SEQUENCING\_VIROME\_\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
<tr>
<td style="text-align:right;">
220
</td>
<td style="text-align:left;">
PROTEOMIC\_BIOMARKER\_PANELS\_OLINK\_\_XX
</td>
<td style="text-align:left;">
Summary Table
</td>
<td style="text-align:left;">
Flag if a patient has omics data generated within xx days of index date
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1 = patient has omics data available within xx days of index date
</td>
</tr>
</tbody>
</table>
