Two studies in IBD Plexus have integrated electronic medical record
(EMR) data:

1.  A Study of a Prospective Adult Research Cohort with Inflammatory
    Bowel Disease (SPARC IBD) is a longitudinal study following adult
    IBD patients as they receive care at 17 different sites across the
    United States. To learn more about SPARC IBD and it’s development
    please see \[The Development and Initial Findings of A Study of a
    Prospective Adult Research Cohort with Inflammatory Bowel Disease
    (SPARC IBD)\]
    ([https://doi.org/10.1093/ibd/izab071).](https://doi.org/10.1093/ibd/izab071).)

2.  IBD Qorus is a prospective cohort focused on improving the quality
    of care for patients with IBD. Currently there are over 30 sites
    enrolling patients across the United States. To learn more about IBD
    Qorus please see
    <https://www.crohnscolitisfoundation.org/research/ibd-qorus>.

This vignette details the functions built into the ibdplexus package to
help navigate the EMR data for these two studies.

    library(ibdplexus, quietly = T)
    library(tidyr, quietly = T)
    library(dplyr, quietly = T)
    #> 
    #> Attaching package: 'dplyr'
    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag
    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union
    library(lubridate, quietly = T)
    #> 
    #> Attaching package: 'lubridate'
    #> The following objects are masked from 'package:base':
    #> 
    #>     date, intersect, setdiff, union

# Calculating BMI

`calculate_bmi()` filters the observations\_emr domain to find the
weight and height of a patient. Weight is converted to kg and height is
converted to meters. For patients with more than one BMI, outliers are
removed. This function produces a long data.frame with one row per
patient BMI.

In the `sparc_summary()` function, this data.frame is cut to find the
BMI closest to the specified index date.

Future versions of this package will include a similar functionality for
a Qorus summary table.

# Extracting Diagnosis Codes from the EMR

`emr_extract_diagnosis()` filters the diagnosis and patient\_problem
table based on specified inclusion and exclusion criteria.

This function can search through the EMR using ICD10 codes or keywords.
The default function will search through the EMR using ICD10 codes. One
may search for comorbidities, general disease complications, common
symptoms, extra-intestinal manifestations, cancer, common clinical trial
criteria, and infectious diseases using the pre-set condition categories
outlined below. There is also an option to custom input ICD10 codes.

The text search option of this function filters through EMR diagnosis
descriptions for any keyword. All relevant diagnosis will be returned
for the keyword inputted. Diagnosis descriptions that include the
specified keyword but are not a positive diagnosis for the patient will
not be included. For example, rows are dropped if they are a negative
diagnosis (“without” diagnosis), are screenings or scans for a
diagnosis, are family history of the diagnosis, or are indicating risk
for the diagnosis.

There is also the option to use both codes and text to search through
the EMR for a diagnosis. More specifics on the inputs for code and text
search options is detailed in the emr\_extract\_diagnosis parameters
section of this document.

The result is a list of data.frames with all relevant EMR ICD10
diagnosis, patient problem and/or patient problem records.

The list of pre-programmed ICD10 codes is below.

<table>
<colgroup>
<col style="width: 20%" />
<col style="width: 50%" />
<col style="width: 29%" />
</colgroup>
<thead>
<tr class="header">
<th>Condition Categories</th>
<th>ICD 10 Diagnosis Codes</th>
<th>ICD 9 Diagnosis Codes</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>SYSTEMIC FUNGAL INFECTION</td>
<td>B39, B45, B38, B40, B46, B44, B37, B59</td>
<td>110-118</td>
</tr>
<tr class="even">
<td>CANCER</td>
<td>All C codes, d0, d1, d2, d3, d4</td>
<td>140-229</td>
</tr>
<tr class="odd">
<td>COLORECTAL CANCER</td>
<td>C18, C19, C20</td>
<td>153, 154.0, 154.1</td>
</tr>
<tr class="even">
<td>CERVIX CARCINOMA</td>
<td>D06</td>
<td>233.1</td>
</tr>
<tr class="odd">
<td>SKIN CARCINOMA</td>
<td>C44.01, C44.02, C44.11, C44.12, C44.21, C44.22, C44.31, C44.32,
C44.41, C44.42, C44.51, C44.52, C44.61, C44.62, C44.71, C44.72, C44.81,
C44.82, C44.91, C44.92</td>
<td>232</td>
</tr>
<tr class="even">
<td>STOMA</td>
<td>L24.B0, L24.B1, L24.B3, Z93.3, Z93.2</td>
<td>V44.2, V44.3</td>
</tr>
<tr class="odd">
<td>DEMYELINATING DISORDER</td>
<td>G35, G36, G37</td>
<td>340, 341</td>
</tr>
<tr class="even">
<td>CELIAC</td>
<td>K90.0</td>
<td>579.0</td>
</tr>
<tr class="odd">
<td>PSC</td>
<td>K83.01</td>
<td>576.1</td>
</tr>
<tr class="even">
<td>GI BLEEDING</td>
<td>K92.1</td>
<td>569.3, 578.1, 599.70, 777.3, 792.1</td>
</tr>
<tr class="odd">
<td>GI ULCER</td>
<td>K25, K27, K28, K26, K63.3, K62.6</td>
<td>531-534</td>
</tr>
<tr class="even">
<td>PERIANAL ABSCESS OR FISTULA</td>
<td>K50.913, K50.914, K50.813, K50.814, K50.013, K50.014, K50.113,
K50.114, K51.013, K51.014, K51.213, K51.214, K51.313, K51.314, K51.413,
K51.414, K51.513, K51.514, K51.813, K51.814, K51.913, K51.914, K60,
K61</td>
<td>565.1, 566</td>
</tr>
<tr class="odd">
<td>WEIGHT LOSS</td>
<td>R63.4</td>
<td>783.1, 783.2</td>
</tr>
<tr class="even">
<td>B2 OR B3</td>
<td>K50.912, K50.112, K50.012, K50.812</td>
<td>560.89, 560.9</td>
</tr>
<tr class="odd">
<td>MALNOURISHMENT</td>
<td>E4</td>
<td>263.9, 269.9</td>
</tr>
<tr class="even">
<td>ANEMIA</td>
<td>D50, D51, D52, D53</td>
<td>280-281</td>
</tr>
<tr class="odd">
<td>DIARRHEA</td>
<td>R19.7, K59.1, K58.0</td>
<td>564.5, 787.91</td>
</tr>
<tr class="even">
<td>NAUSEA OR VOMITING</td>
<td>R11</td>
<td>787.0</td>
</tr>
<tr class="odd">
<td>HYPOALBUMINEMIA</td>
<td>E88.09</td>
<td>273.8</td>
</tr>
<tr class="even">
<td>FEVER</td>
<td>R50.9, R61</td>
<td>780.6</td>
</tr>
<tr class="odd">
<td>ABDOMINAL PAIN</td>
<td>R10</td>
<td>789.0</td>
</tr>
<tr class="even">
<td>CDI</td>
<td>A04.7</td>
<td>008.45</td>
</tr>
<tr class="odd">
<td>ARTHRITIS OR LOW BACK PAIN</td>
<td>M13, M05, M06, M07, M08, M10, M11, M12, M14, M1A, ( need to add
M54.5)</td>
<td>710-716, 724.2</td>
</tr>
<tr class="even">
<td>DACTYLITIS</td>
<td>L08.9</td>
<td>686.9</td>
</tr>
<tr class="odd">
<td>NON UC IBD DIAGNOSIS</td>
<td>K50, K52.3, K52.83, K55.9</td>
<td>555, 558.9</td>
</tr>
<tr class="even">
<td>TOXIC MEGACOLON</td>
<td>K59.31</td>
<td>564.7</td>
</tr>
<tr class="odd">
<td>FULMINANT COLITIS</td>
<td>K55.03</td>
<td>557.0</td>
</tr>
<tr class="even">
<td>INTRAABDOMINAL ABSCESS</td>
<td>L02.211, K65.1</td>
<td>567.22, 682.2</td>
</tr>
<tr class="odd">
<td>STRICTURE STENOSIS</td>
<td>K56.69</td>
<td>560.89</td>
</tr>
<tr class="even">
<td>COLON ADENOMA</td>
<td>D12.2, D12.3, D12.4, D12.5, D12.6, K31.A2, K55.20</td>
<td>211.3, 235.2</td>
</tr>
<tr class="odd">
<td>INFECTION</td>
<td>L0, A49, A0 (need to refine and add B99)</td>
<td>001-009,130-136</td>
</tr>
<tr class="even">
<td>TUBERCULOSIS</td>
<td>A15, A17, A18, A19</td>
<td>010-018</td>
</tr>
<tr class="odd">
<td>DIABETES</td>
<td>E08, E09, E10, E11, E13</td>
<td>250</td>
</tr>
<tr class="even">
<td>HYPERTENSION</td>
<td>I10, I11, I12, I13, I15, I16</td>
<td>401-405</td>
</tr>
<tr class="odd">
<td>COPD</td>
<td>J44</td>
<td>491.21, 493.2, 496</td>
</tr>
<tr class="even">
<td>CKD STAGE IIB OR MORE</td>
<td>N18.32, N18.4, N18.5</td>
<td>585.2-585.5</td>
</tr>
<tr class="odd">
<td>UNSTABLE ANGINA OR MYOCARDIAL INFARCTION</td>
<td>I20, I21</td>
<td>410, 412, 413</td>
</tr>
<tr class="even">
<td>AUTOIMMUNE INFLAMMATORY DISEASE</td>
<td>M05, M06, M3, M04</td>
<td>714, 710</td>
</tr>
<tr class="odd">
<td>HEPATITIS B</td>
<td>B16, B18.0, B18.1, B19.1</td>
<td>070.2, 070.3</td>
</tr>
<tr class="even">
<td>HEPATITIS C</td>
<td>B17.1, B18.2, B19.2</td>
<td>070.41, 070.44, 070.51, 070.54, 070.7</td>
</tr>
<tr class="odd">
<td>INHERITED AUTOIMMUNE DISORDER</td>
<td>D80.0, D82.0, D80.4, D82.3, N41.4, Q82.8, D81.0, D81.1, D81.2,
D81.3, E70.330, D76.1, D82.4, D82.2, D81.6, D81.7, D83, D80.2, D84.1,
G11.3, D81.5, D81.8</td>
<td>279.04, 279.12, 279.02, 279.8, 601.8, 757.2, 757.39, 279.2, 270.2,
288.4, 279.8, 279.06, 279.01, 277.6, 334.8, 277.2, 266.2</td>
</tr>
</tbody>
</table>

### emr\_extract\_diagnosis parameters

`data` is a list of data frames typically generated by load\_data from
the ibdplexus package. It must include demographics and the diagnosis,
patient problem and/or patient history tables. Only EMR data is
required.

`code_search` is the option to extract relevant diagnosis codes using
ICD 10 codes. This defaults to true in the function. If code\_search is
true, the inclusion parameter must be either one of the pre-set
condition categories from above, or “CUSTOM”. The function will not run
if code\_search is true and an unknown character string is in the
inclusion criteria.

`text_search` is the option to extract relevant diagnosis codes using
keyword text search. This defaults to false. When set to true and
code\_search is false, the inclusion parameter can be any string of
characters. The different arguments of the character string should be
separated by a pipe (eg. “argument1|argument2”).

`inclusion` When searching by code, the diagnosis category of ICD 10
codes to search for. Can choose between predefined categories outlined
above or enter a custom defined list. If searching for a custom defined
list, this parameter must be set to “CUSTOM”, and the list should be
specified in the `custominc` parameter. When text searching, a regular
expression string of keywords to search for (eg.
“argument1|argument2|argument3”)

**note for combined text and code search**: The function allows for
code\_search and text\_search to both be true, so the function will
filter through the EMR using both ICD 10 codes and keyword text search.
Currently, the function will only run with both of these options if the
input is one of the specific ICD 10 condition categories. For example,
if text\_search = T and code\_search = T with inclusion = “COLORECTAL
CANCER”, the function will return all results that match the ICD 10
codes in the colorectal cancer condition category, as well as any
results that have colorectal cancer in the description of the diagnosis
but are not one of the specified ICD 10 categories.

`exclusion` diagnosis category of ICD 10 codes to exclude from search.
Can currently be either “SKIN CARCINOMA”, “CERVIX CARCINOMA”, or
“CUSTOM” (for which the custom exclusion list will be specified in the
`customexc` parameter). More predefined lists can be included in future
updates of the function.

`custominc` optional list of custom ICD 10 codes to be searched for. The
variable “inclusion” must also be set to “CUSTOM” for this option. If
multiple inputs, needs to be in a regular expression string (eg.
“argument1|argument2|argument3”)

`customexc` optional list of custom ICD 10 codes to be excluded from the
search. The variable “exclusion” must also be set to “CUSTOM” for this
option. If multiple inputs, needs to be in a regular expression string
(eg. “argument1|argument2|argument3”)

`datecutoff` Type of date cutoff to be used for EMR extract. Current
options include “ENROLLMENT” - number of days before consent into study.

`index_range` Number of days before date cutoff to be used for EMR
extract. Time must be specified in number of days. Default is 36500 days
(100 years).
