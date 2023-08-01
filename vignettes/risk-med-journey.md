# RISK Med Journey

The RISK medication journey table documents the long term medication
journey of
[RISK](https://www.crohnscolitisfoundation.org/research/current-research-initiatives/pediatric-risk-stratification)
pediatric patients for biologics, aminosalicylates and immunomodulators.
The medication start and end dates are from the case report forms for
the RISK study. The columns included in the RISK medication journey are:

<table>
<colgroup>
<col style="width: 27%" />
<col style="width: 72%" />
</colgroup>
<thead>
<tr class="header">
<th>Column</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>DEIDENTIFIED_MASTER_PATIENT_ID</td>
<td>Unique patient ID</td>
</tr>
<tr class="even">
<td>MEDICATION_NAME</td>
<td>Name of the medication patient was on</td>
</tr>
<tr class="odd">
<td>MED_START_DATE</td>
<td>The earliest reported start date of a medication from the RISK case
report form. If no medication start date was supplied but it was
indicated the medication was administered, the medication start date is
the visit encounter date associated with the first time the medication
was administered.</td>
</tr>
<tr class="even">
<td>MED_END_DATE</td>
<td>The reported medication end date. If a patient has multiple
medication end dates but only one medication start date, the latest
medication end date is used. If the patient has multiple medication
start and end dates, each interval for the medication is a separate row
in the medication journey table. The logic for the imputed medication
end date is explained further below.</td>
</tr>
<tr class="odd">
<td>MED_ORDER</td>
<td>An index that counts the order in which a patient received
medications based on medication start date. The first medication a
patient received == 1.</td>
</tr>
<tr class="even">
<td>MED_GROUP</td>
<td>The class of medication. Either biologic, 5-ASA (aminosalicylates),
or immunomodulators.</td>
</tr>
<tr class="odd">
<td>MEDS_OVERLAP</td>
<td>List of medications that overlap with the medication a patient is on
at any point in the interval.</td>
</tr>
<tr class="even">
<td>OVERLAPPING_MOA</td>
<td>List of overlapping mechanisms of action that overlap with the
medication is on at any point in the interval.</td>
</tr>
<tr class="odd">
<td>OVERLAP_DAYS_[MEDICATION NAME]</td>
<td>Count of the number of days an additional medication overlapped with
the original medication interval. The possible overlapping medications
include: Adalimumab, Azathioprine, Certolizumab Pegol, Infliximab
(Unspecified), Mercaptopurine, Mesalamine, Methotrexate, Natalizumab,
Olsalazine, Sulfasalazine, and Tacrolimus.</td>
</tr>
<tr class="even">
<td>ANTIBIOTICS_OVERLAP</td>
<td>1 if the patient was also on antibiotics at any point in the
medication interval.</td>
</tr>
<tr class="odd">
<td>STEROID_OVERLAP</td>
<td>1 if the patient was also on steroids at any point in the medication
interval.</td>
</tr>
<tr class="even">
<td>END_DATE_IMPUTED</td>
<td>1 if the medication end date was imputed, 0 if the medication end
date was directly reported on a CRF.</td>
</tr>
</tbody>
</table>

### Medication End Date Logic

For all medications other than Natalizumab and Infliximab, there is an
option on the RISK case report form to report a medication end date.
When that end date is reported, that is the MED\_END\_DATE used in the
med journey table and the column END\_DATE\_IMPUTED will have a 0.

For all medications other than Natalizumab and Infliximab if no end date
is reported, the last RISK visit encounter date is used as the
medication end date. The column END\_DATE\_IMPUTED will be 1 for these
rows.

#### Anti-TNF Logic

Some patients reported being on two different anti-TNF medications at
some point in their medication journey. In the case that there was no
reported medication end date for one anti-TNF medication and another
anti-TNF medication was started, then the day before the start date of
the next anti-TNF is used as the medication end date for the prior
medication. The column END\_DATE\_IMPUTED will be 1 for these rows.

For Natalizumab and Infliximab there was no option on the RISK case
report form to report the medication end date. If the end date for these
medications was not imputed because of the start of another anti-TNF
medication, the last reported medication administered date is used as
the medication end date. The column END\_DATE\_IMPUTED will be 1 for
these rows.

### Overlapping Medications

Medications are considered overlapping if there is at least 1 day where
the medication intervals overlap. If a medication was started on the
same day of another medication end date, these medications are not
considered overlapping.
