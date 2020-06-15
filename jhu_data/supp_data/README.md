# Supplementary Data Provided by State Governments

## Update June 15, 2020

It seems that JHU has corrected these issues and back-filled the number of
confirmed plus presumed cases, so there are no more big spikes. This data is no
longer needed now.

## Massachusetts

On June 1, 2020, Massachusetts started differentiating between confirmed
positive cases and presumed positive cases. Confirmed positive cases are those
who received a positive PCR test, indicating that they are actively infected
with the virus. Presumed positive cases are those who received a positive
antibody test and either showed COVID-19 symptoms or were exposed to someone
with COVID-19. On June 1, 2020, there was a huge spike in the number of reported
cases in MA because the state government grouped together confirmed positive and
presumed positive cases, whereas before June 1, only confirmed positive cases
were counted as positive. Also, on June 1, the MA state government updated all
previous case data to differentiate between confirmed positive and presumed
positive on the state level. On the county level, on June 1 they updated all
counties' case count on all prior days to include both confirmed and presumed
positives but do not differentiate between the two types of positives.

In the official state numbers, there is a big spike in cases on June 1 because
of this reporting issue. We attempt to rectify this by using the difference
between confirmed and presumed positive cases on June 1 and confirmed and
presumed positive cases on May 31 as the number of new cases on June 1 at the
county level. We obtained these figures from the MA coronavirus data portal:

https://www.mass.gov/info-details/archive-of-covid-19-cases-in-massachusetts

## Michigan

A similar thing happened in Michigan on June 5, 2020. There is a big spike in
cases in the JHU data. We use the Michigan state government statistics instead
of the JHU data. We downloaded this data from the [Michigan Coronavirus data
portal](https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html).
The file `Cases_by_County_by_Date` only reports the confirmed cases, not
probable cases. We can verify this by comparing it to the
`Cases_and_Deaths_by_County` data, which is also supplied. Unfortunately the
`Cases_and_Deaths_by_County` data only has data available on the last date.
