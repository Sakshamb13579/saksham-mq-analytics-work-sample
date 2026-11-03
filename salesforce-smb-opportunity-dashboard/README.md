# Salesforce Australia SMB Opportunity Dashboard

## Overview

This university group project sized and located Salesforce Australia's
untapped SMB CRM opportunity across Manufacturing, Finance and Insurance, and
Professional, Scientific and Technical Services.

## My role

I was responsible for the data architecture and dashboard engineering across
the analytical questions: building the central feature repository from the
Australian Bureau of Statistics Counts of Australian Businesses, including
Entries and Exits (CABEE), and related ABS business data; curating the
question-specific data used by the team; and building the interactive
dashboard end to end.

Two teammates contributed directly to the analysis: one developed a
size-based segmentation dimension used in two questions, and another
developed the turnover-based affordability model with its supporting research
and write-up.

This is a team deliverable. I built the data system and dashboard components
described above.

## Included files

- `dashboard/opportunity_dashboard.html` is the full interactive dashboard,
  covering clustering, opportunity sizing, entry and exit analysis, grants,
  and model evidence.
- `tableau/Q3_Grants_Formation_v2_2.twbx` is the packaged Tableau dashboard
  for the grants and business-formation analysis.
- `documentation/Q3_Data_Model.md` documents the prepared extracts and data
  flow.
- `documentation/Q3_Methodology.md` documents the measures, controls, and
  interpretation limits.

## Tools and data

The work used Python/JavaScript for the interactive dashboard, Tableau for
the Q3 package, and Excel-based source-data preparation. The ABS data covers
business counts, entries, and exits. Programme and grant information is a
prepared snapshot, not a live eligibility service.

## Interpretation limits

- Modelled CRM non-adoption is an opportunity-sizing measure, not an observed
  customer attribute.
- Scenario adoption rates and plan values are assumptions, not revenue
  forecasts.
- The cluster labels are planning bands, not claims of four naturally
  separate market types.
- Grant counts and dates describe a recorded snapshot and do not demonstrate
  grant effectiveness or conversion.
