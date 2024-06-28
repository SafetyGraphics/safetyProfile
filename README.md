# {safetyProfile}

The {safetyProfile} package contains a shiny application that allows users to view subject-level
safety profile reports for patients participating in clinical trials. The visualizations included in
the package present a patient-centric summary of parameter changes over the trial period, including
laboratory results, concomitant medications, adverse events along with participant demographics
data. The {safetyProfile} can be used as a stand-alone shiny application and at the same time its
flexible shiny modules allow the tool to be used as a part of the {safetyGraphics} R package (robust
safety monitoring framework). The package has been developed as part of the
<a target="_blank" href="https://safetygraphics.github.io/">Interactive Safety Graphics (ISG) workstream</a>
of the ASA Biopharm-DIA Safety Working Group. 

## Package Usage

First install the package from GitHub:

```
`devtools::install_github('safetyGraphics/safetyProfile', ref = 'main')`
```

Then run the stand-alone app with example clinical data:

```
safetyProfile::profileApp()
```

To use your own data specify your data via a named list and optionally define your column names:

```
    # ADaM data
    data <- list(
        dm = adsl,
        aes = adae,
        labs = adlb
    )

    settings <- list(
        dm = list(
            id_col = "USUBJID",
            treatment_col = "ARM",
            sex_col = "SEX",
            race_col = "RACE",
            age_col = "AGE"
        ),
        aes = list(
            id_col = "USUBJID",
            stdy_col = "ASTDY",
            endy_col = "AENDY",
            term_col = "AEDECOD",
            bodsys_col = "AEBODSYS",
            severity_col = "AESEV"
        ),
        labs = list(
            id_col = "USUBJID",
            visit_col = "AVISIT"
            studyday_col = "ADY",
            measure_col = "PARAM",
            value_col = "AVAL",
            normal_col_low = "ANRLO", 
            normal_col_high = "ANRHI"
        )
    )

    profileApp(
        data = data,
        settings = settings
    )
```
