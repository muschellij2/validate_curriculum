
## Installation

You can install `transcriptr` from GitHub with:

    # install.packages("remotes")
    remotes::install_github("muschellij2/transcriptr")

## Running the App

``` r
shiny::runGitHub("muschellij2/validate_curriculum")
```

## JHU Transcripts

For the necessary input, please visit <https://sis.jhu.edu/sswf/>, log
in and then go to Registration → My Grades, then click \[View Unofficial
Transcript\]. This should download a `Transcript.pdf`. The
`read_jhu_transcript` will read this in and give a `data.frame` of the
parsed output.

The Shiny app:
<https://jhubiostatistics.shinyapps.io/validate_curriculum/> should also
do this for you, allowing you to download a CSV of your data. If you
want the grades, you must check that box for the export.
