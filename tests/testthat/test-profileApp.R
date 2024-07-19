library(shinytest)

app <- ShinyDriver$new("./fixtures")

test_that("all expected inputs and outputs exist", {
    widgets <- app$listWidgets()

    expected_inputs <- c(
        'profile-id-idSelect',
        'profile-overview-domainSelect',
        'profile-tableWrap'
    ) %>% sort()
    actual_inputs <- sort(widgets$input)

    expect_true(all(
        actual_inputs == expected_inputs
    ))

    expected_outputs <- c(
            'profile-ae_plot_ui-AEplot',
            'profile-ae_plot_ui-AEtable',
            'profile-ae_plot_ui-text1',
            'profile-id-demogList',
            'profile-lb_tbl_ui-lb_tbl',
            'profile-overview-overview'
        ) %>% sort()
    actual_outputs <- sort(widgets$output)

    expect_true(all(
        actual_outputs == expected_outputs
    ))
})

app$stop()
