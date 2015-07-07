context("Diagnostics")

test_that("generate_table produces the correct output", {

    # Unconditional Table ----------------------
    blogsville_table <- generate_table(plotname = "blogsville", dataframe = blogsville, y = "tmax")
    expect_equal(dim(blogsville_table), c(6, 13))
    expect_equal(file.exists("blogsville_cor_explained.pdf"), TRUE)
    expect_equal(file.exists("blogsville_cor_matrices.pdf"), TRUE)
    file.remove("blogsville_cor_explained.pdf")
    file.remove("blogsville_cor_matrices.pdf")


    # Conditional Table ----------------------
    conditional_table_one <- generate_table(plotname = "blogsville_one",
                                            dataframe = blogsville, y = "pcrp",
                                            conditional = TRUE, conditional_step = 1)
    conditional_table_two <- generate_table(plotname = "blogsville_two",
                                dataframe = blogsville, y = "pcrp",
                                conditional = TRUE, conditional_step = 2)

    expect_equal(dim(conditional_table_one), c(6, 13))
    expect_equal(dim(conditional_table_two), c(6, 13))
    expect_equal(file.exists("blogsville_one_cor_explained.pdf"), TRUE)
    expect_equal(file.exists("blogsville_one_cor_matrices.pdf"), TRUE)
    expect_equal(file.exists("blogsville_two_cor_explained.pdf"), TRUE)
    expect_equal(file.exists("blogsville_two_cor_matrices.pdf"), TRUE)
    file.remove("blogsville_one_cor_explained.pdf")
    file.remove("blogsville_one_cor_matrices.pdf")
    file.remove("blogsville_two_cor_explained.pdf")
    file.remove("blogsville_two_cor_matrices.pdf")
    }
)
