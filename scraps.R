
## according to supplemental info, method should be switched to SPM but that does not appear to have occurred
#unique(test$method_type)

# 
# ggplot(test_altered, aes(x = diff_either)) +
#   geom_histogram(binwidth = .1) +
#   theme_minimal()
# 
# ggplot(test_altered |> filter(diff_either <= 2.5, diff_either >= -2.5), 
#        aes(x = diff_either)) +
#   geom_histogram(binwidth = .1) +
#   theme_minimal()
# 
