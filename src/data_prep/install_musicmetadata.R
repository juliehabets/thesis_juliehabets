install.packages("devtools")
devtools::install_github("hannesdatta/musicMetadata", force = TRUE)


library(musicMetadata)

# Classify single labels
classify_labels('Interscope')

# Classify vector of labels
labels <- c('300 Entertainment/Atlantic', 'Bad Boy Records', 'Virgin Records Ltd')
data.frame(label=labels, parent_label = classify_labels(labels, concatenate = T))
