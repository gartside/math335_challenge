#!/usr/bin/env Rscript

library(tidyverse)

names_year <- read_csv("names_year.csv")

#utility functions
getNameTibble  <- function(x) names_year %>% filter(name==x) %>% select(c(year, Total))
getGrandTotal <- function(x) names_year %>% filter(year==x) %>% select(c(Total)) %>% sum()

# question one
james_data <- getNameTibble("James")

james_data %>% ggplot(aes(year, Total)) +
    geom_point() +
    labs(title="Frequency of children named James by year",
         x="Year",
         y="Yearly total") +
    geom_vline(xintercept=1964, linetype='dashed', color='blue') +
    annotate("text", x = 1969, y = 0, label = "1964", color = "blue")
ggsave("plot1.png")

james_data <- james_data %>%
    mutate(grandTotal = (year %>% modify(function(x) getGrandTotal(x))),
           propGrandTotal = Total/grandTotal)
james_data %>%
    ggplot(aes(year, grandTotal)) +
        geom_point() +
        labs(title="Total number of children born each year",
             x="Year", y="Number born")
ggsave("plot2.png")
james_data %>%
    ggplot(aes(year, propGrandTotal)) +
    geom_point() +
    labs(title="Proportion of children named James by year",
         x="Year",
         y="Yearly proportion") +
    geom_vline(xintercept=1964, linetype='dashed', color='blue') +
    annotate("text", x = 1969, y = 0, label = "1964", color = "blue")
ggsave("plot3.png")

# question two
brittany_data <- getNameTibble("Brittany")
brittany_data <- brittany_data %>%
    mutate(grandTotal = (year %>% modify(function(x) getGrandTotal(x))),
           propGrandTotal = Total/grandTotal)
brittany_data %>%
    ggplot(aes(year, propGrandTotal)) +
        geom_point() +
    labs(title="Proportion of children named Brittany by year",
         x="Year",
         y="Yearly proportion")
maxBrittanyIndex <- brittany_data$propGrandTotal %>% which.max()
maxBrittanyYear <- brittany_data$year[maxBrittanyIndex]
ggsave("plot4.png")

# question three
mary_data <- getNameTibble("Mary") %>%
    filter(year >= 1920) %>%
    filter(year <= 2000) %>%
    mutate(name = "Mary") %>%
    mutate(propGrandTotal = Total/(year %>% modify(function(x) getGrandTotal(x))))
martha_data <- getNameTibble("Martha") %>%
    filter(year >= 1920) %>%
    filter(year <= 2000) %>%
    mutate(name = "Martha") %>%
    mutate(propGrandTotal = Total/(year %>% modify(function(x) getGrandTotal(x))))
peter_data <- getNameTibble("Peter") %>%
    filter(year >= 1920) %>%
    filter(year <= 2000) %>%
    mutate(name = "Peter") %>%
    mutate(propGrandTotal = Total/(year %>% modify(function(x) getGrandTotal(x))))
paul_data <- getNameTibble("Paul") %>%
    filter(year >= 1920) %>%
    filter(year <= 2000) %>%
    mutate(name = "Paul") %>%
    mutate(propGrandTotal = Total/(year %>% modify(function(x) getGrandTotal(x))))
combined_data <- rbind(mary_data, martha_data, peter_data, paul_data)
combined_data %>%
    ggplot(aes(year, propGrandTotal, color = name)) +
    geom_point() +
    labs(title="Proportion of children by year",
         x="Year",
         y="Yearly proportion") +
    scale_color_brewer(palette = "Set2") # ok for color blind people
ggsave("plot5.png")

# question four
harry_data <- getNameTibble("Harry")
harry_data <- harry_data %>%
    mutate(propGrandTotal = Total/(year %>% modify(function(x) getGrandTotal(x))))
harry_data %>%
    ggplot(aes(year, propGrandTotal)) +
    geom_point() +
    labs(title="Proportion of children named Harry by year",
         x="Year",
         y="Yearly proportion") +
    geom_vline(xintercept=2001, linetype='dashed', color='blue') +
    annotate("text", x = 2006, y = 0.001, label = "2001", color = "blue")
ggsave("plot6.png")
harry_data %>%
    filter(year > 1985) %>%
    ggplot(aes(year, propGrandTotal)) +
        geom_point() +
        labs(title="Proportion of children named Harry by year",
             x="Year",
             y="Yearly proportion") +
        geom_vline(xintercept=2001, linetype='dashed', color='blue') +
        annotate("text", x = 2003, y = 0.0001, label = "2001", color = "blue")
ggsave("plot7.png")
