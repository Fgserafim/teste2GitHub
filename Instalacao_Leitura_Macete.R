install.packages("tidyverse")
install.packages("magrittr")
install.packages("lubridate")
install.packages("ggplot")
install.packages("dplyr")
install.packages("tibble")
install.packages("abjur/abjData")

library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tibble)
library(abjur/abjData)


lista.de.pacotes = c("tidyverse","lubridate", "magrittr", "ggplot2") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

