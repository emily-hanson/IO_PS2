# I hope this file gives you a laugh if you look at it because I did things in a very inefficient Nalinda way

library(stringr)

industryFile <- transform(industryFile, C1_COUNT_TOTAL_Commuteout = 1 - 
		industryFile$"C1_COUNT_TOTAL_Commute within census subdivision (CSD) of residence"/
		industryFile$"C1_COUNT_TOTAL_Total - Commuting destination for the employed labour force aged 15 years and over with a usual place of work - 25% sample data")

industryFile <- transform(industryFile, C1_COUNT_TOTAL_Dwellingperkm = 
		industryFile$"C1_COUNT_TOTAL_Total.private.dwellings"/
		industryFile$"C1_COUNT_TOTAL_Land.area.in.square.kilometres")

industryFile <- transform(industryFile, C1_COUNT_TOTAL_Dwellingperpop = 
		industryFile$"C1_COUNT_TOTAL_Private.dwellings.occupied.by.usual.residents"/
		industryFile$"C1_COUNT_TOTAL_Population..2021")

split_cols <- str_split_fixed(industryFile$C1_COUNT_TOTAL_65.years.and.over, " ", 2)
split_cols <- as.data.frame(split_cols)
C1_COUNT_TOTAL_65.years.and.over <- sub(")","",split_cols$V2)
C1_COUNT_TOTAL_65.years.and.over <- as.numeric(C1_COUNT_TOTAL_65.years.and.over)
industryFile$C1_COUNT_TOTAL_65.years.and.over <- C1_COUNT_TOTAL_65.years.and.over

