# DJT expenditures
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)

DJT_exps <- read_csv("C:/Users/vivek/Downloads/donald-j-trump-for-president-inc/expenditures.csv")
View(DJT_exps)

str(DJT_exps)
summary(DJT_exps)

# making changes to data frame 
DJT_exps$`Payee state`<-as.factor(DJT_exps$`Payee state`)
DJT_exps$`Payee name`<-as.factor(DJT_exps$`Payee name`)
DJT_exps$`Entity type`<-as.factor(DJT_exps$`Entity type`)
DJT_exps$Purpose<-as.factor(DJT_exps$Purpose)

#converting the amount to numeric after stripping the '$' symbol
DJT_exps[6] <- lapply(DJT_exps[6], function(x) as.numeric(gsub("[,$]", "", x)))


summary(DJT_exps)

#splitting based on positive and negative.

DJT_exps_pos<-DJT_exps[which(DJT_exps$Amount>0),]
View(DJT_exps_pos)

DJT_exps_neg<-DJT_exps[which(DJT_exps$Amount<0),]
View(DJT_exps_neg)


#pos_exp_by_payee
DJT_exps_pos %>% 
  select(`Entity type`,Amount,`Payee name`) %>%
  group_by(`Entity type`,`Payee name`) %>%
  summarise(value=sum(Amount))-> DJT_exps_pos_by_payee

DJT_exps_pos_by_payee<-DJT_exps_pos_by_payee[order(-(DJT_exps_pos_by_payee$value)),]

View(DJT_exps_pos_by_payee)

#payee by state

DJT_exps_pos %>% 
  select(Amount,`Payee state`) %>%
  group_by(`Payee state`) %>%
  summarise(value=sum(Amount))-> DJT_exps_pos_by_state



DJT_exps_pos_by_state<-DJT_exps_pos_by_state[order(-(DJT_exps_pos_by_state$value)),]
View(DJT_exps_pos_by_state)


#Adding actual state names 

listofstates_org<-unique(DJT_exps$`Payee state`)
listofstates_org

# Is Ala. referring to Alaska or Alabama? Assuming Ala is Alabama, since AK is Alaska.
nos<-c( "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE" ,"FL", "GA", "HI", "ID", "IL", "IN",
"IA", "KS", "KY", "LA", "ME", "MD", "MA",
"MI", "MN" ,"MS", "MO" ,"MT" ,"NE" ,"NV" ,"NH" ,
"NJ" ,"NM" ,"NY" ,"NC" ,"ND", "OH", "OK", "OR", "PA" ,
"RI" ,"SC", "SD", "TN","TX", "UT", "VT" ,"VA" ,"WA", "WV", "WI", "WY")




co_de<-c( "AL", "AZ", "AR", "CA", "CO", "CT", "DC","FL", "GA", "IL", "IN",
          "IA", "KS", "KY", "LA", "MA","MD",
          "MI", "MS", "MO" ,"NC", "NH" ,"NJ" ,"NM" ,
          "NY" ,"NE" ,"NV" , "OH", "OK",  "PA" ,
          "SC",  "TN","TX", "UT", "VA" ,"WA",  "WI", "WY", "ZZ")


nos_np<-c("Alabama" ,"Arizona", "Arkansas" ,"California"  ,   "Colorado"      , "Connecticut"  ,
          "DC"  , "Florida"     ,   "Georgia" ,"Illinois" ,      "Indiana"  , "Iowa", "Kansas",
"Kentucky"  ,     "Louisiana","Massachusetts","Maryland", 
             "Michigan" ,    "Mississippi"   ,
           "Missouri" ,"North Carolina", "New Hampshire","New Jersey", "New Mexico","New York","Nebraska" ,"Nevada",
"Ohio",       "Oklahoma", "Pennsylvania", "South Carolina","Tennessee",   "Texas","Utah", "Virginia", "Washington",
          "Wisconsin", "Wyoming", "Unknown" )

misssss<-c("AK","DE","ID","ME", "MN" ,"MT","ND","OR","RI","SD","VT", "WV")

DJT_exps_pos_by_state<-DJT_exps_pos_by_state[order((DJT_exps_pos_by_state$`Payee state`)),]

DJT_exps_pos_by_state<-cbind.data.frame(DJT_exps_pos_by_state,nos_p)

DJT_exps_pos_by_state<-cbind.data.frame(DJT_exps_pos_by_state,nos_np)



df<-DJT_exps_pos_by_state

#Plotting on map


df$hover <- with(df, paste(state, '<br>', "Total", `value`))
# give state boundaries a white border
l <- list(color = toRGB("black"), width = 1)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('blue')
)

plot_ly(df, z = df$value, text = df$hover, locations = df$nos_p, type = 'choropleth',
        locationmode = 'USA-states', color = df$value, colors = 'Greens',
        marker = list(line = l), colorbar = list(title = "Millions USD")) %>%
  layout(title = 'DJT  by State<br>(Hover for breakdown)', geo = g)



