# We start by importing the data

aa = read.delim(file = "american_airline_empl.txt" , sep = "")
da = read.delim(file = "delta_airline_empl.txt" , sep = "")
fe = read.delim(file = "federal_express_empl.txt" , sep = "")
ua = read.delim(file = "united_airline_empl.txt" , sep = "")

#Next we need to convert the columns imported as string to numeric

aa$Full.time = as.numeric(gsub("," , "" , aa$Full.time ))
aa$Part.time = as.numeric(gsub("," , "" , aa$Part.time ))
aa$Grand = as.numeric(gsub("," , "" , aa$Grand ))

da$Full.time = as.numeric(gsub("," , "" , da$Full.time ))
da$Part.time = as.numeric(gsub("," , "" , da$Part.time ))
da$Grand = as.numeric(gsub("," , "" , da$Grand ))

fe$Full.time = as.numeric(gsub("," , "" , fe$Full.time ))
fe$Part.time = as.numeric(gsub("," , "" , fe$Part.time ))
fe$Grand = as.numeric(gsub("," , "" , fe$Grand ))

ua$Full.time = as.numeric(gsub("," , "" , ua$Full.time ))
ua$Part.time = as.numeric(gsub("," , "" , ua$Part.time ))
ua$Grand = as.numeric(gsub("," , "" , ua$Grand ))

aa = tibble(aa)
da = tibble(da)
fe = tibble(fe)
ua = tibble(ua)

# to successfuly merge the 4 tibbles at hand, we first change the name of the columns to avoid confusion

aa = rename(aa , 'American Airlines FT' = 'Full.time' , 'American Airlines PT' = 'Part.time' , 'American Airlines Grand' = 'Grand' )
da = rename(da , 'Delta Airlines FT' = 'Full.time' , 'Delta Airlines PT' = 'Part.time' , 'Delta Airlines Grand' = 'Grand' )
fe = rename(fe , 'Federal Express FT' = 'Full.time' , 'Federal Express PT' = 'Part.time' , 'Federal Express Grand' = 'Grand' )
ua = rename(ua , 'United Airlines FT' = 'Full.time' , 'United Airlines PT' = 'Part.time' , 'United Airlines Grand' = 'Grand' )

#Next we create a column out of the combination of year and month columns, which corresponds to a date column. We need also to add a day column!
aa$day = 1
aa = unite(aa, date , Year , Month , day , sep = "-")
aa$date = ymd(aa$date)

da$day = 1
da = unite(da, date , Year , Month , day , sep = "-")
da$date = ymd(da$date)

fe$day = 1
fe = unite(fe, date , Year , Month , day , sep = "-")
fe$date = ymd(fe$date)

ua$day = 1
ua = unite(ua, date , Year , Month , day , sep = "-")
ua$date = ymd(ua$date)

df = merge(aa , da , by = 'date')
df = merge(df , fe , by = 'date')
df = merge(df , ua , by = 'date')

df = select(df , -Total.x , -Total.y)

ggplot(df, aes(x = date)) +
  geom_line(aes(y = `American Airlines FT`, color = "American Airlines")) +
  geom_line(aes(y = `Delta Airlines FT`, color = "Delta Airlines")) +
  geom_line(aes(y = `Federal Express FT`, color = "Federal Express")) +
  geom_line(aes(y = `United Airlines FT`, color = "United Airlines")) +
  scale_color_manual(name = "Legends", values = c("American Airlines" = "red", "Delta Airlines" = "blue" , "Federal Express" = "green" , "United Airlines" = 'orange')) +
  labs(x = "Date", y = "Number of Full-Time Employees by  Airline") +
  theme_classic()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+ theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave("full time employees.png", width = 15, height = 6)

ggplot(df, aes(x = date)) +
  geom_line(aes(y = `American Airlines PT`, color = "American Airlines")) +
  geom_line(aes(y = `Delta Airlines PT`, color = "Delta Airlines")) +
  geom_line(aes(y = `Federal Express PT`, color = "Federal Express")) +
  geom_line(aes(y = `United Airlines PT`, color = "United Airlines")) +
  scale_color_manual(name = "Legends", values = c("American Airlines" = "red", "Delta Airlines" = "blue" , "Federal Express" = "green" , "United Airlines" = 'orange')) +
  labs(x = "Date", y = "Number of Part-Time Employees by Airline") +
  theme_classic()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+ theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


ggsave("part time employees.png", width = 15, height = 6)

df$ptf_aa = df$`American Airlines PT`/(df$`American Airlines FT` + df$`American Airlines PT`)
df$ptf_da = df$`Delta Airlines PT`/(df$`Delta Airlines FT` + df$`Delta Airlines PT`)
df$ptf_fe = df$`Federal Express PT`/(df$`Federal Express FT` + df$`Federal Express PT`)
df$ptf_ua = df$`United Airlines PT` / (df$`United Airlines FT` + df$`United Airlines PT`)

ggplot(df, aes(x = date)) +
  geom_line(aes(y = `ptf_aa`, color = "American Airlines")) +
  geom_line(aes(y = `ptf_da`, color = "Delta Airlines")) +
  geom_line(aes(y = `ptf_fe`, color = "Federal Express")) +
  geom_line(aes(y = `ptf_ua`, color = "United Airlines")) +
  scale_color_manual(name = "Legends", values = c("American Airlines" = "red", "Delta Airlines" = "blue" , "Federal Express" = "green" , "United Airlines" = 'orange')) +
  labs(x = "Date", y = "Fraction of Part-Time Employees by Airline") +
  theme_classic()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+ theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave("fraction of part time employees.png", width = 15, height = 6)

print(df[which.max(df$`American Airlines Grand`),1])
print(df[which.max(df$`Delta Airlines Grand`),1])
print(df[which.max(df$`Federal Express Grand`),1])
print(df[which.max(df$`United Airlines Grand`),1])

print(df[which.min(df$`American Airlines Grand`),1])
print(df[which.min(df$`Delta Airlines Grand`),1])
print(df[which.min(df$`Federal Express Grand`),1])
print(df[which.min(df$`United Airlines Grand`),1])