print("start")
library(data.table)

#Data=fread("mnt/VigieChiro/Raw/SpNuit2Valid_DI_0_DataLP_PF_exportTot.csv")
Data=fread("C:/wamp64/www/DataOpeni.csv")

test="5ab8eb985d4d3fa86e0003ab"


print(summary(test %in% Data$participation))

if(test %in% Data$participation){
  
  Datasub=subset(Data,Data$participation==test)
  print(summary(Datasub))
  print(head(Datasub))
}


#Ftest=list.files("mnt/DataGroup",recursive=T,full.names=T)

#for (i in 1:length(Ftest))
#{
#print(Ftest[i])
#Try=fread(Ftest[i])
#}



#library(moments)



print("Cool !!")