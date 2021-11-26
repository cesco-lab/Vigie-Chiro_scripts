F1=list.files("E:/Chiropteres_FORECCAsT",recursive=T,full.names=T,pattern="log.txt$")
F2=list.files("E:/Chiropteres_FORECCAsT",recursive=T,full.names=T,pattern="meta.csv$")
F3=list.files("E:/Chiropteres_FORECCAsT",recursive=T,full.names=T,pattern="settings.ini$")

file.remove(F1)
file.remove(F2)
file.remove(F3)