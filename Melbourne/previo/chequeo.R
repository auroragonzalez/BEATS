py = read.table("chequeoValidacion.csv", sep=",", head=T)
mat = read.table("validacionMatlab.csv", sep=",")


s1py = py[py$X==1,2]
s1ma = as.numeric(mat[1,])

s1ma %in% s1py
s1py %in% s1ma

round(s1ma,3) %in% round(s1py,3)
s1py %in% s1ma
