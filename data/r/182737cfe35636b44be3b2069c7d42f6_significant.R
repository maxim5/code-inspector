data = c(perm.dist)
data = sort(data)
sig = .05
left.tail=data[floor(length(data)*sig)]
right.tail=data[floor(length(data)*(1-sig))]

data = perm.dist
data = apply(data,2,sort)
sig = .05
left.tail=data[floor(nrow(data)*sig)]
right.tail=data[floor(length(data)*(1-sig))]
