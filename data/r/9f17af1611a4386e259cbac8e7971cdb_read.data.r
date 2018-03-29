data <- read.table('data-sv/sv.sanitized-access.20070110')
# REmove some superflous columns
data <- data[, -8] #
data <- data[, -2]
# Name columns properly
colnames(data) <- c('time', 'client', 'type', 'size', 'verb', 'address', 'original', 'mimetype')
# Normalize time
data['time'] <- data['time'] - data[1, 'time']

# Filter only GET requests and only some columns
getData <- data[data['verb'] == 'GET', c('time', 'client', 'address')]

# Remove some yahoo ping that fucks up results
getData <- getData[getData['address'] != 'http://us.dl1.yimg.com/download.yahoo.com/pgdownload8/msgup_us.yim', ]

# Data is taken from 24 hours, but we don't want to wait so long for tests
getData['time'] <- getData['time'] / 18

# Take only half of the data
getData <- getData[1:250000, ]

# Get unique clients
clients <- unique(getData[, 'client'])
counts <- c('client', 0)
for (client in clients) {
  clientData <- getData[getData['client'] == client, c('time', 'address')]
  counts <- rbind(counts, c(client, length(clientData[, 1])))
  write.csv(clientData, file = paste('data/clients/', client, sep=''), col.names=FALSE, row.names=FALSE, quote=FALSE, sep=',')
}
#counts2 <- read.table('data-uc/counts.txt')
counts2 <- as.data.frame(counts[-1, c(1, 2)])
summary(as.numeric(as.character(counts2[, 2])))

write.table(counts2, file='data/counts.txt', row.names=FALSE, col.names=FALSE, quote=FALSE, sep=' ')
write.table(clients, file='data/clients.txt', row.names=FALSE, col.names=FALSE, quote=FALSE, sep=' ')
 