library(tidyverse)
library(crqa)

p=as.character("Ohhh ah Ohhh ah Ohhhh aaahhh You know you love me, I know you care Just shout whenever, and I'll be there You are my love, you are my heart And we will never ever ever be apart Are we an item? Girl, quit playing We're just friends, what are you saying Said there's another and looked right in my eyes My first love broke my heart for the first time And I was like... Baby, baby, baby oooh Like baby, baby, baby nooo Like baby, baby, baby oooh I thought you'd always be mine mine Baby, baby, baby oooh Like baby, baby, baby nooo Like baby, baby, baby oooh I thought you'd always be mine mine Oh, for you I would have done whatever And I just can't believe we ain't together And I wanna play it cool, but I'm losin' you I'll buy you anything, I'll buy you any ring And I'm in pieces, baby fix me And just shake me 'til you wake me from this bad dream I'm going down, down, down, down And I just can't believe my first love won't be around And I'm like Baby, baby, baby oooh Like baby, baby, baby nooo Like baby, baby, baby oooh I thought you'd always be mine mine Baby, baby, baby oooh Like baby, baby, baby nooo Like baby, baby, baby oooh I thought you'd always be mine mine Luda! When I was 13, I had my first love There was nobody that compared to my baby And nobody came between us or could ever come above She had me going crazy, oh, I was star-struck She woke me up daily, don't need no Starbucks She made my heart pound, and skip a beat when I see her in the street and At school on the playground but I really wanna see her on the weekend She knows she got me dazing cause she was so amazing And now my heart is breaking but I just keep on saying Baby, baby, baby oooh Like baby, baby, baby nooo Like baby, baby, baby oooh I thought you'd always be mine mine Baby, baby, baby oooh Like baby, baby, baby nooo Like baby, baby, baby oooh I thought you'd always be mine mine I'm gone Yeah Yeah Yeah, Yeah Yeah Yeah Now I'm all gone Yeah Yeah Yeah, Yeah Yeah Yeah Now I'm all gone Yeah Yeah Yeah, Yeah Yeah Yeah Now I'm all gone gone, gone, gone... I'm gone") 
               

justin=p


justin <- unlist(strsplit(as.character(justin), " ")) ## make it a vector of words
justin = gsub("[[:punct:]]", "", justin) ## get rid of punctuations,
justin = tolower(justin) ## lower case everything


# we split into long character vector; delimiter=''
chars = unlist(strsplit(justin,'')) 
# what are the unique characters? 
uniqChars = unique(chars)
# let's use the apply function from plyr  to loop and recode
charSeries = apply(data.frame(chars),1,function(x) which(x==uniqChars))
charSeries[1:20]

# here are our characters
plot(charSeries,type='b')

words = unlist(strsplit(justin,' '))
uniqWords = unique(words)
uniqWords

wordSeries = apply(data.frame(words),1,function(x) which(x==uniqWords))
wordSeries[1:20]


plot(wordSeries,type='b')


head(crqa)

## initialize the parameters


delay = 1; embed = 1; rescale = 1; radius = 0.0001;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 1; checkl = list(do = TRUE, datatype = "categorical", thrshd = 10, pad=FALSE)


res = crqa(justin, justin, delay, embed, rescale, radius, normalize,
         mindiagline, minvertline, tw, datatype = "categorical")

RP = res$RP # take out the recurrence plot
RP = matrix(as.numeric(RP), nrow = length(justin)) # transform it for plotting

par(mar = c(3.8, 3.8, 0.2,2), font.axis = 2, cex.axis = 1,
    font.lab = 2, cex.lab = 1.2)

tstamp = seq(0, length(justin), 10)
cols = c("white","blue4")



plot(tstamp, tstamp, type = "n", xlab = "", ylab = "")

l = 1
for (l in 1:length(justin)){
  ind = which(RP[,l] == 1)
  points(rep(l,length(ind)), ind, cex = 1.2, col = "black", pch = 20)
  
}


###name the axes!
mtext("Lyrics", at = mean(tstamp), side = 1, line = 2, cex = 1.2, font = 2)
mtext("Lyrics", at = mean(tstamp), side = 2, line = 2, cex = 1.2, font = 2)

unlist(res[1:9])

# R O G U E - plotting actual lyrics#

### maybe a lil peek

#set parameters
delay = 1; embed = 1; rescale = 0; radius = 0.0001;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 1; whiteline = FALSE; recpt = FALSE; 
side = "both"; method = 'rqa'; metric = 'euclidean';  
datatype = "categorical"

res <- crqa(justin, justin, delay, embed, rescale, radius, normalize, 
            mindiagline, minvertline, tw, whiteline, recpt, side, method, metric, 
            datatype)

RP <- res$RP
parC <- list(unit = 10, labelx = "Time", labely = "Time", cols = "black", pcex = .5,
             pch = 15, las = 0, labax = seq(0, nrow(RP), 10),
             labay = seq(0, nrow(RP), 10))
plotRP(RP, parC)



text_zoom <- justin[80:110]
ans_zoom <- crqa(text_zoom, text_zoom, delay, embed, rescale, radius, normalize, 
                 mindiagline, minvertline, tw, whiteline, recpt, side, method, 
                 metric, datatype)
RP <- ans_zoom$RP


parC$labay <- parC$labax <- text_zoom
parC$las <- 2
parC$unit <- 1
parC$labelx <- parC$labely <- "Lyrics"
plotRP(RP, parC)
