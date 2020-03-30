library(data.tree)

#reading data and making it as a data frame
data = read.csv("C://Users//JARVIS//Desktop//Bhargavi//College//Productive Crap//Pattern Recognition//Decision Tree//play_tennis.csv", sep=',', header=TRUE)
data = data.frame(data)

#finding the attributes
attributes = colnames(data)
attributes = attributes[c(-1,-ncol(data))]

#entropy function
entropyCalc <- function(s) {
  p1 = s[1]/sum(s)
  p2 = s[2]/sum(s)
  if(p1 !=0 && p2 != 0)
    entropy = -p1*log2(p1)-p2*log2(p2)
  else
    entropy = 0
  return (entropy)
}

#information gain function
informationGain <- function(s,a){
  newdata = data.frame(data[a],data$play)
  t=table(newdata)
  sum1 = c()
  for(i in 1:(length(t[ ,1])))
    sum1[i] = t[i,1] + t[i,2]
  tot = 0
  for(i in 1:length(sum1)) {
    s1 = c(t[i,1], t[i,2])
    tot = tot + (sum1[i]/sum(sum1)*entropyCalc(s1))
  }
  entropy_play = entropyCalc(s)
  return(entropy_play - tot)
}

#function to check if the subset is completely pure
IsPure <- function(data) {
  if(length(unique(data)) == 1)
    return(TRUE)
  else
    return (FALSE)
}

#id3 algorithm for various options of a given algorithm
id3 = function(node,d,attribute){
  data = d[attribute]  
  child1 = unique(data[,ncol(data)])
  children = c()
  for(i in 1:length(child1))
    children[i] = toString(child1[i])
  parent = node
  for( i in 1:length(children))
    if(length(attributes)>0){
      node = parent$AddChild(name = children[i])
      dChild = d[d[attribute] == children[i],]
      #print(dChild)
      if(IsPure(dChild$play))
        child = node$AddChild(name = unique(dChild$play))
      else{
        t = data.frame(table(dChild$play))
        s = c()
        s[1] = t[1, "Freq"]
        s[2] = t[2, "Freq"]
        gain = c()
        for( i in 1:length(attributes))
          gain[i] = informationGain(s,attributes[i])
        
        #choosing the attribute with maximum gain
        max_gain = max(gain)
        pos=0
        for( i in 1:length(gain))
          if(max_gain == gain[i]){
            pos=i
            break
          }
        selected_attr = attributes[pos]
        attributes = attributes[-pos]
        node = node$AddChild(name = selected_attr)
        node = id3(node,dChild,selected_attr)
      }
    }
}

#main function
t = data.frame(table(data$play))
s = c()
s[1] = t[1, "Freq"]
s[2] = t[2, "Freq"]
gain = c()
for( i in 1:length(attributes))
  gain[i] = informationGain(s,attributes[i])
#choosing the attribute with maximumgain
max_gain = max(gain)
pos=0
for( i in 1:length(gain))
  if(max_gain == gain[i]){
    pos=i
    break
  }
selected_attr = attributes[pos]
attributes = attributes[-pos]

playTennis <- Node$new(name = selected_attr)
node = playTennis
node = id3(node,data,selected_attr)
print(playTennis)

