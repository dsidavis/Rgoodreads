getSimpleFields =
function(x, dataframe = TRUE, ...)
{
  i = xmlSApply(x, function(x)  {
                     kids = xmlChildren(x)
                     (length(kids) == 1 &&  inherits(kids[[1]], c("XMLInternalTextNode", "XMLInternalCDataNode"))) || (length(kids) == 0)
                })
  ans = structure(lapply(x[i], getFieldValue), names = sapply(x[i], xmlName))
  if(dataframe)
      as.data.frame(ans, ...)
  else
      ans
}


getFieldValue =
function(node)
{
  type = xmlGetAttr(node, "type", "")

  if(xmlGetAttr(node, "nil", FALSE) == "true")
      return(NA)

  if(xmlSize(node) == 0)
      return(NA)
  
  val = xmlValue(node)
  
  switch(type,
          integer = as.integer(val),
          boolean = as.logical(toupper(val)),
          val
         )
}


addElement =
function(x, field) {
    i = !(field %in% names(x))
    if(any(i))
        x[field[i]] = NA
    x
}    
