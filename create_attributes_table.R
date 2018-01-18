create_attributes_table <- function(number_attributes) {
  
  n <- number_attributes
  
  table <- data.frame(attributeName = rep("NA", n),
                      attributeDefinition = rep("NA", n),
                      measurementScale = rep("NA", n),
                      domain = rep("NA", n),
                      formatString = rep(NA, n),
                      definition = rep("NA", n),
                      unit = rep(NA, n),
                      numberType = rep(NA, n),
                      missingValueCode = rep(NA, n),
                      missingValueCodeExplanation = rep(NA, n),
                      stringsAsFactors = F)
  
  for (i in seq_len(n)) {
    
    table$attributeName[i] <- readline(prompt = paste0("Enter attribute name:\n"))
    
    table$attributeDefinition[i] <- readline(prompt = paste0("Enter attribute definition:\n"))
    
    measurementScale1 <- readline(prompt = paste0("Enter measurement scale. Choose one of: \n dateTime - d \n interval - i \n nominal  - n \n ordinal  - o \n ratio    - r \n"))
    
    switch(measurementScale1,
           "d" = {
             table$measurementScale[i] = "dateTimeDomain"
           },
           "i" = {
             table$measurementScale[i] = "interval"
           },
           "n" = {
             table$measurementScale[i] = "nominal"
           },
           "o" = {
             table$measurementScale[i] = "ordinal"
           },
           "r" = {
             table$measurementScale[i] = "ratio"
           })
    
    if (table$measurementScale[i] %in% c("nominal", "ordinal")) {
      table$domain[i] = "textDomain"
    } else if (table$measurementScale[i] %in% c("ratio", "interval")) {
      table$domain[i] = "numericDomain"
    } else {
      table$domain[i] = "dateTimeDomain"
    }
    
    table$definition[i] <- table$attributeDefinition[i]
    
    if (table$domain[i] == "numericDomain") {
      table$unit[i] <- readline(prompt = paste0("Enter units of measurement: \n"))
      table$numberType[i] <- readline(prompt = paste0("Enter number type: \n"))
    } else {
      table$unit[i] = NA
      table$numberType[i] = NA
    }
    
  }
  
  return(table)
  
}