extract_powerBi <- function(input_json) {
  data <- input_json$results[[1]]$result$data
  dm0 <- data$dsr$DS[[1]]$PH[[1]]$DM0
  
  # Colunas
  columns_types <- dm0[[1]]$S
  columns <- sapply(data$descriptor$Select, function(item) {
    if (item$Kind == 1) {
      return(item$GroupKeys[[1]]$Source$Property)
    } else {
      return(item$Value)
    }
  })
  value_dicts <- data$dsr$DS[[1]]$ValueDicts
  
  # Valores
  df <- dm0 %>% 
    reconstruct_arrays(columns_types) %>%
    expand_values(columns_types, value_dicts) %>% 
    replace_newlines_with("") %>% 
    purrr::map(magrittr::extract2, "C") %>% 
    purrr::map(unlist) %>% 
    purrr::map(setNames, columns) %>% 
    dplyr::bind_rows()
  
  return(df)
}


reconstruct_arrays <- function(dm0, columns_types) {
  length <- length(columns_types)
  prevItem <- NULL
  for (index in 1:length(dm0)) {
    
    item <- dm0[[index]]
    currentItem <- item$C
    if ("R" %in% names(item) || "Ø" %in% names(item)) {
      copyBitset <- ifelse("R" %in% names(item), item$R, 0)
      #print(names(item))
      if ("Ø" %in% names(item)){
        print(names(item))
        print("situação problema")
      }
      deleteBitSet <- ifelse("Ø" %in% names(item), item$A, 0)
      for (i in 1:length) {
        if (is_bit_set_for_index(i - 1, copyBitset)) {
          currentItem <- append(currentItem, prevItem[i], after = i-1)
        } else if (is_bit_set_for_index(i - 1, deleteBitSet)) {
          currentItem <- append(currentItem, NULL, after = i-1)
        }
      }
    }
    dm0[[index]]$C <- currentItem
    prevItem <- currentItem
  }
  return(dm0)
}

is_bit_set_for_index <- function(index, bitset) {
  return(bitwAnd(bitset, bitwShiftL(1, index)) != 0)
}

expand_values <- function(dm0, columns_types, value_dicts) {
  for (idx in 1:length(columns_types)) {
    col <- columns_types[[idx]]
    if ("DN" %in% names(col)) {
      for (index in 1:length(dm0)) {
        item <- dm0[[index]]
        dataItem <- item$C
        if (is.integer(dataItem[[idx]])) {
          valDict <- value_dicts[[col$DN]]
          dataItem[idx] <- valDict[[dataItem[[idx]] + 1]]
        }
        dm0[[index]]$C <- dataItem
      }
    }
  }
  return(dm0)
}

replace_newlines_with <- function(dm0, replacement) {
  for (item in dm0) {
    elem <- item$C
    for (i in 1:length(elem)) {
      if (is.character(elem[[i]])) {
        elem[[i]] <- gsub("\n", replacement, elem[[i]])
      }
    }
  }
  return(dm0)
}

