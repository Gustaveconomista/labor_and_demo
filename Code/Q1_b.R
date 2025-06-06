# DATA PREPARATION SECTION
# Filter the main dataset (sdem) to include only women aged 25-65 and create child indicator
df = sdem %>%
  filter(sex == 2, eda %in% 25:65) %>%  # Filter for females (sex == 2) aged 25-65
  mutate(hj_peq = case_when(
    # Create indicator for women with young children: 1 if has children AND has child under 16 AND is in partnership
    n_hij != 0 & chld_less16 != 0 & (par_c %in% c(101,201)) ~ 1,
    T ~ 0  # Otherwise assign 0 (no young children or not in partnership)
  ))

# MAIN TABLE GENERATION FUNCTION
# Function to create summary statistics tables by marital status
Tables = function(df1, status){
  # Filter dataset by specific marital status (5=married, 1=cohabiting, 6=single)
  df1 = df1 %>% 
    filter(e_con == status)
  
  # Initialize empty 5x8 matrix to store results (5 age groups, 8 columns for stats)
  final = matrix(0, 5, 8)
  final = data.frame(final)
  
  # ROW 1: ALL WOMEN (regardless of age/children status)
  # Columns 1&5: Overall employment rate and weekly earnings for all women
  final[1,c(1,5)] = c(mean(df1$Employed), mean(df1$Wage/52))
  
  # Columns 2&6: Employment rate and weekly earnings for women with less than high school education
  final[1,c(2,6)] = df1 %>% 
    filter(cs_p13_1 < 4) %>%  # Education level less than 4 (less than HS)
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Columns 3&7: Employment rate and weekly earnings for women with high school education
  final[1,c(3,7)] = df1 %>% 
    filter(cs_p13_1 == 4) %>%  # Education level equals 4 (high school)
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Columns 4&8: Employment rate and weekly earnings for women with college or above
  final[1,c(4,8)] = df1 %>%
    filter(cs_p13_1 %in% c(7,8,9)) %>%  # Education levels 7,8,9 (college or above)
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # ROW 2: WOMEN UNDER 35 WITH YOUNG CHILDREN
  # Overall stats for women under 35 with young children
  final[2,c(1,5)] = df1 %>% 
    filter(hj_peq == 1, eda < 35) %>%  # Has young child indicator = 1 AND age < 35
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Same group broken down by education levels (less than HS)
  final[2,c(2,6)] = df1 %>% 
    filter(hj_peq == 1, eda < 35, cs_p13_1 < 4) %>% 
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Same group broken down by education levels (high school)
  final[2,c(3,7)] = df1 %>% 
    filter(hj_peq == 1, eda < 35, cs_p13_1 == 4) %>% 
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Same group broken down by education levels (college+)
  final[2,c(4,8)] = df1 %>% 
    filter(hj_peq == 1, eda < 35,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # ROW 3: WOMEN UNDER 35 WITH NO SMALL CHILDREN
  # Overall stats for women under 35 without young children
  final[3,c(1,5)] = df1 %>% 
    filter(hj_peq == 0, eda < 35) %>%  # No young child indicator = 0 AND age < 35
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Same group broken down by education levels (less than HS, HS, college+)
  final[3,c(2,6)] = df1 %>%
    filter(hj_peq == 0, eda < 35,cs_p13_1 < 4) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  final[3,c(3,7)] = df1 %>%
    filter(hj_peq == 0, eda < 35,cs_p13_1 == 4) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  final[3,c(4,8)] = df1 %>% 
    filter(hj_peq == 0, eda < 35,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # ROW 4: WOMEN AGED 35-54
  # Overall stats for middle-aged women (35-54)
  final[4,c(1,5)] = df1 %>% 
    filter(eda >= 35, eda<=54) %>%  # Age between 35 and 54
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Same group broken down by education levels
  final[4,c(2,6)] = df1 %>%
    filter(eda >= 35, eda<=54,cs_p13_1 < 4) %>% 
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  final[4,c(3,7)] = df1 %>%
    filter(eda >= 35, eda<=54,cs_p13_1 == 4) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  final[4,c(4,8)] = df1 %>%
    filter(eda >= 35, eda<=54,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # ROW 5: WOMEN AGED 55 AND OLDER
  # Overall stats for older women (55+)
  final[5,c(1,5)] = df1 %>% 
    filter(eda >= 55) %>%  # Age 55 and above
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Same group broken down by education levels
  final[5,c(2,6)] = df1 %>% 
    filter(eda >= 55,cs_p13_1 < 4) %>% 
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  final[5,c(3,7)] = df1 %>% 
    filter(eda >= 55,cs_p13_1 == 4) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  final[5,c(4,8)] = df1 %>% 
    filter(eda >= 55,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/52)) %>%
    as.numeric()
  
  # Assign descriptive row names for the 5 age/family status groups
  rownames(final) = c("All", "Less than 35 with Child Younger than 16",
                      "Less than 35 with No Small Children", "35-54",
                      "55 and Older")
  
  # Assign column names: first 4 columns for labor force participation, last 4 for earnings
  colnames(final) = c("All", "Less than HS", "High School", "College or Above",
                      "All", "Less than HS", "High School", "College or Above")  
  return(final)
}

# LATEX TABLE GENERATION FUNCTION
# Function to convert data tables into professionally formatted LaTeX tables
create_final_table = function(df, status, status_name, output_file = NULL) {
  # Generate the data table using the Tables function
  table_data = Tables(df, status)
  
  # Create a clean label name for LaTeX referencing (lowercase, hyphens instead of spaces)
  label_name = tolower(gsub(" ", "-", status_name))
  
  # Build LaTeX table code following professional academic formatting standards
  latex_code = c(
    "\\begin{table}[htbp]",  # Table environment with positioning options
    "    \\centering",        # Center the table
    paste("    \\caption{Labor Force Participation and Earnings -", status_name, "Aged 25-65}"),  # Table caption
    paste("    \\label{tab:", label_name, "}", sep = ""),  # Table label for cross-referencing
    "    \\resizebox{\\textwidth}{!}{%",  # Resize table to fit page width
    "    \\begin{tabular}{lcccccccc}",    # Table structure: 1 left-aligned, 8 center-aligned columns
    "    \\toprule",  # Top horizontal line (booktabs package)
    # Create grouped column headers spanning multiple columns
    "     & \\multicolumn{4}{c}{\\textbf{Labor Force Participation}} & \\multicolumn{4}{c}{\\textbf{Earnings}} \\\\",
    "    \\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",  # Partial horizontal lines under group headers
    # Individual column headers
    "     & All & Less than HS & High School & College+ & All & Less than HS & High School & College+ \\\\",
    "    \\midrule"  # Middle horizontal line separating headers from data
  )
  
  # Define row labels matching the data structure
  row_names = c("All", 
                "Less than 35 with Child Younger than 16",
                "Less than 35 with No Small Children", 
                "35-54",
                "55 and Older")
  
  # Add data rows to LaTeX code
  for(i in 1:nrow(table_data)) {
    # Format numbers to 3 decimal places for consistent presentation
    formatted_numbers = sprintf("%.3f", table_data[i,])
    # Construct each data row with proper LaTeX formatting
    data_row = paste("    ", row_names[i], " & ", 
                     paste(formatted_numbers, collapse = " & "),
                     " \\\\", sep = "")
    latex_code = c(latex_code, data_row)
  }
  
  # Close the LaTeX table structure
  latex_code = c(latex_code,
                 "    \\bottomrule",      # Bottom horizontal line
                 "    \\end{tabular}%",  # End tabular environment
                 "    }",                # Close resizebox
                 "    \\end{table}")     # End table environment
  
  # Output handling: save to file if specified, always print to console
  if(!is.null(output_file)) {
    writeLines(latex_code, output_file)  # Write LaTeX code to .tex file
  }
  
  cat(latex_code, sep = "\n")  # Display LaTeX code in console
  return(latex_code)           # Return code for further processing if needed
}

# BATCH TABLE GENERATION FUNCTION
# Utility function to generate all three required tables at once
generate_all_tables = function(df) {
  # Define the three marital status categories as specified in the problem set
  status_list = list(
    list(code = 5, name = "Married Women"),      # Marital status code 5
    list(code = 1, name = "Cohabiting Women"),   # Marital status code 1
    list(code = 6, name = "Single Women")        # Marital status code 6
  )
  
  # Loop through each marital status and generate corresponding table
  for(status_info in status_list) {
    # Print progress indicators for user feedback
    cat("\n", rep("=", 60), "\n")
    cat("GENERATING TABLE FOR:", status_info$name, "\n")
    cat(rep("=", 60), "\n")
    
    # Create standardized filename based on group name
    filename = paste0(tolower(gsub(" ", "_", status_info$name)), "_table.tex")
    # Generate the table
    create_final_table(df, status_info$code, status_info$name, filename)
    
    cat("\nTable saved to:", filename, "\n")
  }
}

# EXECUTION SECTION
# Generate all tables using the batch function
generate_all_tables(df)

# Alternative: Generate tables individually with specific file paths (using 'here' package for path management)
create_final_table(df, 5, "Married Women", here("Out", "married_women_final.tex"))
create_final_table(df, 1, "Cohabiting Women", here("Out", "cohabiting_women_final.tex")) 
create_final_table(df, 6, "Single Women", here("Out", "single_women_final.tex"))
