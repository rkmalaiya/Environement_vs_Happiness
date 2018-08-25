###HW1 -- computing Chi2 from a contingency table.
##The objectives of this homework are:
	## 1: to use variables
	## 2: to use operations on vectors
	## 3: print output to the screen (for use via script)	

##Note: see the script we did in class, in addition to the readings outlined for this week (Introduction to Statistics in R, Dalgaard) 


###Do not change anything in this block
	authors.data <- rbind(
						cbind(7836, 13112, 6026),
						cbind(53655,102383,42413),
						cbind(115615, 184541, 59226),
						cbind(161926, 340479, 62754),
						cbind(38177, 105101, 12670),
						cbind(46371, 58367, 14299)
					)
	rownames(authors.data) <- c('Rousseau','Chateaubriand','Hugo','Zola','Proust','Giraudoux')
	colnames(authors.data) <- c('PERIOD','COMMA','OTHER.PUNCTUATION')
	row.sums <- rowSums(authors.data)
	column.sums <- colSums(authors.data)
	grand.sum <- sum(authors.data)
###Do not change anything in this block


#***HERE: print to the screen: the row sums, the column sums, and the grand sum
	print(paste('Row Sums', row.sums, 'Column Sums', column.sums, 'Grand Sums', grand.sum))

#***HERE: create two new variables -- one is 'row.frequencies' the other is 'column.frequencies'
	##row.frequencies should be computed as the row sums divided by the grand sum
	row.frequencies = row.sums / grand.sum
	
	##column.frequencies should be computed as the column sums divided by the grand sum
  column.frequencies = column.sums / grand.sum
	
	
####Do not change anything in this block
	observed.table <- authors.data
	expected.table <- (row.frequencies %o% column.frequencies) * grand.sum
		#a trick to make the table into a vector -- so we can do vectorized operations
	observed.table.flattened <- c(observed.table)
	expected.table.flattened <- c(expected.table)
####Do not change anything in this block



#***HERE: 	create a new variable called 'chi2.vector'
	##		use 'observed.table.flattened' and 'expected.table.flattened' exactly as specified in the Chi2 formula
	##  	see https://en.wikipedia.org/wiki/Chi-squared_test#Example_chi-squared_test_for_categorical_data
  chi.vector = (observed.table.flattened - expected.table.flattened )^2 / expected.table.flattened
    
#***HERE: 	You will have to use a function that will sum all the elements in a vector. 
	##		create a variable called 'chi2' which is the summed values of the elements in 'chi2.vector' 
	##	Then print the 'chi2' variable to the screen
  chi2 = sum(chi.vector)
  print(chi2)
  
  