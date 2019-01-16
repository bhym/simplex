#function to create random DNA sequence
randDNA = function(n){
  paste(sample(c("A","T","C","G"), n, replace=TRUE), collapse="")
}

# Evolution of a DNA sequence according to kimura model
# Transition rate
Alpha=(1/2)*10^-6
# Transversion rate
Beta=(1/4)*10^-6
# Alpha + 2*Beta = total mutation rate per generation
# Inputs: sequence, t (number of generations)
evolution_kimura<-function(sequence, t){
  # random probabilities of having mutations drawn form a uniform distribution
  random_mut <- runif(n = nchar(sequence), 0, 1)
  sequence_new <- sequence
  p0 = 0.25*(1+exp(-4*Beta*t)+2*exp(-2*(Alpha+Beta)*t))
  p1 = 0.25*(1-exp(-4*Beta*t))
  p2 = 0.25*(1+exp(-4*Beta*t)-2*exp(-2*(Alpha+Beta)*t))
  substitution_mat <- matrix(c(p0, p1, p1, p2, p1, p0, p2, p1, p1, p2, p0, p1, p2, p1, p1, p0), 
                             ncol=4, nrow=4, byrow = T)
  letters_dna <- c("A","T","C","G")
  # find mutation sites
  mutation_sites <- which(random_mut<1-p0)
  for (u in mutation_sites){
    vector <- match(letters_dna, substr(sequence, u,u), nomatch = 0)
    probability_vector <- vector%*%substitution_mat-random_mut[u]
    new_letters <- which(probability_vector == min(probability_vector))
    if (length(new_letters)==2){
      new_letter <- new_letters[round(runif(1, 1, 2))]
    } else{
      new_letter <- new_letters
    }
    substr(sequence_new, u,u) <- letters_dna[new_letter]
  }
  # return new sequence and number of mutations
  return(c(sequence_new, length(mutation_sites)))
}
