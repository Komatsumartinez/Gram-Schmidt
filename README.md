# Gram-Schmidt Process
  This repository contains an implementation of the Gram-Schmidt process in F#.

# Overview
  The Gram-Schmidt process is a method for orthonormalizing a set of vectors in an inner product space. Given a set of linearly independent vectors, the Gram-Schmidt    process produces a set of orthonormal vectors that span the same subspace.

  This implementation of the Gram-Schmidt process is defined in the GramSchmidt.fs module, which includes several helper functions for performing vector operations, such as computing dot products and scaling vectors.

# Usage
  To use this implementation of the Gram-Schmidt process, simply call the gramSchmidt function with a list of input vectors. The function returns a list of orthonormal vectors that span the same subspace as the input vectors.

let inputVectors = [
    [| 1.0; 0.0; 0.0 |]
    [| 1.0; 1.0; 0.0 |]
    [| 1.0; 1.0; 1.0 |]
]

let outputVectors = gramSchmidt inputVectors
