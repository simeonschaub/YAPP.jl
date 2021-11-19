module YAPP

using Dictionaries, MultivariatePolynomials, BangBang, LinearAlgebra
macro inbounds(x) esc(x) end

include("PartiallySorted.jl")
include("PartiallySortedDictionary.jl")
include("Polynomial.jl")

end
