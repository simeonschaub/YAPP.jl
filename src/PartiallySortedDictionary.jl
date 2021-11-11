struct PartiallySortedIndices{T, P <: PartiallySorted{T}} <: AbstractIndices{T}
    indices::Dict{T, NTuple{2, Int}}
    values::P
end