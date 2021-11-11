struct PartiallySortedIndices{T, P <: PartiallySorted{T}, F} <: AbstractIndices{T}
    indices::Dict{T, NTuple{2, Int}}
    values::P
    get_class::F # T -> Int
end

Base.iterate(x::PartiallySortedIndices, state...) = iterate(x.values, state...)
Base.in(i, x::PartiallySortedIndices) = haskey(x.indices, i)
Base.length(x::PartiallySortedIndices) = length(x.indices)

Dictionaries.isinsertable(::PartiallySortedIndices) = true
function _insert!(x::PartiallySortedIndices, i)
    values = x.values
    class = x.get_class(i)
    push!(values, i, class)
    idx = (class, lastindex(values.classes[class]))
    x.indices[i] = idx
    return idx
end
Base.insert!(x::PartiallySortedIndices, i) = (_insert!(x, i); x)
function _delete!(x::PartiallySortedIndices, i)
    idx = pop!(x.indices, i)
    deleteat!(x.values, idx)
    return idx
end
Base.delete!(x::PartiallySortedIndices, i) = (_delete!(x, i); x)

struct PartiallySortedDictionary{
    K, V, I <: PartiallySortedIndices{K}, P <: PartiallySorted{V},
} <: AbstractDictionary{K, V}
    indices::I
    values::P
end

Base.@propagate_inbounds function Base.getindex(x::PartiallySortedDictionary, k)
    idx = x.indices.indices[k]
    return x.values[idx]
end
Base.isassigned(x::PartiallySortedDictionary, k) = k in keys(x)
Base.keys(x::PartiallySortedDictionary) = x.indices

Dictionaries.issettable(::PartiallySortedDictionary) = true
function Base.setindex!(x::PartiallySortedDictionary, v, k)
    idx = x.indices.indices[k]
    x.values[idx] = v
    return x
end

Dictionaries.isinsertable(::PartiallySortedDictionary) = true
function Base.insert!(x::PartiallySortedDictionary, k)
    class, _ = _insert!(x, k)
    push!(x.values, k, class)
    return x
end
function Base.delete!(x::PartiallySortedDictionary, k)
    idx = _delete!(x, k)
    deleteat!(x.values, idx)
    return x
end