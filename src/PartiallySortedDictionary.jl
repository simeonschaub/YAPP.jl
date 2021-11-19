struct PartiallySortedIndices{T, P <: PartiallySorted{T}, F} <: AbstractIndices{T}
    indices::Dict{T, NTuple{2, Int}}
    values::P
    get_class::F # T -> Int
end
function PartiallySortedIndices{T}(f) where {T}
    return PartiallySortedIndices(Dict{T, NTuple{2, Int}}(), PartiallySorted{T}(), f)
end

Base.iterate(x::PartiallySortedIndices, state...) = iterate(x.values, state...)
function Base.iterate(x::ReverseIndices{<:Any, <:PartiallySortedIndices}, state...)
    return iterate(Iterators.reverse(x.inds.values), state...)
end
function Dictionaries.iteratetoken(x::PartiallySortedIndices, state...)
    tmp = iterate(x.values, state...)
    tmp === nothing && return nothing
    i, state′ = tmp
    return gettoken(x, i)[2], state′
end
Base.in(i::T, x::PartiallySortedIndices{T}) where {T} = haskey(x.indices, i)
Base.length(x::PartiallySortedIndices) = length(x.indices)

Dictionaries.isinsertable(::PartiallySortedIndices) = true

struct PartiallySortedDictionary{
    K, V, I <: PartiallySortedIndices{K}, P <: PartiallySorted{V},
} <: AbstractDictionary{K, V}
    indices::I
    values::P
end
function PartiallySortedDictionary{K, V}(f) where {K, V}
    return PartiallySortedDictionary(PartiallySortedIndices{K}(f), PartiallySorted{V}())
end

Base.@propagate_inbounds function Base.getindex(x::PartiallySortedDictionary{K}, k::K) where {K}
    idx = x.indices.indices[k]
    return x.values[idx]
end
Base.isassigned(x::PartiallySortedDictionary{K}, k::K) where {K} = k in keys(x)
Base.keys(x::PartiallySortedDictionary) = x.indices

Dictionaries.issettable(::PartiallySortedDictionary) = true
Base.@propagate_inbounds function Base.setindex!(
    x::PartiallySortedDictionary{K, V},
    v::V,
    k::K,
) where {K, V}
    idx = x.indices.indices[k]
    x.values[idx] = v
    return x
end

Dictionaries.isinsertable(::PartiallySortedDictionary) = true

# inspired by https://github.com/JuliaFolds/BangBang.jl/blob/6e7260cd6b23fd806e4c4b93520d7327add08593/src/extras.jl#L96
function Dictionaries.gettoken(x::PartiallySortedIndices{T}, key::T; ht_keyindex=Base.ht_keyindex) where {T}
    ht = x.indices
    # Ideally, to improve performance for the case that requires
    # resizing, we should use something like `ht_keyindex` while
    # keeping computed hash value and then do something like
    # `ht_keyindex2!` if `f` returns non-`nothing`.
    keyindex = ht_keyindex(ht, key)

    hadindex = keyindex > 0
    idx = hadindex ? @inbounds(ht.vals[keyindex]) : (0, 0)
    return hadindex, (keyindex, idx)
end
function Dictionaries.gettoken!(x::PartiallySortedIndices{T}, key::T) where {T}
    hadindex, (keyindex, idx) = gettoken(x, key; ht_keyindex=Base.ht_keyindex2!)
    ht = x.indices
    age0 = ht.age
    if !hadindex
        keys = x.values
        class = x.get_class(key)

        idx = _push!(keys, key, class)
        @inbounds Base._setindex!(ht, idx, key, -keyindex)
        if ht.age != age0
            keyindex = Base.ht_keyindex2!(ht, key)
        end
        ht.age += 1
    end
    return hadindex, (keyindex, idx)
end
function Dictionaries.gettoken!(x::PartiallySortedDictionary{K}, key::K) where {K}
    ret = gettoken!(keys(x), key)
    hadindex, (_, (class, _)) = ret
    if !hadindex
        entries = _entries!(x.values, class)
        resize!(entries, length(entries) + 1)
    end
    return ret
end

const PartiallySortedDictOrIndices = Union{PartiallySortedDictionary, PartiallySortedIndices}

function Dictionaries.gettokenvalue(x::PartiallySortedDictOrIndices, (_, idx))
    return @inbounds x.values[idx]
end
function Dictionaries.settokenvalue!(
    x::PartiallySortedDictionary{K, V},
    (_, idx),
    value::V,
) where {K, V}
    return @inbounds x.values[idx] = value
end
function Dictionaries.deletetoken!(x::PartiallySortedIndices, (keyindex, (class, i)))
    ht = x.indices
    Base._delete!(ht, keyindex)
    deleteat!(x.values, (class, i))
    classes = x.values.classes
    class_in(classes)(class) || return x
    entries = @inbounds classes[class]
    for i in i:lastindex(entries)
        key = @inbounds entries[i]
        @inbounds ht[key] = (class, i)
    end
    return x
end
function Dictionaries.deletetoken!(x::PartiallySortedDictionary, (keyindex, idx))
    deletetoken!(keys(x), (keyindex, idx))
    deleteat!(x.values, idx)
    return x
end

function Base.similar(x::PartiallySortedIndices, ::Type{V}) where {V}
    return PartiallySortedDictionary(x, similar(x.values, V))
end
function Base.copy(x::PartiallySortedIndices)
    return PartiallySortedIndices(copy(x.indices), copy(x.values), x.get_class)
end
function Base.empty!(x::PartiallySortedDictOrIndices)
    empty!(x.indices)
    empty!(x.values)
    return x
end