struct PartiallySortedIndices{T, P <: PartiallySorted{T}, F} <: AbstractIndices{T}
    indices::Dict{T, NTuple{2, Int}}
    values::P
    get_class::F # T -> Int
end
PartiallySortedIndices{T}(f) where {T} = PartiallySortedIndices(Dict{T, NTuple{2, Int}}(), PartiallySorted{T}(), f)

Base.iterate(x::PartiallySortedIndices, state...) = iterate(x.values, state...)
function Base.iterate(x::ReverseIndices{<:Any, <:PartiallySortedIndices}, state...)
    return iterate(Iterators.reverse(x.inds.values), state...)
end
Base.in(i::T, x::PartiallySortedIndices{T}) where {T} = haskey(x.indices, i)
Base.length(x::PartiallySortedIndices) = length(x.indices)

Dictionaries.isinsertable(::PartiallySortedIndices) = true
#function _insert!(x::PartiallySortedIndices, i)
#    values = x.values
#    class = x.get_class(i)
#    idx = _push!(values, i, class)
#    x.indices[i] = idx
#    return idx
#end
#Base.insert!(x::PartiallySortedIndices, i) = (_insert!(x, i); x)
#function _delete!(x::PartiallySortedIndices, i)
#    idx = pop!(x.indices, i)
#    deleteat!(x.values, idx)
#    return idx
#end
#Base.delete!(x::PartiallySortedIndices, i) = (_delete!(x, i); x)

struct PartiallySortedDictionary{
    K, V, I <: PartiallySortedIndices{K}, P <: PartiallySorted{V},
} <: AbstractDictionary{K, V}
    indices::I
    values::P
end
PartiallySortedDictionary{K, V}(f) where {K, V} = PartiallySortedDictionary(PartiallySortedIndices{K}(f), PartiallySorted{V}())

Base.@propagate_inbounds function Base.getindex(x::PartiallySortedDictionary{K}, k::K) where {K}
    idx = x.indices.indices[k]
    return x.values[idx]
end
Base.isassigned(x::PartiallySortedDictionary{K}, k::K) where {K} = k in keys(x)
Base.keys(x::PartiallySortedDictionary) = x.indices

Dictionaries.issettable(::PartiallySortedDictionary) = true
function Base.setindex!(x::PartiallySortedDictionary{K, V}, v::V, k::K) where {K, V}
    idx = x.indices.indices[k]
    x.values[idx] = v
    return x
end

Dictionaries.isinsertable(::PartiallySortedDictionary) = true
#function Base.insert!(x::PartiallySortedDictionary{K}, k::K) where {K}
#    class, _ = _insert!(x, k)
#    push!(x.values, k, class)
#    return x
#end
#function Base.delete!(x::PartiallySortedDictionary{K}, k::K)
#    idx = _delete!(x, k)
#    deleteat!(x.values, idx)
#    return x
#end

function Dictionaries.gettoken!(x::PartiallySortedIndices{T}, key::T) where {T}
    ht = x.indices
    # Ideally, to improve performance for the case that requires
    # resizing, we should use something like `ht_keyindex` while
    # keeping computed hash value and then do something like
    # `ht_keyindex2!` if `f` returns non-`nothing`.
    keyindex = Base.ht_keyindex2!(ht, key)

    age0 = ht.age
    hadindex = keyindex > 0
    if !hadindex
        keys = x.values
        class = x.get_class(key)

        idx = _push!(keys, key, class)
        @inbounds Base._setindex!(ht, idx, key, -keyindex)
    else
        idx = @inbounds ht.vals[keyindex]
    end
    if ht.age != age0
        keyindex = Base.ht_keyindex2!(ht, key)
    end
    return hadindex, (keyindex, idx)
end
function Dictionaries.gettoken!(x::PartiallySortedDictionary{K}, key::K) where {K}
    ret = gettoken!(keys(x), key)
    hadindex, (_, (class, _)) = ret
    if !hadindex
        classes = x.values.classes
        if class > lastindex(classes)
            resize!(classes, length(classes) + class - lastindex(classes))
        end
        if !isassigned(classes, class)
            entries = eltype(classes)()
            classes[class] = entries
        else
            entries = classes[class]
        end
        resize!(entries, length(entries) + 1)
    end
    return ret
end

const PartiallySortedDictOrIndices = Union{PartiallySortedDictionary, PartiallySortedIndices}

function Dictionaries.gettokenvalue(x::PartiallySortedDictOrIndices, (_, idx))
    return @inbounds x.values[idx]
end
function Dictionaries.settokenvalue!(x::PartiallySortedDictionary{K, V}, (_, idx), value::V) where {K, V}
    return @inbounds x.values[idx] = value
end

function Dictionaries.deletetoken!(x::PartiallySortedIndices, (keyindex, idx))
    ht = x.indices
    Base._delete!(ht, keyindex)
    deleteat!(x.values, idx)
    return x
end
function Dictionaries.deletetoken!(x::PartiallySortedDictionary, (keyindex, idx))
    deletetoken!(keys(x), (keyindex, idx))
    deleteat!(x.values, idx)
    return x
end