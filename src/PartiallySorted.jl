struct PartiallySorted{T, A <: AbstractVector{<:AbstractVector{T}}}
    classes::A
end
PartiallySorted{T}() where {T} = PartiallySorted(Vector{Vector{T}}())

Base.IteratorSize(::PartiallySorted) = Base.SizeUnknown()
Base.eltype(::Type{<:PartiallySorted{T}}) where {T} = T
Base.keytype(::Type{<:PartiallySorted}) = NTuple{2, Int}
Base.@propagate_inbounds function Base.getindex(x::PartiallySorted, (class, i)::NTuple{2, Int})
    return x.classes[class][i]
end
Base.@propagate_inbounds function Base.setindex!(x::PartiallySorted, e, (class, i)::NTuple{2, Int})
    x.classes[class][i] = e
    return x
end
function _entries!(x::PartiallySorted, class::Int)
    classes = x.classes
    if class > lastindex(classes)
        resize!(classes, length(classes) + class - lastindex(classes))
    end
    if isassigned(classes, class)
        entries = classes[class]
    else
        entries = similar(eltype(classes), 0)
        classes[class] = entries
    end
    return entries
end
function _push!(x::PartiallySorted, e, class::Int)
    entries = _entries!(x, class)
    push!(entries, e)
    return class, lastindex(entries)
end
Base.push!(x::PartiallySorted, e, class::Int) = (_push!(x, e, class); x)
Base.deleteat!(x::PartiallySorted, (class, i)::Tuple{Int, Any}) = deleteat!(x.classes[class], i)

class_in(classes) = class -> isassigned(classes, class) && !isempty(classes[class])
function Base.firstindex(x::PartiallySorted)
    classes = x.classes
    class = findfirst(class_in(classes), eachindex(classes))
    class === nothing && return lastindex(classes) + 1, 0
    return class, firstindex(classes[class])
end
function Base.lastindex(x::PartiallySorted)
    classes = x.classes
    class = findlast(class_in(classes), eachindex(classes))
    class === nothing && return firstindex(classes) - 1, 0
    return class, lastindex(classes[class])
end
function Base.nextind(x::PartiallySorted, (class, i)::NTuple{2, Int})
    classes = x.classes
    i < lastindex(classes[class]) && return class, i + 1
    class′ = findnext(class_in(classes), eachindex(classes), class + 1)
    class′ === nothing && return lastindex(classes) + 1, 0
    return class′, firstindex(classes[class′])
end
function Base.prevind(x::PartiallySorted, (class, i)::NTuple{2, Int})
    classes = x.classes
    i > firstindex(classes[class]) && return class, i - 1
    class′ = findprev(class_in(classes), eachindex(classes), class - 1)
    class′ === nothing && return firstindex(classes) - 1, 0
    return class′, lastindex(classes[class′])
end

function Base.iterate(x::PartiallySorted, (idx, last_idx)=(firstindex(x), lastindex(x)))
    return idx > last_idx ? nothing : (x[idx], (nextind(x, idx), last_idx))
end
function Base.iterate(
    x::Iterators.Reverse{<:PartiallySorted},
    (idx, first_idx)=(lastindex(x.itr), firstindex(x.itr)),
)
    return idx < first_idx ? nothing : (x.itr[idx], (prevind(x.itr, idx), first_idx))
end


function Base.similar(x::PartiallySorted, ::Type{T}) where {T}
    classes = x.classes
    classes′ = similar(classes, Vector{T})
    @inbounds for class in eachindex(classes)
        isassigned(classes, class) || continue
        classes′[class] = Vector{T}(undef, length(classes[class]))
    end
    return PartiallySorted(classes′)
end

function Base.copy(x::PartiallySorted{T}, ::Type{T′}=T) where {T, T′}
    classes = x.classes
    classes′ = similar(classes, Vector{T′})
    @inbounds for class in eachindex(classes)
        isassigned(classes, class) || continue
        entries = classes[class]
        classes′[class] = copyto!(similar(entries, T′), entries)
    end
    return PartiallySorted(classes′)
end

struct EachIndex{T}
    s::T
end
Base.keys(s::PartiallySorted) = EachIndex(s)

Base.IteratorSize(e::EachIndex) = Base.IteratorSize(e.s)
Base.length(e::EachIndex) = length(e.s)
Base.first(e::EachIndex) = firstindex(e.s)
Base.last(e::EachIndex) = lastindex(e.s)
Base.iterate(e::EachIndex, state=firstindex(e.s)) = state > lastindex(e.s) ? nothing : (state, nextind(e.s, state))
Base.eltype(::Type{EachIndex{T}}) where {T} = keytype(T)