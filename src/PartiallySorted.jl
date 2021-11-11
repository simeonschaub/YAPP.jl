struct PartiallySorted{T, A <: AbstractVector{<:AbstractVector{T}}, F}
    classes::A
end

Base.HasLength(::PartiallySorted) = Base.SizeUnknown()
Base.eltype(::Type{<:PartiallySorted{T}}) where {T} = T
Base.keytype(::Type{<:PartiallySorted}) = NTuple{2, Int}
Base.@propagate_inbounds function Base.getindex(x::PartiallySorted, (class, i)::NTuple{2, Int})
    return x.classes[class][i]
end
Base.@propagate_inbounds function Base.setindex!(x::PartiallySorted, e, (class, i)::NTuple{2, Int})
    x.classes[class][i] = e
    return x
end
function Base.push!(x::PartiallySorted, e, class::Int)
    classes = x.classes
    if class > lastindex(classes)
        resize!(classes, length(classes) + class - lastindex(classes))
    end
    if isassigned(classes, class)
        entries = classes[class]
    else
        entries = eltype(classes)()
        classes[class] = entries
    end
    push!(entries, e)
    return x
end
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
    class′ = findnext(class_in(classes), eachindex(classes), class)
    class′ === nothing &&
        error("No index after $((class, i)) in $(typeof(x))")
    return class′, firstindex(classes[class′])
end
function Base.prevind(x::PartiallySorted, (class, i)::NTuple{2, Int})
    classes = x.classes
    i > firstindex(classes[class]) && return class, i - 1
    class′ = findprev(class_in(classes), eachindex(classes), class)
    class′ === nothing &&
        error("No index before $((class, i)) in $(typeof(x))")
    return class′, lastindex(classes[class′])
end

Base.iterate(x::PartiallySorted, state=firstindex(x)) = state > lastindex(x) ? nothing : (x[state], nextind(x, state))
    

struct EachIndex{T}
    s::T
end
Base.keys(s::PartiallySorted) = EachIndex(s)

Base.length(e::EachIndex) = length(e.s)
Base.first(e::EachIndex) = firstindex(e.s)
Base.last(e::EachIndex) = lastindex(e.s)
Base.iterate(e::EachIndex, state=firstindex(e.s)) = state > lastindex(e.s) ? nothing : (state, nextind(e.s, state))
Base.eltype(::Type{EachIndex{T}}) where {T} = keytype(T)