module YAPP

using Dictionaries, MultivariatePolynomials, BangBang, OffsetArrays

include("PartiallySorted.jl")
include("PartiallySortedDictionary.jl")

#BangBang.possible(::typeof(BangBang._setindex!), ::C, ::V, ::K) where {V0, K0, C <: AbstractDictionary{V0, K0}, V, K} =
#    BangBang.implements(setindex!, C) &&
#    promote_type(K0, K) <: K0 &&
#    promote_type(V0, V) <: V0
#
##BangBang.possible(::typeof(BangBang._setindex!), ::C)  where {C <: AbstractDictionary} = true
#BangBang.implements(::typeof(BangBang.setindex!), ::Type{<:AbstractDictionary}) = true
#
#BangBang.pure(::typeof(insert!)) = _insert
#function _insert(dict::AbstractDictionary{K0, V0}, key::K, value::V) where {K0, V0, K, V}
#    new_inds = similar(keys(dict), promote_type(K0, K))
#    new_dict = similar(Dictionary(new_inds, dict), promote_type(V0, V))
#    return insert!(new_dict, key, value)
#end
#BangBang.possible(::typeof(insert!), ::C, ::V, ::K) where {V0, K0, C <: AbstractDictionary{V0, K0}, V, K} =
#    BangBang.implements(setindex!, C) &&
#    promote_type(K0, K) <: K0 &&
#    promote_type(V0, V) <: V0
#insert!!(dict, key, value) = may(insert!, dict, key, value)
#
#
##function BangBang.generic_modify!!(f, dict::AbstractDictionary, key)
##    if haskey(dict, key)
##        val = f(Some(dict[key]))
##    else
##        val = f(nothing)
##    end
##    if val === nothing
##        dict′ = delete!!(dict, key)
##    else
##        dict′ = insert!(dict, something(val), key)
##    end
##    return (dict′, val)
##end
#
#function modify!!(f, h::AbstractDictionary{K1}, key0::K2) where {K1, K2}
#    promote_type(K1, K2) <: K1 || return generic_modify!!(f, h, key0)
#    key = convert(K1, key0)
#    isequal(key, key0) || return generic_modify!!(f, h, key0)
#
#    haskey, token = gettoken!(h, key)
#    vnew = f(haskey ? Some(gettokenvalue(token)) : nothing)
#
#end



struct Monomial{E} <: AbstractMonomial
    exponents::E
end

struct Term{C, M <: Monomial} <: AbstractTerm{C}
    coeff::C
    monomial::M
end

struct Polynomial{C, M<:AbstractMonomial, D <: AbstractDictionary{M, C}} <: AbstractPolynomial{C}
    coeffs::D
end

#function BangBang.add!!(p1::MvPolynomial, p2::MvPolynomial)
#    return mergewi
#end




end
