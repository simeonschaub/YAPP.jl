struct Monomial{E} <: AbstractMonomial
    exponents::E
end
MultivariatePolynomials.coefficient(::Monomial) = true
MultivariatePolynomials.exponents(m::Monomial) = m.exponents
MultivariatePolynomials.termtype(::M, ::Type{C}) where {M <: Monomial, C} = Term{C, M}
MultivariatePolynomials.term(c, m::Monomial) = Term(c, m)
Base.:(==)(m1::Monomial, m2::Monomial) = m1.exponents == m2.exponents

struct Variable <: AbstractVariable
    i::Int
end
MultivariatePolynomials.name_base_indices(x::Variable) = :x, (x.i,)

struct Term{C, M <: Monomial} <: AbstractTerm{C}
    coeff::C
    monomial::M
end
MultivariatePolynomials.coefficient(t::Term) = t.coeff
MultivariatePolynomials.exponents(t::Term) = exponents(t.monomial)
MultivariatePolynomials.monomialtype(::Type{Term{C, M}}) where {C, M} = M

const TermLike = Union{Term, Monomial}
MultivariatePolynomials.variable(::TermLike) = :x
MultivariatePolynomials.variables(t::TermLike) = (Variable(i) for i in 1:length(exponents(t)))
MultivariatePolynomials.powers(t::TermLike) = ((Variable(i), e) for (i, e) in enumerate(exponents(t)))

struct Polynomial{C, M<:AbstractMonomial, D <: PartiallySortedDictionary{M, C}} <: AbstractPolynomial{C}
    coeffs::D
end
get_class(m::Monomial) = count(!iszero, exponents(m)) + 1
Polynomial{C, M}() where {C, M<:AbstractMonomial} = Polynomial(PartiallySortedDictionary{M, C}(get_class))
Polynomial{C}() where {C} = Polynomial{C, Monomial{Vector{Int}}}()

MultivariatePolynomials.terms(p::Polynomial) = (Term(c, m) for (m, c) in pairs(Iterators.reverse(p.coeffs)))
MultivariatePolynomials.monomialtype(::Type{<:Polynomial{<:Any, M}}) where {M} = M

function Base.copy(p::Polynomial{C}, ::Type{C′}=C) where {C, C′}
    coeffs = p.coeffs
    coeffs′ = PartiallySortedDictionary(copy(keys(coeffs)), copy(coeffs.values, C′))
    return Polynomial(coeffs′)
end

const PolynomialLike = Union{Polynomial, TermLike}

function promote_collection_type(::Type{<:NTuple{N, T1}}, ::Type{NTuple{N, T2}}) where {N, T1, T2}
    return NTuple{N, promote_type(T1, T2)}
end
promote_collection_type(c1, c2) = Vector{promote_type(eltype(c1), eltype(c2))}
_exponent_type(::Type{Monomial{E}}) where {E} = E
function promote_monomial_type(p1::PolynomialLike, ps::PolynomialLike...)
    return Monomial{foldl(ps; init=_exponent_type(monomialtype(p1))) do T, p
        promote_collection_type(T, _exponent_type(monomialtype(p)))
    end}
end
function promote_coefficient_type(p1::PolynomialLike, ps::PolynomialLike...)
    return promote_type(coefficienttype(p1), coefficienttype.(ps)...)
end

function (p::PolynomialLike)(xs)
    return sum(terms(p)) do t
        coefficient(t) * prod(zip(xs, exponents(t))) do (x, e)
            x^e
        end
    end
end

function add!(p::Polynomial, t::TermLike)
    coeffs = p.coeffs
    hadindex, token = gettoken!(coeffs, monomial(t))
    if hadindex
        settokenvalue!(coeffs, token, gettokenvalue(coeffs, token) + coefficient(t))
    else
        settokenvalue!(coeffs, token, coefficient(t))
    end
    return p
end
add!(p1::Polynomial, p2::Polynomial) = (mergewith!(+, p1.coeffs, p2.coeffs); p1)
function BangBang.add!!(p1::Polynomial{C}, p2::PolynomialLike) where {C}
    C′ = promote_type(C, coefficienttype(p2))
    if !(C′ <: C)
        p1 = copy(p1, C′)
    end
    return add!(p1, p2)
end

function Base.:+(p1::Polynomial, ps::PolynomialLike...)
    p1′ = copy(p1, promote_type(coefficienttype(p1), coefficienttype.(ps)...))
    return foldl(add!, ps; init=p1′)
end
function Base.:+(p1::TermLike, ps::PolynomialLike...)
    C′ = promote_coefficient_type(p1, ps...)
    M′ = promote_monomial_type(p1, ps...)
    return foldl(add!, (p1, ps...); init=Polynomial{C′, M′}())
end

LinearAlgebra.rmul!(p::Polynomial, α::Number) = (map!(x -> x * α, p.coeffs); p)
function BangBang.rmul!!(p::Polynomial{C}, α::T) where {C, T <: Number}
    C′ = promote_type(C, T)
    if !(C′ <: C)
        p = copy(p, C′)
    end
    return rmul!(p, α)
end
function Base.:*(p::Polynomial{C}, α::T) where {C, T <: Number}
    C′ = promote_type(C, T)
    return rmul!(copy(p, C′), α)
end
function Base.:*(α::T, p::Polynomial{C}) where {C, T <: Number}
    C′ = promote_type(C, T)
    return rmul!(copy(p, C′), α)
end

Base.:*(m1::Monomial, ms::Monomial...) = Monomial(.+(exponents(m1), exponents.(ms)...))
function Base.:*(t1::TermLike, ts::TermLike...)
    return Term(*(coefficient(t1), coefficient.(ts)...), *(monomial(t1), monomial.(ts)...))
end
function Base.:*(p1::PolynomialLike, ps::PolynomialLike...)
    C′ = promote_coefficient_type(p1, ps...)
    M′ = promote_monomial_type(p1, ps...)
    p = Polynomial{C′, M′}()
    for terms in Iterators.product(terms.((p1, ps...))...)
        add!(p, *(terms...))
    end
    return p
end