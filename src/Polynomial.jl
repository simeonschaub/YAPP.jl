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

const TermLike = Union{Term, Monomial}
MultivariatePolynomials.variable(::TermLike) = :x
MultivariatePolynomials.variables(t::TermLike) = (Variable(i) for i in 1:length(exponents(t)))
MultivariatePolynomials.powers(t::TermLike) = ((Variable(i), e) for (i, e) in enumerate(exponents(t)))

struct Polynomial{C, M<:AbstractMonomial, D <: PartiallySortedDictionary{M, C}} <: AbstractPolynomial{C}
    coeffs::D
end
Polynomial{C}() where {C} = Polynomial(PartiallySortedDictionary{Monomial{Vector{Int}}, C}(x -> count(!iszero, exponents(x)) + 1))
MultivariatePolynomials.terms(p::Polynomial) = (Term(c, m) for (m, c) in pairs(Iterators.reverse(p.coeffs)))
function Base.copy(p::Polynomial{C}, ::Type{C′}=C) where {C, C′}
    coeffs = p.coeffs
    coeffs′ = PartiallySortedDictionary(copy(keys(coeffs)), copy(coeffs.values, C′))
    return Polynomial(coeffs′)
end

const PolynomialLike = Union{Polynomial, TermLike}

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
    C′ = promote_type(coefficienttype(p1), coefficienttype.(ps)...)
    return foldl(add!, (p1, ps...); init=Polynomial{C′}())
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
    C′ = promote_type(coefficienttype(p1), coefficienttype.(ps)...)
    p = Polynomial{C′}()
    for terms in Iterators.product(terms.((p1, ps...))...)
        add!(p, *(terms...))
    end
    return p
end