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
MultivariatePolynomials.variable(t::TermLike) = :x
MultivariatePolynomials.variables(t::TermLike) = (Variable(i) for i in 1:length(exponents(t)))
MultivariatePolynomials.powers(t::TermLike) = ((Variable(i), e) for (i, e) in enumerate(exponents(t)))

struct Polynomial{C, M<:AbstractMonomial, D <: PartiallySortedDictionary{M, C}} <: AbstractPolynomial{C}
    coeffs::D
end
Polynomial{C}() where {C} = Polynomial(PartiallySortedDictionary{Monomial{Vector{Int}}, C}(x -> count(!iszero, exponents(x)) + 1))
MultivariatePolynomials.terms(p::Polynomial) = (Term(c, m) for (m, c) in pairs(Iterators.reverse(p.coeffs)))

function BangBang.add!!(p::Polynomial, t::TermLike)
    coeffs = p.coeffs
    hadindex, token = gettoken!(coeffs, monomial(t))
    if hadindex
        settokenvalue!(coeffs, token, gettokenvalue(coeffs, token) + coefficient(t))
    else
        settokenvalue!(coeffs, token, coefficient(t))
    end
    return p
end
function BangBang.add!!(p1::Polynomial, p2::Polynomial)
    mergewith!(+, p1.coeffs, p2.coeffs)
    return p1
end