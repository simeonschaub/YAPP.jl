struct Monomial{E} <: AbstractMonomial
    exponents::E
end
MultivariatePolynomials.coefficient(::Monomial) = true
MultivariatePolynomials.exponents(m::Monomial) = m.exponents
MultivariatePolynomials.termtype(::M, ::Type{C}) where {M <: Monomial, C} = Term{C, M}
MultivariatePolynomials.term(c, m::Monomial) = Term(c, m)
function Base.:(==)(m1::Monomial, m2::Monomial)
    e1, e2 = exponents.((m1, m2))
    return all(minimum(firstindex, (e1, e2)):maximum(lastindex, (e1, e2))) do i
        get(e1, i, zero(eltype(e1))) == get(e2, i, zero(eltype(e2)))
    end
end
function Base.convert(::Type{Monomial{NTuple{N, T}}}, m::Monomial) where {N, T}
    e = exponents(m)
    lastindex(e) <= N || error("Cannot convert $(length(e))-element monomial to NTuple{$N, $T}.")
    return Monomial(ntuple(i -> T(get(e, i, zero(T)))::T, N))
end
function Base.convert(::Type{Monomial{A}}, m::Monomial) where {T, A <: AbstractVector{T}}
    e = exponents(m)
    return Monomial(convert(A, T[get(e, i, zero(T)) for i in 1:lastindex(e)]))
end

const hash_monomial_seed = UInt === UInt64 ? 0x2bf03c38d5434740 : 0x3d0a6f7c
function hash(m::Monomial, h::UInt)
    A = exponents(m)
    h += hash_monomial_seed

    # For short arrays, it's not worth doing anything complicated
    if length(A) < 4096
        for (i, x) in Iterators.reverse(pairs(A))
            if !iszero(x)
                h = hash(i => x, h)
            end
        end
        return h
    end

    # Goal: Hash approximately log(N) entries with a higher density of hashed elements
    # weighted towards the end and special consideration for repeated values. Colliding
    # hashes will often subsequently be compared by equality -- and equality between arrays
    # works elementwise forwards and is short-circuiting. This means that a collision
    # between arrays that differ by elements at the beginning is cheaper than one where the
    # difference is towards the end. Furthermore, choosing `log(N)` arbitrary entries from a
    # sparse array will likely only choose the same element repeatedly (zero in this case).

    # To achieve this, we work backwards, starting by hashing the last element of the
    # array. After hashing each element, we skip `fibskip` elements, where `fibskip`
    # is pulled from the Fibonacci sequence -- Fibonacci was chosen as a simple
    # ~O(log(N)) algorithm that ensures we don't hit a common divisor of a dimension
    # and only end up hashing one slice of the array (as might happen with powers of
    # two). Finally, we find the next distinct value from the one we just hashed.

    # This is a little tricky since skipping an integer number of values inherently works
    # with linear indices, but `findprev` uses `keys`. Hoist out the conversion "maps":
    ks = keys(A)
    key_to_linear = LinearIndices(ks) # Index into this map to compute the linear index
    linear_to_key = vec(ks)           # And vice-versa

    # Start at the last index
    keyidx = last(ks)
    linidx = key_to_linear[keyidx]
    fibskip = prevfibskip = oneunit(linidx)
    first_linear = first(LinearIndices(linear_to_key))
    n = 0
    while true
        n += 1
        # Hash the element
        elt = A[keyidx]
        if !iszero(elt)
            h = hash(keyidx=>elt, h)
        end

        # Skip backwards a Fibonacci number of indices -- this is a linear index operation
        linidx = key_to_linear[keyidx]
        linidx < fibskip + first_linear && break
        linidx -= fibskip
        keyidx = linear_to_key[linidx]

        # Only increase the Fibonacci skip once every N iterations. This was chosen
        # to be big enough that all elements of small arrays get hashed while
        # obscenely large arrays are still tractable. With a choice of N=4096, an
        # entirely-distinct 8000-element array will have ~75% of its elements hashed,
        # with every other element hashed in the first half of the array. At the same
        # time, hashing a `typemax(Int64)`-length Float64 range takes about a second.
        if rem(n, 4096) == 0
            fibskip, prevfibskip = fibskip + prevfibskip, fibskip
        end

        if fibskip > 1
            # Find a key index with a value distinct from `elt` -- might be `keyidx` itself
            keyidx = findprev(!isequal(elt), A, keyidx)
            keyidx === nothing && break
        end
    end

    return h
end

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
MultivariatePolynomials.monomialtype(::P) where {P <: Polynomial} = monomialtype(P)

function Base.copy(p::Polynomial{C}, ::Type{C′}=C) where {C, C′}
    coeffs = p.coeffs
    coeffs′ = PartiallySortedDictionary(copy(keys(coeffs)), copy(coeffs.values, C′))
    return Polynomial(coeffs′)
end

const PolynomialLike = Union{Polynomial, TermLike}

function promote_collection_type(::Type{<:NTuple{N, T1}}, ::Type{NTuple{M, T2}}) where {N, M, T1, T2}
    return NTuple{max(N, M), promote_type(T1, T2)}
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

function +ₘ(t1::NTuple{N, T1}, t2::NTuple{M, T2}) where {N, M, T1, T2}
    T = promote_type(t1, t2)
    return ntuple(i -> convert(T, get(t1, i, false) + get(t1, i, false)), max(N, M))
end
+ₘ(t1::NTuple{N, T1}, t2::Tuple...) where {N, T1} = Base.afoldl(+ₘ, t1, t2)
function +ₘ(c1, cs...)
    return Base.promote_eltype(c1, cs...)[
        +(get(c1, i, false), get.(cs, i, false)...)
        for i in minimum(firstindex, (c1, cs...)):maximum(lastindex, (c1, cs...))
    ]
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
function BangBang.add!!(p1::Polynomial{C, M}, p2::PolynomialLike) where {C, M}
    C′ = promote_type(C, coefficienttype(p2))
    M′ = promote_monomial_type(p1, p2)
    if M′ <: M && C′ <: C
        return add!(p1, p2)
    else
        return p1 + p2
    end
end

function Base.:+(p1::Polynomial, ps::PolynomialLike...)
    C′ = promote_coefficient_type(p1, ps...)
    M′ = promote_monomial_type(p1, ps...)
    if M′ <: monomialtype(p1)
        p1′ = copy(p1, C′)
        return foldl(add!, ps; init=p1′)
    else
        return foldl(add!, (p1, ps...); init=Polynomial{C′, M′}())
    end
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

Base.:*(m1::Monomial, ms::Monomial...) = Monomial(.+ₘ(exponents(m1), exponents.(ms)...))
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